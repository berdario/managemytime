{-# LANGUAGE OverloadedStrings #-}

module ManageMyTime.Auth where

import           Control.Arrow              ((&&&))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (asks)
import           Control.Monad.Trans.Except (throwE)
import           Crypto.Scrypt              (EncryptedPass (..), Pass (..),
                                             verifyPass')
import           Data.Aeson                 (ToJSON)
import           Data.Default               (def)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Database.Persist.Sql       (Entity, entityVal, getBy)
import           GHC.Conc                   (atomically)
import           ManageMyTime.Models        (AppEnv (..), AppM, Token,
                                             Unique (..), User (..), runDb,
                                             userPasswordHash)
import           ManageMyTime.Types
import           Prelude                    hiding (exp, lookup)
import           STMContainers.Map          (Map, delete, insert, lookup, newIO)
import           System.IO.Unsafe           (unsafePerformIO)
import           Web.JWT                    (Algorithm (..), IntDate, JSON, JWT,
                                             JWTClaimsSet (..), StringOrURI,
                                             UnverifiedJWT, VerifiedJWT, claims,
                                             decodeAndVerifySignature,
                                             encodeSigned, intDate,
                                             secondsSinceEpoch, secret,
                                             stringOrURI, stringOrURIToText)


data ReasonInvalid = TokenCorrupted | TokenCorrupted2 | TokenExpired | LoggedOut | UserNameChanged deriving (Show)

jwtSecret = secret "X5LjzvSUdP+OTPlhrFcE6lRwsy8FGTuuNMet0f/Kd1O3DZPbB78BC6t7HEgTFflveh0J834ofEkU"

decode :: Token -> Maybe (JWT VerifiedJWT)
decode = decodeAndVerifySignature jwtSecret

encode :: JWTClaimsSet -> JSON
encode = encodeSigned HS256 jwtSecret

now :: IO Integer
now = fmap round getPOSIXTime

newExpiration :: IO (Maybe IntDate)
newExpiration = fmap (intDate . fromIntegral . (+ 86401)) now
-- 86401 seconds ~= 1 Day

isExpired :: IntDate -> AppM Bool
isExpired d = do
  now' <- liftIO now
  return $ ((secondsSinceEpoch d) < (fromIntegral now'))

newTkn :: Text -> AppM (Either String JSON)
newTkn user = do
  expiration <- liftIO newExpiration
  sign (stringOrURI user) expiration

sign :: Maybe StringOrURI -> Maybe IntDate -> AppM (Either String JSON)
sign (Just user) (Just expiration) = do
  sessions <- asks getSessions
  liftIO $ atomically $ insert tkn (stringOrURIToText user) sessions
  return $ Right tkn
  where
   tkn = encode $ def{sub=(Just user), exp=(Just expiration)}
sign Nothing     _                 = return $ Left "invalid username"
sign _           Nothing           = return $ Left "internal time error"


verify :: Pass -> Maybe (Entity User) -> Bool
-- This is a timing oracle that reveals which users exist to attackers. If this was a real application,
-- to close this hole without opening up ourselves to DOS, we'd need to have some fail2ban setup
verify pass (Just u) = verifyPass' pass $ EncryptedPass $ userPasswordHash $ entityVal u
verify _ Nothing = False

login :: Text -> Text -> AppM (Either String Token)
login user pass = do
  mUser <- runDb $ getBy $ UniqueName user
  eitherTkn <- if (verify (Pass (encodeUtf8 pass)) mUser) then
                       (newTkn user)
                  else (return $ Left "login error")
  return eitherTkn

type EUser = (Either ReasonInvalid (Entity User))

validate :: Token -> AppM EUser
validate tkn = maybe (return $ Left TokenCorrupted) getUser $ decode tkn
  where
   getUser = (validateClaims tkn) . (sub &&& exp) . claims

queryUser :: Text -> AppM EUser
queryUser name = do
   mUser <- runDb $ getBy $ UniqueName name
   return $ maybe (Left UserNameChanged) Right mUser

validateClaims :: Token -> (Maybe StringOrURI, Maybe IntDate) -> AppM EUser
validateClaims _ (Nothing, _) = return $ Left TokenCorrupted2
validateClaims _ (_, Nothing) = return $ Left TokenCorrupted2
validateClaims tkn ((Just name), (Just expiration)) = do
   expired <- isExpired expiration
   if expired then (return $ Left TokenExpired) else (sessionCheck (stringOrURIToText name) tkn)

sessionCheck :: Text -> Token -> AppM EUser
sessionCheck name tkn = do
  sessions <- asks getSessions
  storedTkn <- liftIO $ atomically $ lookup name sessions
  let tknCheck = fmap (tkn ==) storedTkn
  if (tknCheck == (Just False)) then (return $ Left LoggedOut) else (queryUser name)

logout :: Text -> AppM ()
logout name = do
  sessions <- asks getSessions
  -- the token is rejected only when a newer one is stored, thus deleting it
  -- wouldn't force a logout
  liftIO $ atomically $ insert "" name sessions
