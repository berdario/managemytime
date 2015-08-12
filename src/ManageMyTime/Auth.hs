{-# LANGUAGE OverloadedStrings #-}

module ManageMyTime.Auth where

import Prelude hiding (exp, lookup)
import GHC.Conc (atomically)
import System.IO.Unsafe (unsafePerformIO)
import Control.Arrow ((&&&))
import Control.Monad.IO.Class (liftIO)
import Crypto.Scrypt (Pass(..), EncryptedPass(..), verifyPass')
import Data.Aeson (ToJSON)
import Data.Text.Encoding (encodeUtf8)
import Data.Default (def)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.Persist.Sql (getBy, entityVal, Entity)
import Web.JWT (JWT, JSON, UnverifiedJWT, VerifiedJWT, JWTClaimsSet(..), Algorithm(..),
                secret, decodeAndVerifySignature, encodeSigned, IntDate, intDate,
                secondsSinceEpoch, stringOrURI, stringOrURIToText, claims)
import STMContainers.Map (Map, newIO, lookup, insert)
import ManageMyTime.Models (runDb, Unique(..), userPasswordHash, User(..))
import ManageMyTime.Types

data ReasonInvalid = TokenCorrupted | TokenCorrupted2 | TokenExpired | LoggedOut | UserNameChanged deriving (Show)

jwtSecret = secret "X5LjzvSUdP+OTPlhrFcE6lRwsy8FGTuuNMet0f/Kd1O3DZPbB78BC6t7HEgTFflveh0J834ofEkU"

sessions = unsafePerformIO newIO

decode :: Text -> Maybe (JWT VerifiedJWT)
decode = decodeAndVerifySignature jwtSecret

encode :: JWTClaimsSet -> JSON
encode = encodeSigned HS256 jwtSecret

now :: IO Integer
now = fmap round getPOSIXTime

newExpiration :: IO (Maybe IntDate)
newExpiration = fmap (intDate . fromIntegral . (+ 86401)) now
-- 86401 seconds ~= 1 Day

isExpired :: IntDate -> IO Bool
isExpired d = do
  now' <- now
  return $ ((secondsSinceEpoch d) < (fromIntegral now'))

newTkn :: Text -> IO (Either String JSON)
newTkn user = do
  expiration <- newExpiration
  liftIO $ sign (stringOrURI user) expiration

sign (Just user) (Just expiration) = do
  atomically $ insert tkn (stringOrURIToText user) sessions
  return $ Right tkn
  where
   tkn = encode $ def{sub=(Just user), exp=(Just expiration)}
sign Nothing     _                 = return $ Left "invalid username"
sign _           Nothing           = return $ Left "internal time error"


verify pass (Just u) = verifyPass' pass $ EncryptedPass $ userPasswordHash $ entityVal u
verify _ Nothing = False

login user pass = do
  mUser <- runDb $ getBy $ UniqueName user
  eitherTkn <- if (verify (Pass (encodeUtf8 pass)) mUser) then
                       (newTkn user)
                  else (return $ Left "login error")
  return eitherTkn

validate tkn = maybe (return $ Left TokenCorrupted) getUser $ decode tkn
  where
   getUser = (validateClaims tkn) . (sub &&& exp) . claims

queryUser :: Text -> IO (Either ReasonInvalid (Entity User))
queryUser name = do
   mUser <- runDb $ getBy $ UniqueName name
   return $ maybe (Left UserNameChanged) Right mUser

validateClaims _ (Nothing, _) = return $ Left TokenCorrupted2
validateClaims _ (_, Nothing) = return $ Left TokenCorrupted2
validateClaims tkn ((Just name), (Just expiration)) = do
   expired <- isExpired expiration
   if expired then (return $ Left TokenExpired) else (sessionCheck (stringOrURIToText name) tkn)

sessionCheck name tkn = do
  storedTkn <- atomically $ lookup name sessions
  let tknCheck = fmap (tkn ==) storedTkn
  if (tknCheck == (Just False)) then (return $ Left LoggedOut) else (queryUser name)
