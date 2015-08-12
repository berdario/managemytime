{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}

module ManageMyTime where

import GHC.TypeLits (Symbol)
import Control.Error.Util (note, noteT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT(..), hoistEither, left, right, bimapEitherT)
import Text.Read (readMaybe)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Calendar (Day, showGregorian)
import Database.Persist.Sql (Entity, entityKey, PersistEntity, PersistEntityBackend, SqlBackend(..))
import Network.Wai (Application)
import Servant (JSON, (:>), (:<|>)(..), Proxy(..), ServantErr(..), Capture, Headers, Header,
                ReqBody, QueryParam, Get, Post, Put, Delete, err403, err404, err409)
import qualified Servant
import Servant.API.ResponseHeaders (addHeader)
import Servant.Utils.Links (safeLink, MkLink, IsElem, HasLink)

import ManageMyTime.Models (get, insertUnique, fromSqlKey, toSqlKey, doMigrations, runDb,
                            Key, Item(..), Task(..), User(..), createUser)
import qualified ManageMyTime.Auth as Auth
import ManageMyTime.Types


type CRUD kty ty = Capture "id" kty :> Get '[JSON] ty
              :<|> ReqBody '[JSON] ty :>
                     Post '[JSON] (Headers '[Header "Location" (MkLink (Get '[JSON] ty))] kty)
              :<|> Capture "id" kty :> ReqBody '[JSON] ty :> Put '[JSON] ()
              :<|> Capture "id" kty :> Delete '[JSON] ()

type CRUDProfile = Get '[JSON] ClientUser
              :<|> ReqBody '[JSON] ClientUser :> Put '[JSON] ()
              :<|> Delete '[JSON] ()

type Auth = Header "Authorization" Text
type AuthRoute (nextRes :: Symbol) nextTy resTy = ReqBody '[JSON] Registration :>
       Post '[JSON] (Headers '[Header "Location" (MkLink (nextRes :> Get '[JSON] nextTy))] resTy)

type CRUDHours = Get '[JSON] Int
            :<|> ReqBody '[JSON] Int :>
                   Post '[JSON] (Headers '[Header "Location" (MkLink (Get '[JSON] Int))] ())
            :<|> ReqBody '[JSON] Int :> Put '[JSON] ()
            :<|> Delete '[JSON] ()

type GetItems = QueryParam "from" Day :> QueryParam "to" Day :> Get '[JSON] [Item]

type AuthenticatedAPI = Auth :> "task" :> CRUD (Key Task) ClientTask
                   :<|> Auth :> "item" :> CRUD (Key Item) ClientItem
                   :<|> Auth :> "preferred-hours" :> CRUDHours
                   :<|> Auth :> "profile" :> CRUDProfile
                   :<|> Auth :> "user" :> CRUD (Key User) UserWithPerm
                   :<|> Auth :> "tasks" :> Get '[JSON] ([ClientTask], [ClientTask])
                   :<|> Auth :> "items" :> GetItems
                   :<|> Auth :> "users" :> Get '[JSON] (Map UserKey UserWithPerm)
                   :<|> Auth :> "logout" :> Post '[JSON] ()

type TimeAPI = AuthenticatedAPI
          :<|> "registration" :> AuthRoute "profile" ClientUser ()
          :<|> "login" :> AuthRoute "items" [Item] Text

timeAPI :: Proxy TimeAPI
timeAPI = Proxy

apiLink :: (IsElem endpoint TimeAPI, HasLink endpoint) =>
            Proxy endpoint -> MkLink endpoint
apiLink = safeLink timeAPI
taskLink = apiLink (Proxy :: Proxy (Auth :> "task" :> Capture "id" (Key Task) :> Get '[JSON] ClientTask))
itemLink = apiLink (Proxy :: Proxy (Auth :> "item" :> Capture "id" (Key Item) :> Get '[JSON] ClientItem))
itemsLink = apiLink (Proxy :: Proxy (Auth :> "items" :> Get '[JSON] [Item]))
profileLink = apiLink (Proxy :: Proxy (Auth :> "profile" :> Get '[JSON] ClientUser))
userLink = apiLink (Proxy :: Proxy (Auth :> "user" :> Capture "id" (Key User) :> Get '[JSON] UserWithPerm))

type AppM = EitherT Servant.ServantErr IO

decorateAuthError tknErr = err403{errBody=encodeUtf8 $ TL.pack $ show $ tknErr}

liftValidate tkn = bimapEitherT decorateAuthError id  $ EitherT $ Auth.validate tkn

withUserFrom :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend) => (Entity User -> (Key val) -> val -> AppM b) -> Text -> (Key val) -> AppM b
withUserFrom f tkn key = do
  usr <- liftValidate tkn
  mTask <- runDb $ get key
  case mTask of
    Nothing -> left err404
    (Just task) -> f usr key task


withTaskPerms :: (Entity User -> (Key Task) -> Task -> AppM b) -> Text -> (Key Task) -> AppM b
withTaskPerms f = withUserFrom f'
  where f' = (\usr key task -> if (entityKey usr == taskAuthorId task) then (f usr key task) else left err403)

getTask :: Text -> (Key Task) -> AppM ClientTask
getTask = withTaskPerms (\_ _ t -> right $ taskName t)
newTask tkn taskname = do
  usr <- liftValidate tkn
  mNewKey <- runDb $ insertUnique $ Task taskname $ toSqlKey 1
  hoistEither $ case mNewKey of
    (Just key) -> Right $ addHeader (taskLink key) key
    Nothing -> Left err409
updateTask = undefined
deleteTask = undefined
getItem = undefined
newItem = undefined
updateItem = undefined
deleteItem = undefined
getPreferredHours = undefined
newPreferredHours = undefined
updatePreferredHours = undefined
deletePreferredHours = undefined
getProfile = undefined
updateProfile = undefined
deleteProfile = undefined
getUser = undefined
newUser = undefined
updateUser = undefined
deleteUser = undefined
getTasks = undefined
getItems = undefined
getUsers = undefined
logout = undefined
register Registration{..} = do
  user <- liftIO $ createUser newUserName password
  mNewKey <- runDb $ insertUnique user
  hoistEither $ case mNewKey of
    (Just key) -> Right $ addHeader profileLink ()
    Nothing -> Left err409
login Registration{..} = do
  tkn <- bimapEitherT decorateAuthError id  $ EitherT $ Auth.login newUserName password
  right $ addHeader itemsLink tkn


server :: Servant.Server TimeAPI
server = authApi
    :<|> register
    :<|> login

crudAccessDenied e = const (left e)
                :<|> const (left e)
                :<|> const (const (left e))
                :<|> const (left e)

authApi =  maybe (crudAccessDenied err403) taskCrud
     :<|> (\x -> itemCrud )
     :<|> (\x -> preferredHoursCrud )
     :<|> (\x -> profileCrud )
     :<|> (\x -> userCrud )
     :<|> (\x -> getTasks )
     :<|> (\x -> getItems )
     :<|> (\x -> getUsers )
     :<|> (\x -> left err404 --logout
     )

taskCrud tkn =  getTask tkn :<|>  newTask tkn :<|> updateTask :<|> deleteTask
itemCrud = getItem :<|> newItem :<|> updateItem :<|> deleteItem
preferredHoursCrud = getPreferredHours :<|> newPreferredHours :<|> updatePreferredHours :<|> deletePreferredHours
profileCrud = getProfile :<|> updateProfile :<|> deleteProfile
userCrud = getUser :<|> newUser :<|> updateUser :<|> deleteUser

app :: Application
app = Servant.serve timeAPI server
