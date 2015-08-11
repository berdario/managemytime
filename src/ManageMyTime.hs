{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module ManageMyTime where

import GHC.TypeLits (Symbol)
import Control.Error.Util (note)
import Control.Monad.Trans.Either (EitherT, hoistEither)
import Text.Read (readMaybe)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Calendar (Day, showGregorian)
import Network.Wai (Application)
import Servant (JSON, (:>), (:<|>)(..), Capture, Headers, Header, ReqBody,
                QueryParam, Get, Post, Put, Delete, err404, err409)
import qualified Servant
import Servant.API.ResponseHeaders (addHeader)
import Servant.Utils.Links (safeLink, MkLink, IsElem, HasLink)

import ManageMyTime.Models (get, insertUnique, fromSqlKey, toSqlKey, doMigrations, runDb, Key, Item(..), Task(..), User(..))
import ManageMyTime.Types

type CRUD kty ty = Capture "id" kty :> Get '[JSON] ty
              :<|> ReqBody '[JSON] ty :>
                     Post '[JSON] (Headers '[Header "Location" (MkLink (Get '[JSON] ty))] kty)
              :<|> Capture "id" kty :> ReqBody '[JSON] ty :> Put '[JSON] ()
              :<|> Capture "id" kty :> Delete '[JSON] ()

type CRUDProfile = Get '[JSON] ClientUser
              :<|> ReqBody '[JSON] Registration :>
                     Post '[JSON] (Headers '[Header "Location" (MkLink (Get '[JSON] ClientUser))] ())
              :<|> ReqBody '[JSON] ClientUser :> Put '[JSON] ()
              :<|> Delete '[JSON] ()

type CRUDHours = Get '[JSON] Int
            :<|> ReqBody '[JSON] Int :>
                   Post '[JSON] (Headers '[Header "Location" (MkLink (Get '[JSON] Int))] ())
            :<|> ReqBody '[JSON] Int :> Put '[JSON] ()
            :<|> Delete '[JSON] ()

type GetItems = QueryParam "from" Day :> QueryParam "to" Day :> Get '[JSON] [Item]

type TimeAPI = "task" :> CRUD (Key Task) ClientTask
          :<|> "item" :> CRUD (Key Item) ClientItem
          :<|> "preferred-hours" :> CRUDHours
          :<|> "profile" :> CRUDProfile
          :<|> "user" :> CRUD (Key User) UserWithPerm
          :<|> "tasks" :> Get '[JSON] ([ClientTask], [ClientTask])
          :<|> "items" :> GetItems
          :<|> "users" :> Get '[JSON] (Map UserKey UserWithPerm)


timeAPI :: Servant.Proxy TimeAPI
timeAPI = Servant.Proxy

apiLink :: (IsElem endpoint TimeAPI, HasLink endpoint) =>
            Servant.Proxy endpoint -> MkLink endpoint
apiLink = safeLink timeAPI

type AppM = EitherT Servant.ServantErr IO

getTask :: (Key Task) -> AppM ClientTask
getTask k = do
  mTask <- runDb $ get  k
  hoistEither $ note err404 $ fmap taskName mTask
newTask taskname = do
  mNewKey <- runDb $ insertUnique $ Task taskname $ toSqlKey 1
  hoistEither $ case mNewKey of
    (Just key) -> Right $ addHeader link key
                    where link = apiLink (Servant.Proxy :: Servant.Proxy ("task" :> Capture "id" (Key Task) :> Get '[JSON] ClientTask)) $ key
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
register = undefined
updateProfile = undefined
deleteProfile = undefined
getUser = undefined
newUser = undefined
updateUser = undefined
deleteUser = undefined
getTasks = undefined
getItems = undefined
getUsers = undefined


server :: Servant.Server TimeAPI
server = taskCrud
    :<|> itemCrud
    :<|> preferredHoursCrud
    :<|> profileCrud
    :<|> userCrud
    :<|> getTasks
    :<|> getItems
    :<|> getUsers

taskCrud = getTask :<|> newTask :<|> updateTask :<|> deleteTask
itemCrud = getItem :<|> newItem :<|> updateItem :<|> deleteItem
preferredHoursCrud = getPreferredHours :<|> newPreferredHours :<|> updatePreferredHours :<|> deletePreferredHours
profileCrud = getProfile :<|> register :<|> updateProfile :<|> deleteProfile
userCrud = getUser :<|> newUser :<|> updateUser :<|> deleteUser

app :: Application
app = Servant.serve timeAPI server
