{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}

module ManageMyTime where

import GHC.TypeLits (Symbol)
import Control.Arrow ((&&&))
import Control.Error.Util (note, noteT)
import Control.Monad ((<=<))
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
import Data.Tuple.Extra (both)
import Database.Persist.Sql (Entity, entityKey, PersistEntity, PersistEntityBackend, SqlBackend(..),
                             delete, replace, entityVal, selectList)
import Network.Wai (Application)
import Servant (JSON, (:>), (:<|>)(..), Proxy(..), ServantErr(..), Capture, Headers, Header,
                ReqBody, QueryParam, Get, Post, Put, Delete, err403, err404, err409)
import qualified Servant
import Servant.API.ResponseHeaders (addHeader)
import Servant.Utils.Links (safeLink, MkLink, IsElem, HasLink)

import ManageMyTime.Models (get, insertUnique, fromSqlKey, toSqlKey, doMigrations, runDb,
                            Key, Item(..), Task(..), User(..), createUser, updateTaskName,
                            toClientItem, userPreferredHours, setPreferredHours, userName,
                            Unique(..), pickSelect, toMap)
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

type CRUDHours = Get '[JSON] (Maybe Int)
            :<|> ReqBody '[JSON] Int :> Post '[JSON] ()
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
validateLevel minLevel = (\u -> if (>= minLevel)(userAuth $ entityVal u) then (return u) else (left err403)) <=< liftValidate

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
  mNewKey <- runDb $ insertUnique $ Task taskname $ entityKey usr
  hoistEither $ case mNewKey of
    (Just key) -> Right $ addHeader (taskLink key) key
    Nothing -> Left err409

updateTask :: Text -> (Key Task) -> ClientTask -> AppM ()
updateTask tkn key newtext = do
  usr <- liftValidate tkn
  mTask <- runDb $ get key
  case mTask of
    Nothing -> left err404
    (Just task) -> if (entityKey usr == taskAuthorId task) then
                      runDb $ updateTaskName key newtext
                   else left err403

deleteTask = withTaskPerms (\_ key _ -> runDb $ delete key)

withItemPerms :: (Entity User -> (Key Item) -> Item -> AppM b) -> Text -> (Key Item) -> AppM b
withItemPerms f = withUserFrom f'
  where f' = (\usr key item -> if (entityKey usr == itemUserId item) then (f usr key item) else left err403)


getItem :: Text -> (Key Item) -> AppM ClientItem
getItem = withItemPerms (\_ _ t -> toClientItem t)

newItem tkn ClientItem{..} = do
  usr <- liftValidate tkn
  mNewKey <- runDb $ insertUnique $ Item{itemTaskId=toSqlKey taskid, itemUserId=entityKey usr,
                                         itemDay=date, itemDuration=duration}
  case mNewKey of
    (Just key) -> right $ addHeader (itemLink key) key
    Nothing -> left err409

updateItem :: Text -> (Key Item) -> ClientItem -> AppM ()
updateItem tkn key ClientItem{..} = do
  usr <- liftValidate tkn
  mItem <- runDb $ get key
  case mItem of
    Nothing -> left err404
    (Just item) -> if (entityKey usr == itemUserId item) then
                       runDb $ replace key $ Item{itemTaskId=toSqlKey taskid, itemUserId=entityKey usr,
                                                        itemDay=date, itemDuration=duration}
                   else left err403

deleteItem = withItemPerms (\_ key _ -> runDb $ delete key)

getPreferredHours :: Text -> AppM (Maybe Int)
getPreferredHours tkn = do
  usr <- liftValidate tkn
  return $ userPreferredHours $ entityVal usr

updatePreferredHours tkn val = do
  usr <- liftValidate tkn
  runDb $ setPreferredHours (entityKey usr) (Just val)

deletePreferredHours tkn =  do
  usr <- liftValidate tkn
  runDb $ setPreferredHours (entityKey usr) Nothing

getProfile :: Text -> AppM Text
getProfile tkn = do
  usr <- liftValidate tkn
  return $ userName $ entityVal usr

updateProfile :: Text -> Text -> AppM ()
updateProfile tkn newname = do
  usr <- liftValidate tkn
  -- it'd be nice to use replaceUnique here, but `No instance for (Eq (Unique User))`
  runDb $ replace (entityKey usr) $ (entityVal usr){userName=newname}


deleteProfile :: Text -> AppM ()
deleteProfile tkn = do
  usr <- liftValidate tkn
  runDb $ delete $ entityKey usr

validateAdmin = validateLevel Admin

toUserWithPerm User{..} = UserWithPerm{username=userName, auth=userAuth}

getUser :: Text -> (Key User) -> AppM UserWithPerm
getUser tkn key = do
  validateAdmin tkn
  target <- runDb $ get key
  maybe (left err404) (return . toUserWithPerm) target

newUser tkn UserWithPerm{..} = do
  validateAdmin tkn
  newuser <- liftIO $ createUser username $ pack ""
  mNewKey <- runDb $ insertUnique newuser{userAuth=auth}
  hoistEither $ case mNewKey of
    (Just key) -> Right $ addHeader (userLink key) key
    Nothing -> Left err409

updateUser :: Text -> (Key User) -> UserWithPerm -> AppM ()
updateUser tkn key UserWithPerm{..} = do
  validateAdmin tkn
  mTarget <- runDb $ get key
  case mTarget of
    Nothing -> left err404
    (Just target) -> runDb $ replace key $ target{userName=username, userAuth=auth}

deleteUser :: Text -> (Key User) -> AppM ()
deleteUser tkn key = do
  validateAdmin tkn
  runDb $ delete key

getTasks :: Text -> AppM ([ClientTask], [ClientTask])
getTasks tkn = do
  usr <- fmap entityKey $ liftValidate tkn
  allTasks <- runDb $ selectList [] []
  return $ both (map taskName) $ span ((usr==) . taskAuthorId) $ map entityVal allTasks

getItems :: Text -> Maybe Day -> Maybe Day -> AppM [Item]
getItems tkn from to = do
  (key, auth) <- fmap (entityKey &&& (userAuth.entityVal)) $ liftValidate tkn
  items <- runDb $ pickSelect key (auth >= Manager) from to
  return $ map entityVal items

getUsers :: Text -> AppM (Map UserKey UserWithPerm)
getUsers tkn = do
  validateLevel Manager tkn
  users <- runDb $ selectList [] []
  return $ fmap toUserWithPerm $ toMap users

logout :: Text -> AppM ()
logout tkn = do
  usr <- liftValidate tkn
  liftIO $ Auth.logout $ userName $ entityVal usr

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

crudHoursAccessDenied e = left e
                     :<|> const (left e)
                     :<|> const (left e)
                     :<|> left e

crudProfileAccessDenied e = left e
                       :<|> const (left e)
                       :<|> left e

authApi =  maybe (crudAccessDenied err403) taskCrud
     :<|>  maybe (crudAccessDenied err403) itemCrud
     :<|>  maybe (crudHoursAccessDenied err403) preferredHoursCrud
     :<|>  maybe (crudProfileAccessDenied err403) profileCrud
     :<|>  maybe (crudAccessDenied err403) userCrud
     :<|>  maybe (left err403) getTasks
     :<|>  maybe (const $ const $ left err403) getItems
     :<|>  maybe (left err403) getUsers
     :<|>  maybe (left err403) logout

taskCrud tkn =  getTask tkn :<|>  newTask tkn :<|> updateTask tkn :<|> deleteTask tkn
itemCrud tkn = getItem tkn :<|> newItem tkn :<|> updateItem tkn :<|> deleteItem tkn
preferredHoursCrud tkn = getPreferredHours tkn :<|> updatePreferredHours tkn :<|> updatePreferredHours tkn :<|> deletePreferredHours tkn
profileCrud tkn = getProfile tkn :<|> updateProfile tkn :<|> deleteProfile tkn
userCrud tkn = getUser tkn :<|> newUser tkn :<|> updateUser tkn :<|> deleteUser tkn

app :: Application
app = Servant.serve timeAPI server
