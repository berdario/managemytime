{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}

module ManageMyTime where

import GHC.TypeLits (Symbol, KnownSymbol)
import Control.Arrow ((&&&))
import Control.Error.Util (note, noteT)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Error (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Text.Read (readMaybe)
import Data.Int (Int64)
import Data.List (partition)
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Calendar (Day, showGregorian)
import Data.Tuple.Extra (both)
import Database.Persist.Sql (Entity, entityKey, PersistEntity, PersistEntityBackend, SqlBackend(..),
                             delete, replace, replaceUnique, entityVal, selectList)
import Network.Wai (Application)
import Network.URI (URI)
import Servant (JSON, (:>), (:<|>)(..), Proxy(..), ServantErr(..), Capture, Headers, Header,
                ReqBody, QueryParam, Get, Post, Put, Delete, err403, err404, err409, Raw)
import Servant (ServerT, Server, serveDirectory, serve, (:~>)(..), enter, URI)
import Servant.API.ResponseHeaders (addHeader)
import Servant.Utils.Links (safeLink, MkLink, IsElem, HasLink)

import ManageMyTime.Models (AppEnv, AppM, Token, get, insertUnique, fromSqlKey, toSqlKey, doMigrations, runDb,
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

type GetItems = QueryParam "from" Day :> QueryParam "to" Day :> Get '[JSON] [Entity Item]

type AuthenticatedAPI = Auth :> "task" :> CRUD (Key Task) ClientTask
                   :<|> Auth :> "item" :> CRUD (Key Item) ClientItem
                   :<|> Auth :> "preferred-hours" :> CRUDHours
                   :<|> Auth :> "profile" :> CRUDProfile
                   :<|> Auth :> "user" :> CRUD (Key User) UserWithPerm
                   :<|> Auth :> "tasks" :> Get '[JSON] (EntityMap (Key Task) ClientTask, EntityMap (Key Task) ClientTask)
                   :<|> Auth :> "items" :> GetItems
                   :<|> Auth :> "users" :> Get '[JSON] (EntityMap (Key User) UserWithPerm)
                   :<|> Auth :> "logout" :> Post '[JSON] ()

type TimeAPI = AuthenticatedAPI
          :<|> "registration" :> AuthRoute "profile" ClientUser ()
          :<|> "login" :> AuthRoute "items" [Item] Text

timeAPI :: Proxy TimeAPI
timeAPI = Proxy

apiLink :: (IsElem endpoint TimeAPI, HasLink endpoint) =>
            Proxy endpoint -> MkLink endpoint
apiLink = safeLink timeAPI
taskLink :: Key Task -> URI
taskLink = apiLink (Proxy :: Proxy (Auth :> "task" :> Capture "id" (Key Task) :> Get '[JSON] ClientTask))
itemLink :: Key Item -> URI
itemLink = apiLink (Proxy :: Proxy (Auth :> "item" :> Capture "id" (Key Item) :> Get '[JSON] ClientItem))
itemsLink :: URI
itemsLink = apiLink (Proxy :: Proxy (Auth :> "items" :> Get '[JSON] [Entity Item]))
profileLink :: URI
profileLink = apiLink (Proxy :: Proxy (Auth :> "profile" :> Get '[JSON] ClientUser))
userLink :: Key User -> URI
userLink = apiLink (Proxy :: Proxy (Auth :> "user" :> Capture "id" (Key User) :> Get '[JSON] UserWithPerm))


decorateServantError :: (Show a) => ServantErr -> a -> ServantErr
decorateServantError sErr errShowable = sErr{errBody=encodeUtf8 $ TL.pack $ show $ errShowable}

decorateAuthError :: (Show a) => a -> ServantErr
decorateAuthError = decorateServantError err403

reportDuplicateError :: (Show (Unique a)) => Maybe (Unique a) -> AppM ()
reportDuplicateError Nothing = return ()
reportDuplicateError (Just duplicate) = throwError $ decorateServantError err409  $ "Duplicate record: " ++ show duplicate

liftValidate :: Text -> AppM (Entity User)
liftValidate tkn = do
  eitherUsr <- Auth.validate tkn
  case eitherUsr of
    Right usr -> return usr
    Left reason -> throwError $ decorateAuthError reason

validateLevel :: AuthLevel -> Token -> AppM (Entity User)
validateLevel minLevel = (\u -> if (>= minLevel)(userAuth $ entityVal u) then (return u) else (throwError err403)) <=< liftValidate

withUserFrom :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend) => (Entity User -> (Key val) -> val -> AppM b) -> Text -> (Key val) -> AppM b
withUserFrom f tkn key = do
  usr <- liftValidate tkn
  mTask <- runDb $ get key
  case mTask of
    Nothing -> throwError err404
    (Just task) -> f usr key task


withTaskPerms :: (Entity User -> (Key Task) -> Task -> AppM b) -> Text -> (Key Task) -> AppM b
withTaskPerms f = withUserFrom f'
  where f' = (\usr key task -> if (entityKey usr == taskAuthorId task) then (f usr key task) else throwError err403)

getTask :: Text -> (Key Task) -> AppM ClientTask
getTask = withTaskPerms (\_ _ t -> return $ taskName t)

newTask :: KnownSymbol h =>
           Text ->
           Text ->
           AppM (Headers '[Header h URI] (Key Task))
newTask tkn taskname = do
  usr <- liftValidate tkn
  mNewKey <- runDb $ insertUnique $ Task taskname $ entityKey usr
  case mNewKey of
    (Just key) -> return $ addHeader (taskLink key) key
    Nothing -> throwError err409

updateTask :: Text -> (Key Task) -> ClientTask -> AppM ()
updateTask tkn key newtext = do
  usr <- liftValidate tkn
  mTask <- runDb $ get key
  case mTask of
    Nothing -> throwError err404
    (Just task) -> if (entityKey usr == taskAuthorId task) then
                      runDb $ updateTaskName key newtext
                   else throwError err403

deleteTask :: Text -> Key Task -> AppM ()
deleteTask = withTaskPerms (\_ key _ -> runDb $ delete key)

withItemPerms :: (Entity User -> (Key Item) -> Item -> AppM b) -> Text -> (Key Item) -> AppM b
withItemPerms f = withUserFrom f'
  where f' = (\usr key item -> if (entityKey usr == itemUserId item) then (f usr key item) else throwError err403)


getItem :: Text -> (Key Item) -> AppM ClientItem
getItem = withItemPerms (\_ _ t -> toClientItem t)

newItem :: KnownSymbol h =>
           Text ->
           ClientItem ->
           AppM (Headers '[Header h URI] (Key Item))
newItem tkn ClientItem{..} = do
  usr <- liftValidate tkn
  mNewKey <- runDb $ insertUnique $ Item{itemTaskId=toSqlKey taskid, itemUserId=entityKey usr,
                                         itemDay=date, itemDuration=duration}
  case mNewKey of
    (Just key) -> return $ addHeader (itemLink key) key
    Nothing -> throwError err409

updateItem :: Text -> (Key Item) -> ClientItem -> AppM ()
updateItem tkn key ClientItem{..} = do
  usr <- liftValidate tkn
  mItem <- runDb $ get key
  case mItem of
    Nothing -> throwError err404
    (Just item) -> if (entityKey usr == itemUserId item) then
                       runDb $ replace key $ Item{itemTaskId=toSqlKey taskid, itemUserId=entityKey usr,
                                                        itemDay=date, itemDuration=duration}
                   else throwError err403

deleteItem :: Text -> Key Item -> AppM ()
deleteItem = withItemPerms (\_ key _ -> runDb $ delete key)

getPreferredHours :: Text -> AppM (Maybe Int)
getPreferredHours tkn = do
  usr <- liftValidate tkn
  return $ userPreferredHours $ entityVal usr

updatePreferredHours :: Text -> Int -> AppM ()
updatePreferredHours tkn val = do
  usr <- liftValidate tkn
  runDb $ setPreferredHours (entityKey usr) (Just val)

deletePreferredHours :: Text -> AppM ()
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
  reportDuplicateError =<< (runDb $ replaceUnique (entityKey usr) $ (entityVal usr){userName=newname})


deleteProfile :: Text -> AppM ()
deleteProfile tkn = do
  usr <- liftValidate tkn
  runDb $ delete $ entityKey usr

validateAdmin :: Text -> AppM (Entity User)
validateAdmin = validateLevel Admin

toUserWithPerm :: User -> UserWithPerm
toUserWithPerm User{..} = UserWithPerm{username=userName, auth=userAuth, prefHours=userPreferredHours}

getUser :: Text -> (Key User) -> AppM UserWithPerm
getUser tkn key = do
  validateAdmin tkn
  target <- runDb $ get key
  maybe (throwError err404) (return . toUserWithPerm) target


newUser :: KnownSymbol h =>
           Text ->
           UserWithPerm ->
           AppM (Headers '[Header h URI] (Key User))
newUser tkn UserWithPerm{..} = do
  validateAdmin tkn
  newuser <- liftIO $ createUser username (pack "changeme") auth prefHours
  mNewKey <- runDb $ insertUnique newuser
  case mNewKey of
    (Just key) -> return $ addHeader (userLink key) key
    Nothing -> throwError err409

updateUser :: Text -> (Key User) -> UserWithPerm -> AppM ()
updateUser tkn key UserWithPerm{..} = do
  validateAdmin tkn
  mTarget <- runDb $ get key
  case mTarget of
    Nothing -> throwError err404
    (Just target) -> reportDuplicateError =<< (runDb $ replaceUnique key $ target{userName=username, userAuth=auth, userPreferredHours=prefHours})

deleteUser :: Text -> (Key User) -> AppM ()
deleteUser tkn key = do
  validateAdmin tkn
  runDb $ delete key

getTasks :: Text -> AppM (EntityMap (Key Task) ClientTask, EntityMap (Key Task) ClientTask)
getTasks tkn = do
  usr <- fmap entityKey $ liftValidate tkn
  allTasks <- runDb $ selectList [] []
  return $ both ((fmap taskName). toMap) $ partition ((usr==) . taskAuthorId . entityVal) allTasks

getItems :: Text -> Maybe Day -> Maybe Day -> AppM [Entity Item]
getItems tkn from to = do
  (key, auth) <- fmap (entityKey &&& (userAuth.entityVal)) $ liftValidate tkn
  runDb $ pickSelect key (auth >= Manager) from to

getUsers :: Text -> AppM (EntityMap (Key User) UserWithPerm)
getUsers tkn = do
  validateLevel Manager tkn
  users <- runDb $ selectList [] []
  return $ fmap toUserWithPerm $ toMap users

logout :: Text -> AppM ()
logout tkn = do
  usr <- liftValidate tkn
  Auth.logout $ userName $ entityVal usr

register :: KnownSymbol h =>
            Registration ->
            AppM (Headers '[Header h URI] ())
register Registration{..} = do
  user <- liftIO $ createUser newUserName password Normal Nothing
  mNewKey <- runDb $ insertUnique user
  case mNewKey of
    (Just key) -> return $ addHeader profileLink ()
    Nothing -> throwError err409

login :: KnownSymbol h =>
         Registration ->
         AppM (Headers '[Header h URI] Text)
login Registration{..} = do
  eitherTkn <- Auth.login newUserName password
  case eitherTkn of
    Right tkn -> return $ addHeader itemsLink tkn
    Left e -> throwError $ decorateAuthError e


server :: ServerT TimeAPI AppM
server = authApi
    :<|> register
    :<|> login

crudAccessDenied e = const (throwError e)
                :<|> const (throwError e)
                :<|> const (const (throwError e))
                :<|> const (throwError e)

crudHoursAccessDenied e = throwError e
                     :<|> const (throwError e)
                     :<|> const (throwError e)
                     :<|> throwError e

crudProfileAccessDenied e = throwError e
                       :<|> const (throwError e)
                       :<|> throwError e

authApi =  maybe (crudAccessDenied err403) taskCrud
     :<|>  maybe (crudAccessDenied err403) itemCrud
     :<|>  maybe (crudHoursAccessDenied err403) preferredHoursCrud
     :<|>  maybe (crudProfileAccessDenied err403) profileCrud
     :<|>  maybe (crudAccessDenied err403) userCrud
     :<|>  maybe (throwError err403) getTasks
     :<|>  maybe (const $ const $ throwError err403) getItems
     :<|>  maybe (throwError err403) getUsers
     :<|>  maybe (throwError err403) logout

type CRUDEndPoints ty clientTy = KnownSymbol h =>
                                 (Key ty -> AppM clientTy)
                                 :<|> ((clientTy -> AppM (Headers '[Header h Servant.URI] (Key ty)))
                                 :<|> ((Key ty -> clientTy -> AppM ())
                                 :<|> (Key ty -> AppM ())))

taskCrud :: Text -> CRUDEndPoints Task ClientTask
taskCrud tkn =  getTask tkn :<|>  newTask tkn :<|> updateTask tkn :<|> deleteTask tkn
itemCrud :: Text -> CRUDEndPoints Item ClientItem
itemCrud tkn = getItem tkn :<|> newItem tkn :<|> updateItem tkn :<|> deleteItem tkn
preferredHoursCrud :: Text -> AppM (Maybe Int)
                  :<|> ((Int -> AppM ())
                  :<|> ((Int -> AppM ())
                  :<|> AppM ()))
preferredHoursCrud tkn = getPreferredHours tkn :<|> updatePreferredHours tkn :<|> updatePreferredHours tkn :<|> deletePreferredHours tkn
profileCrud :: Text -> AppM ClientUser :<|> ((ClientUser -> AppM ()) :<|> AppM ())
profileCrud tkn = getProfile tkn :<|> updateProfile tkn :<|> deleteProfile tkn
userCrud :: Text -> CRUDEndPoints User UserWithPerm
userCrud tkn = getUser tkn :<|> newUser tkn :<|> updateUser tkn :<|> deleteUser tkn

api :: Proxy (TimeAPI :<|> Raw)
api = Proxy

mainServer :: AppEnv -> Server (TimeAPI :<|> Raw)
mainServer env = enter (withEnv env) server :<|> serveDirectory "frontend"

withEnv :: AppEnv -> AppM :~> ExceptT ServantErr IO
withEnv env = Nat (flip runReaderT env)

app :: AppEnv -> Application
app env = serve api $ mainServer env
