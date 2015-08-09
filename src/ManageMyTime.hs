{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ManageMyTime
    ( app, timeAPI, Task(..) ) where

import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import Control.Monad (mzero)
import Text.Read (readMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.ByteString.Conversion (ToByteString, builder, FromByteString, parser)
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, Value(String))
import Data.Time.Calendar (Day, showGregorian)
import Network.URI (parseURIReference)
import Network.Wai (Application)
import Servant (JSON, (:>), (:<|>)(..), Capture, Headers, Header, ReqBody,
                QueryParam, Get, Post, Put, Delete)
import qualified Servant
import Servant.Utils.Links (safeLink, MkLink, IsElem, HasLink)

newtype Task = Task Text deriving (Eq, Show, Generic)

data User = User {email :: Text, username :: Text} deriving (Eq, Show, Generic)

data Registration = Registration
  { email' :: Text
  , username' :: Text
  , password :: Text
  } deriving (Generic)

data Item = Item
  { task :: Task
  , date :: Day
  , duration :: Int -- assumption: hours, minutes?
  } deriving (Eq, Show, Generic)

instance ToJSON Day where
  toJSON d = toJSON (showGregorian d)

instance FromJSON Day where
  parseJSON (String s) = maybe mzero return $ readMaybe $ unpack s
  parseJSON _ = mzero

instance Servant.FromText Day where
  fromText = readMaybe . unpack

instance Servant.ToText Day where
  toText = pack . showGregorian

instance ToJSON Registration
instance ToJSON Task
instance ToJSON User
instance ToJSON Item
instance FromJSON Registration
instance FromJSON Task
instance FromJSON User
instance FromJSON Item

-- this is correct only for ASCII urls, the proper solution would involve urlencoding and punycode
instance ToByteString Servant.URI where
  builder uri = builder $ show uri

instance FromByteString Servant.URI where
  parser = do
    ptxt <- fmap decodeUtf8' parser
    let parseText (Right txt) = maybe invalidURI return $ parseURIReference $ unpack txt
          where
            invalidURI = fail $ "Invalid URI: " ++ show txt
        parseText (Left exc) = fail $ "Invalid UTF-8: " ++ show exc
    parseText ptxt


type CRUD ty = Capture "id" Int :> Get '[JSON] ty
          :<|> ReqBody '[JSON] ty :>
                 Post '[JSON] (Headers '[Header "Location" (MkLink (Get '[JSON] ty))] ())
          :<|> Capture "id" Int :> ReqBody '[JSON] ty :> Put '[JSON] ()
          :<|> Capture "id" Int :> Delete '[JSON] ()

type CRUDProfile = Get '[JSON] User
              :<|> ReqBody '[JSON] Registration :>
                     Post '[JSON] (Headers '[Header "Location" (MkLink (Get '[JSON] User))] ())
              :<|> ReqBody '[JSON] User :> Put '[JSON] ()
              :<|> Delete '[JSON] ()

type GetItems = QueryParam "from" Day :> QueryParam "to" Day :> Get '[JSON] [Item]

type TimeAPI = "task" :> CRUD Task
          :<|> "item" :> CRUD Item
          :<|> "preferred-hours" :> CRUD Int
          :<|> "profile" :> CRUDProfile
          :<|> "user" :> CRUD User
          :<|> "tasks" :> Get '[JSON] [Task]
          :<|> "items" :> GetItems
          :<|> "users" :> Get '[JSON] [User]


timeAPI :: Servant.Proxy TimeAPI
timeAPI = Servant.Proxy

apiLink :: (IsElem endpoint TimeAPI, HasLink endpoint) =>
            Servant.Proxy endpoint -> MkLink endpoint
apiLink = safeLink timeAPI

getTask :: Int -> Task
getTask = const $ Task $ pack "foo"
newTask = undefined
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

taskCrud = (return . getTask) :<|> newTask :<|> updateTask :<|> deleteTask
itemCrud = getItem :<|> newItem :<|> updateItem :<|> deleteItem
preferredHoursCrud = getPreferredHours :<|> newPreferredHours :<|> updatePreferredHours :<|> deletePreferredHours
profileCrud = getProfile :<|> register :<|> updateProfile :<|> deleteProfile
userCrud = getUser :<|> newUser :<|> updateUser :<|> deleteUser

app :: Application
app = Servant.serve timeAPI server
