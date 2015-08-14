{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module ManageMyTime.Types (module ManageMyTime.Types, module ManageMyTime.UndecidableTypes) where

import GHC.Generics (Generic)
import Control.Monad (mzero)
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, Value(String))
import Data.Int (Int64)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8')
import Network.URI (parseURIReference)
import Text.Read (readMaybe)
import Data.ByteString.Conversion (ToByteString, builder, FromByteString, parser)
import Data.Time.Calendar (Day, showGregorian)
import Database.Persist.TH (derivePersistField)
import Servant (FromText, fromText, ToText, toText, URI)

import ManageMyTime.UndecidableTypes

instance ToJSON Day where
  toJSON d = toJSON (showGregorian d)

instance FromJSON Day where
  parseJSON (String s) = maybe mzero return $ readMaybe $ unpack s
  parseJSON _ = mzero

type UserKey = Text
type TaskKey = Text
type ClientTask = Text
type ClientUser = Text

data AuthLevel = Normal | Manager | Admin deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)
derivePersistField "AuthLevel"

data UserWithPerm = UserWithPerm {username :: Text, auth :: AuthLevel, prefHours :: Maybe Int} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Registration = Registration
  { newUserName :: Text
  , password :: Text
  } deriving (Generic, ToJSON, FromJSON)

data ClientItem = ClientItem
  { task :: ClientTask
  , taskid :: Int64
  , date :: Day
  , duration :: Int -- assumption: hours, minutes?
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance FromText Day where
  fromText = readMaybe . unpack

instance ToText Day where
  toText = pack . showGregorian

-- this is correct only for ASCII urls, the proper solution would involve urlencoding and punycode
instance ToByteString URI where
  builder uri = builder $ show uri

instance FromByteString URI where
  parser = do
    ptxt <- fmap decodeUtf8' parser
    let parseText (Right txt) = maybe invalidURI return $ parseURIReference $ unpack txt
          where
            invalidURI = fail $ "Invalid URI: " ++ show txt
        parseText (Left exc) = fail $ "Invalid UTF-8: " ++ show exc
    parseText ptxt
