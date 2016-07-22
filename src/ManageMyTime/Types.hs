{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module ManageMyTime.Types (module ManageMyTime.Types, module ManageMyTime.UndecidableTypes) where

import           Control.Monad                 (mzero)
import           Data.Aeson                    (FromJSON, ToJSON,
                                                Value (String, Object),
                                                eitherDecode, parseJSON, toJSON)
import           Data.Aeson.Types              (Parser)
import           Data.ByteString.Conversion    (FromByteString, ToByteString,
                                                builder, parser)
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                      (Int64)
import qualified Data.Map                      as Map
import           Data.Text                     (Text, pack, unpack)
import           Data.Text.Encoding            (decodeUtf8', encodeUtf8)
import           Data.Time.Calendar            (Day, showGregorian)
import           Database.Persist              (toJsonText)
import           Database.Persist.Sql          (Key)
import           Database.Persist.TH           (derivePersistField)
import           GHC.Generics                  (Generic)
import           Network.URI                   (parseURIReference)
import           Servant                       (FromText, ToText, URI, fromText,
                                                toText)
import           Text.Read                     (readMaybe)

import           ManageMyTime.UndecidableTypes

instance ToJSON Day where
  toJSON d = toJSON (showGregorian d)

instance FromJSON Day where
  parseJSON (String s) = maybe mzero return $ readMaybe $ unpack s
  parseJSON _ = mzero

type ClientTask = Text
type ClientUser = Text

data AuthLevel = Normal | Manager | Admin deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)
derivePersistField "AuthLevel"

data UserWithPerm = UserWithPerm {username :: Text, auth :: AuthLevel, prefHours :: Maybe Int} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Registration = Registration
  { newUserName :: Text
  , password    :: Text
  } deriving (Generic, ToJSON, FromJSON)

data ClientItem = ClientItem
  { task     :: ClientTask
  , taskid   :: Int64
  , date     :: Day
  , duration :: Int -- assumption: hours, minutes?
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype EntityMap k a = EntityMap (Map.Map k a) deriving (Functor)

instance (ToJSON (Key a), ToJSON v) => ToJSON (EntityMap (Key a) v) where
  toJSON (EntityMap m) = Object $ Map.foldrWithKey f mempty m
    where f k v = HashMap.insert (toJsonText k) (toJSON v)

instance (Ord (Key a), FromJSON (Key a), FromJSON v) => FromJSON (EntityMap (Key a) v) where
  parseJSON (Object m) = EntityMap <$> HashMap.foldrWithKey parseKeyPairs (pure mempty) m
    where
      parseKeyPairs :: (Ord (Key a), FromJSON (Key a), FromJSON v) => Text -> Value -> Parser (Map.Map (Key a) v) -> Parser (Map.Map (Key a) v)
      parseKeyPairs k v mapParser = do
        key <- parseJSON $ Data.Aeson.String k
        value <- parseJSON v
        fmap (Map.insert key value) mapParser
  parseJSON _ = mzero

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
