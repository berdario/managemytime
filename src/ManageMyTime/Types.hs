{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module ManageMyTime.Types where

import           Control.Monad              (mzero)
import           Data.Aeson                 (FromJSON, ToJSON,
                                             Value (String, Object), parseJSON,
                                             toJSON)
import           Data.Aeson.Types           (Parser)
import           Data.ByteString.Conversion (FromByteString, ToByteString,
                                             builder, parser)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Int                   (Int64)
import qualified Data.Map                   as Map
import           Data.Text                  (Text, pack, unpack)
import           Data.Time.Calendar         (Day)
import           Database.Persist           (toJsonText)
import           Database.Persist.Sql       (Key)
import           Database.Persist.TH        (derivePersistField)
import           GHC.Generics               (Generic)
import           Network.URI                (escapeURIString,
                                             isUnescapedInURIComponent,
                                             parseURIReference, unEscapeString)
import           Servant                    (URI)
import           Web.HttpApiData            (FromHttpApiData, ToHttpApiData,
                                             parseQueryParam, toQueryParam)

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


instance FromHttpApiData URI where
  -- we rely on fromHeader to decode the bytestring... it's using utf8 which might not be valid
  -- but Network.URI doesn't support unicode urls, and thus we wouldn't get a valid URI out of parseURIReference anyhow
  parseQueryParam txt = maybe invalidURI Right $ parseURIReference $ unEscapeString $ unpack txt
    where
      invalidURI = Left $ pack $ "Invalid URI: " ++ show txt

-- this is correct only for ASCII urls, which are the only ones supported by Network.URI
instance ToHttpApiData URI where
  toQueryParam = pack . escapeURIString isUnescapedInURIComponent . show

instance FromByteString URI where
  parser = (either (fail . unpack) return . parseQueryParam) =<< parser

instance ToByteString URI where
  builder = builder . toQueryParam
