{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ManageMyTime.Docs where

import qualified Data.Map                      as Map
import           Data.Text                     (Text)
import           Data.Time.Calendar            (Day, fromGregorian)
import           Database.Persist.Sql          (Entity (..), toSqlKey)
import           Network.URI                   (URI (..), URIAuth (..))
import           Servant                       (Proxy (..))
import           Servant.API                   (QueryParam)
import           Servant.Docs                  (API, DocQueryParam (..),
                                                ToParam, ToSample, docs,
                                                singleSample, toSamples)
import qualified Servant.Docs                  as Docs

import           ManageMyTime                  (timeAPI)
import           ManageMyTime.Docs.Undecidable ()
import           ManageMyTime.Models           (Item (..))
import           ManageMyTime.Types            (AuthLevel (..), ClientItem (..),
                                                EntityMap (..),
                                                Registration (..),
                                                UserWithPerm (..))

apiDocs :: API
apiDocs = docs timeAPI

instance ToSample Int where
    toSamples _ = singleSample 42

instance ToSample Text where
    toSamples _ = singleSample "frobnicate"

profileURI = URI { uriScheme="http"
                 , uriAuthority=Just URIAuth { uriUserInfo=""
                                             , uriRegName="localhost"
                                             , uriPort=":8080"}
                 , uriPath="/profile"
                 , uriQuery="", uriFragment=""}

instance ToSample URI where
    toSamples _ = singleSample profileURI

instance ToSample (Entity Item) where
    toSamples _ = singleSample $
        Entity (toSqlKey 1) $
            Item (toSqlKey 1) (toSqlKey 1) (fromGregorian 2015 8 23) 2

instance ToSample UserWithPerm where
    toSamples _ = singleSample $ UserWithPerm "max" Normal Nothing

instance ToSample Registration where
    toSamples _ = singleSample $ Registration "max" "igh:ei6Woh"

instance (Ord k, ToSample k, ToSample v) => ToSample (EntityMap k v) where
    toSamples _ = singleSample $ EntityMap $ Map.fromList $ fmap (,) samples <*> samples
        where
        samples :: ToSample a => [a]
        samples = map snd $ toSamples Proxy


instance ToSample ClientItem where
    toSamples _ = singleSample $ ClientItem "minutes" 1 (fromGregorian 2015 8 23) 2

instance ToParam (QueryParam "from" Day) where
    toParam _ =
        DocQueryParam "from"
            ["2015-08-23"]
            "Starting date from which to select items"
            Docs.Normal

instance ToParam (QueryParam "to" Day) where
    toParam _ =
        DocQueryParam "to"
            ["2015-08-25"]
            "Ending date until which items will be selected"
            Docs.Normal

