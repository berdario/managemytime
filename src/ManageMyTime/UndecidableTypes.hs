{-# LANGUAGE UndecidableInstances #-}

module ManageMyTime.UndecidableTypes where

import Database.Persist.Sql (Key, SqlBackend, fromSqlKey, toSqlKey)
import Database.Persist.Class (ToBackendKey)
import Servant (FromText, fromText, ToText, toText)

instance (ToBackendKey SqlBackend a) => FromText (Key a) where
  fromText key = fmap toSqlKey $ fromText key
instance (ToBackendKey SqlBackend a) => ToText (Key a) where
  toText key = toText $ fromSqlKey key
