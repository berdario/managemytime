--{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-} -- it'd be nice to avoid this
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


module ManageMyTime.Models
    (module ManageMyTime.Models, Key, toSqlKey, fromSqlKey, get, insert, insertUnique) where

import Data.Maybe

import Control.Monad.IO.Class (liftIO)
import Control.Arrow ((&&&))
import Crypto.Scrypt (EncryptedPass, getEncryptedPass)
import Data.Aeson (ToJSON)
import Data.Map.Strict (Map, fromList)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Time.Calendar (Day)
import Database.Persist (toJsonText)
import Database.Persist.Sql (Key, PersistFieldSql, insert, insertUnique, get, getBy, entityKey, entityVal, toSqlKey, fromSqlKey, selectList, Entity, )
import Database.Persist.Sqlite (SqlBackend(..), runSqlite, runMigration)
import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)

import ManageMyTime.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    UniqueName name
    auth AuthLevel
    passwordHash ByteString
    preferredHours Int Maybe
    deriving Show
Task
    name Text
    authorId UserId
    UserTask name authorId
    deriving Show
Item json
    taskId TaskId
    userId UserId
    day Day
    duration Int
    DayTask userId day taskId
    deriving Show
|]

toMap :: (ToJSON (Key a)) => [Entity a] -> Map Text a
toMap = fromList . map ((toJsonText.entityKey) &&& entityVal)

connectionString :: Text
connectionString = "sqlite.db"

doMigrations :: IO ()
doMigrations = runSqlite connectionString $ runMigration migrateAll

runDb query = runSqlite connectionString query
