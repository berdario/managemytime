--{-# LANGUAGE EmptyDataDecls #-}
-- it'd be nice to avoid FlexibleContexts
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module ManageMyTime.Models
    (module ManageMyTime.Models, Key, toSqlKey, fromSqlKey, get, insert,
     insertUnique, Unique(..)) where

import           Data.Maybe

import           Control.Arrow           ((&&&))
import           Control.Monad.IO.Class  (liftIO)
import           Crypto.Scrypt           (EncryptedPass, Pass (..),
                                          encryptPassIO', getEncryptedPass)
import           Data.Aeson              (ToJSON)
import           Data.ByteString         (ByteString)
import           Data.Map.Strict         (Map, fromList)
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8)
import           Data.Time.Calendar      (Day)
import           Database.Persist.Class  (Unique)
import           Database.Persist.Sql    (Entity, Key, PersistFieldSql,
                                          entityKey, entityVal, fromSqlKey, get,
                                          getBy, insert, insertUnique,
                                          selectList, toSqlKey, update, (<=.),
                                          (=.), (==.), (>=.))
import           Database.Persist.Sqlite (SqlBackend (..), runMigration,
                                          runSqlite)
import           Database.Persist.TH     (mkMigrate, mkPersist,
                                          persistLowerCase, share, sqlSettings)

import           ManageMyTime.Types

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


createUser name password auth pref = fmap (\hash -> User name auth hash pref) hashIO
  where
   hashIO = fmap getEncryptedPass $ encryptPassIO' $ Pass $ encodeUtf8 password

updateTaskName key newname = update key [TaskName =. newname]

setPreferredHours key val = update key [UserPreferredHours =. val]

toClientItem item = do
  task <- runDb $ get $ itemTaskId item
  let taskname = fmap taskName task
  return ClientItem{task=fromMaybe "" taskname, taskid=fromSqlKey $ itemTaskId item,
                    date=itemDay item, duration=itemDuration item}

pickSelect _   True  Nothing     Nothing   = selectList [] []
pickSelect _   True  (Just from) Nothing   = selectList [ItemDay >=. from] []
pickSelect _   True  Nothing     (Just to) = selectList [ItemDay <=. to  ] []
pickSelect _   True  (Just from) (Just to) = selectList [ItemDay >=. from, ItemDay <=. to] []
pickSelect usr False Nothing     Nothing   = selectList [ItemUserId ==. usr] []
pickSelect usr False (Just from) Nothing   = selectList [ItemUserId ==. usr, ItemDay >=. from] []
pickSelect usr False Nothing     (Just to) = selectList [ItemUserId ==. usr, ItemDay <=. to  ] []
pickSelect usr False (Just from) (Just to) = selectList [ItemUserId ==. usr, ItemDay >=. from, ItemDay <=. to] []

toMap :: (Ord (Key a)) => [Entity a] -> EntityMap (Key a) a
toMap = EntityMap . fromList . map (entityKey &&& entityVal)

connectionString :: Text
connectionString = "sqlite.db"

doMigrations :: IO ()
doMigrations = runSqlite connectionString $ runMigration migrateAll

runDb query = runSqlite connectionString query
