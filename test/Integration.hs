{-# LANGUAGE OverloadedStrings #-}

import Data.Either.Combinators (fromRight')
import Control.Concurrent.Thread.Delay (delay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (runEitherT, EitherT)
import Data.Text (unpack)
import Data.Time.Calendar (fromGregorian)
import System.Directory (doesFileExist, removeFile)
import Network.HTTP.Types (statusCode)
import Servant.API ((:<|>)(..))
import Servant.API.ResponseHeaders (getHeaders, getResponse)
import Servant.Common.Req (ServantError(..))
import Servant.Client (BaseUrl(..), Scheme(..), client)

import Test.Tasty (defaultMain, testGroup, TestName)
import Test.Tasty.HUnit (testCase, testCaseSteps, (@=?), Assertion, assertFailure)

import ManageMyTime (timeAPI)
import ManageMyTime.Models (User(..), Task(..), Item(..), get, insert, fromSqlKey, toSqlKey, runDb, doMigrations, connectionString, createUser)
import ManageMyTime.Types

baseUrl = BaseUrl Http "localhost" 3000

getTask :<|> newTask :<|> updateTask :<|> deleteTask = taskCrud $ Just ""
getItem :<|> newItem :<|> updateItem :<|> deleteItem = itemCrud $ Just ""
getPreferredHours :<|> newPreferredHours :<|> updatePreferredHours :<|> deletePreferredHours = preferredHoursCrud $ Just ""
getProfile :<|> updateProfile :<|> deleteProfile = profileCrud $ Just ""
getUser :<|> newUser :<|> updateUser :<|> deleteUser = userCrud $ Just ""
--
taskCrud :<|> itemCrud :<|> preferredHoursCrud :<|> profileCrud :<|> userCrud :<|> getTasks :<|> getItems :<|> getUsers = authApi

authApi :<|> register :<|> login = client timeAPI baseUrl

run op = do
  result <- runEitherT op
  either (assertFailure . show) (const $ return ()) result
  return $ fromRight' result

runWith a op = do
  b <- runEitherT op
  either (assertFailure . show) (a @=?) b
  return b

checkHttpErr err (FailureResponse status _ _) = err @=? (statusCode status)
checkHttpErr _ f = assertFailure $ "unexpected error " ++ show f

expect errChecker op = do
  result <- runEitherT op
  either errChecker unexpected result
  where
    unexpected resp = assertFailure $ "unexpected success " ++ (show $ getResponse resp)

expect' errChecker op = do
  result <- runEitherT op
  either errChecker unexpected result
  where
    unexpected resp = assertFailure $ "unexpected success " ++ show resp

assert :: (Eq a, Show a) => a -> EitherT ServantError IO a -> Assertion
assert a b = runWith a b >> return ()

deleteIfExists fname = do
  doesExist <- doesFileExist fname
  when doesExist (removeFile fname)

main :: IO ()
main = do
  deleteIfExists $ unpack connectionString
  doMigrations
  setupFixture
  defaultMain taskTests

setupFixture :: IO ()
setupFixture = runDb $ do
  liftIO $ putStrLn "attempt to create fixture data"
  maxid <- insert =<< (liftIO $ createUser "max" "xam" Normal (Just 1))
  johnid <- insert =<< (liftIO $ createUser "john" "a" Normal (Just 3))
  adminid <- insert =<< (liftIO $ createUser "admin" "admin" Admin Nothing)
  managerid <- insert =<< (liftIO $ createUser "manager" "manager" Manager Nothing)
  maxTaskid <- insert $ Task "foo" maxid
  johnTaskId <- insert $ Task "foo" johnid
  insert $ Item maxTaskid maxid (fromGregorian 2015 8 14) 2
  insert $ Item johnTaskId johnid (fromGregorian 2015 8 14) 1
  insert $ Item johnTaskId johnid (fromGregorian 2015 8 15) 1
  liftIO $ putStrLn "fixture data OK"


taskTests = testGroup "all tests"
  [testGroup "/profile and registration test"
    [testCase "registration" $ do
       fmap getResponse $ run $ register $ Registration{newUserName="franz", password="kafka"}
       tkn <- fmap getResponse $ run $ login Registration{newUserName="franz", password="kafka"}
       let franzGetTask :<|> _ :<|> _ :<|> _ = taskCrud $ Just tkn
       tkn <- fmap getResponse $ run $ login Registration{newUserName="max", password="xam"}
       let maxGetTask :<|> _ :<|> _ :<|> _ = taskCrud $ Just tkn
       assert "foo" $ maxGetTask $ toSqlKey 1
       expect' (checkHttpErr 403) $ franzGetTask $ toSqlKey 1
       delay 1000000
       tkn <- fmap getResponse $ run $ login Registration{newUserName="max", password="xam"}
       expect' (checkHttpErr 403) $ maxGetTask $ toSqlKey 1 -- new login invalidates old token
       ]
  ,testGroup "/task tests"
    [testCase "getTask" $ assert "foo" $ getTask $ toSqlKey 1
    ,testCase "newTask" $ do
       response <- run $ newTask "fooz"
       assert "fooz" $ getTask $ getResponse response
    ,testCase "duplicateTask" $ do
       expect (checkHttpErr 409) $ newTask "foo"]
  ,testGroup "/profile and registration test"
    [testCase "registration" $ fmap getResponse $ run $ register $ Registration{newUserName="max",password="xam"}
    ]]
