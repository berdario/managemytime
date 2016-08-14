{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.Thread.Delay (delay)
import           Control.Exception               (bracket)
import           Control.Monad                   (when)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Except      (ExceptT, runExceptT)
import           Data.Either.Combinators         (fromRight')
import           Data.Text                       (unpack)
import           Data.Time.Calendar              (fromGregorian)
import           Database.Persist.Sql            (ConnectionPool, runSqlPool)
import qualified Network.HTTP.Client             as HTTP
import           Network.HTTP.Types              (statusCode)
import           Servant.API                     ((:<|>) (..), Headers)
import           Servant.API.ResponseHeaders     (getHeaders, getResponse)
import           Servant.Client                  (BaseUrl (..), Scheme (..),
                                                  client)
import           Servant.Common.Req              (ServantError (..))
import           System.Directory                (doesFileExist, removeFile)

import           Test.Tasty                      (TestName, defaultMain,
                                                  testGroup)
import           Test.Tasty.HUnit                (Assertion, assertFailure,
                                                  testCase, testCaseSteps,
                                                  (@=?))

import           ManageMyTime                    (timeAPI)
import           ManageMyTime.Models             (AppQuery, Item (..),
                                                  Task (..), User (..),
                                                  connectionString, createPool,
                                                  createUser, doMigrations,
                                                  fromSqlKey, get, insert,
                                                  toSqlKey)
import           ManageMyTime.Types

baseUrl = BaseUrl Http "localhost" 3000 ""
withManager = bracket (HTTP.newManager HTTP.defaultManagerSettings) return

getTask :<|> newTask :<|> updateTask :<|> deleteTask = taskCrud $ Just ""
getItem :<|> newItem :<|> updateItem :<|> deleteItem = itemCrud $ Just ""
getPreferredHours :<|> newPreferredHours :<|> updatePreferredHours :<|> deletePreferredHours = preferredHoursCrud $ Just ""
getProfile :<|> updateProfile :<|> deleteProfile = profileCrud $ Just ""
getUser :<|> newUser :<|> updateUser :<|> deleteUser = userCrud $ Just ""
--
taskCrud :<|> itemCrud :<|> preferredHoursCrud :<|> profileCrud :<|> userCrud :<|> getTasks :<|> getItems :<|> getUsers = authApi

authApi :<|> register :<|> login = client timeAPI

run :: Show a => (HTTP.Manager -> BaseUrl -> ExceptT a IO r) -> IO r
run op = withManager (\manager -> do
  result <- runExceptT $ op manager baseUrl
  either (assertFailure . show) (const $ return ()) result
  return $ fromRight' result)

runWith :: (Eq b, Show b, Show a) =>
        b -> (HTTP.Manager -> BaseUrl -> ExceptT a IO b) -> IO (Either a b)
runWith a op = withManager (\manager -> do
  b <- runExceptT $ op manager baseUrl
  either (assertFailure . show) (a @=?) b
  return b)

checkHttpErr :: Int -> ServantError -> Assertion
checkHttpErr err (FailureResponse status _ _) = err @=? (statusCode status)
checkHttpErr _ f = assertFailure $ "unexpected error " ++ show f

expect :: Show r =>
      (a -> IO ()) ->
      (HTTP.Manager ->
          BaseUrl ->
          ExceptT a IO (Headers ls r)) ->
      IO ()
expect errChecker op = withManager (\manager -> do
  result <- runExceptT $ op manager baseUrl
  either errChecker unexpected result)
  where
    unexpected resp = assertFailure $ "unexpected success " ++ (show $ getResponse resp)

expect' :: Show b =>
        (a -> IO ()) ->
        (HTTP.Manager -> BaseUrl -> ExceptT a IO b) ->
        IO ()
expect' errChecker op = withManager (\manager -> do
  result <- runExceptT $ op manager baseUrl
  either errChecker unexpected result)
  where
    unexpected resp = assertFailure $ "unexpected success " ++ show resp

assert :: (Eq a, Show a) => a -> (HTTP.Manager -> BaseUrl -> ExceptT ServantError IO a) -> Assertion
assert a b = runWith a b >> return ()

deleteIfExists fname = do
  doesExist <- doesFileExist fname
  when doesExist (removeFile fname)

main :: IO ()
main = do
  deleteIfExists $ unpack connectionString
  pool <- createPool
  doMigrations pool
  setupFixture pool
  defaultMain taskTests

setupFixture :: ConnectionPool -> IO ()
setupFixture = runSqlPool $ do
  liftIO $ putStrLn "attempt to create fixture data"
  maxid <- insert =<< (liftIO $ createUser "max" "xam" Normal (Just 1))
  johnid <- insert =<< (liftIO $ createUser "john" "a" Normal (Just 3))
  adminid <- insert =<< (liftIO $ createUser "admin" "admin" Admin Nothing)
  managerid <- insert =<< (liftIO $ createUser "manager" "manager" Manager Nothing)
  maxMinutesTaskId <- insert $ Task "minutes" maxid
  johnMinutesTaskId <- insert $ Task "minutes" johnid
  insert $ Item maxMinutesTaskId maxid (fromGregorian 2015 8 23) 2
  insert $ Item johnMinutesTaskId johnid (fromGregorian 2015 8 23) 1
  insert $ Item johnMinutesTaskId johnid (fromGregorian 2015 8 24) 1
  liftIO $ putStrLn "fixture data OK"


taskTests = testGroup "all tests"
  [testGroup "/profile and registration test"
    [testCase "registration" $ do
       fmap getResponse $ run $ register $ Registration{newUserName="franz", password="kafka"}
       tkn <- fmap getResponse $ run $ login Registration{newUserName="franz", password="kafka"}
       let franzProfile :<|> _ :<|> _ = profileCrud $ Just tkn
       --assert "franz" $ fmap username $ run franzProfile
       assert "franz" franzProfile
    ,testCase "concurrent users login" $ do
       tkn <- fmap getResponse $ run $ login Registration{newUserName="john", password="a"}
       let johnProfile :<|> _ :<|> _ = profileCrud $ Just tkn
       tkn <- fmap getResponse $ run $ login Registration{newUserName="max", password="xam"}
       let maxGetTask :<|> _ :<|> _ :<|> _ = taskCrud $ Just tkn
       assert "minutes" $ maxGetTask $ toSqlKey 1
      --  assert "john" $ fmap username $ run johnProfile
       assert "john" johnProfile
    ,testCase "single login per user" $ do
       tkn <- fmap getResponse $ run $ login Registration{newUserName="john", password="a"}
       let johnProfile :<|> _ :<|> _ = profileCrud $ Just tkn
       delay 1000000
       fmap getResponse $ run $ login Registration{newUserName="john", password="a"}
       expect' (checkHttpErr 403) johnProfile -- new login invalidates old token
       ]
  ,testGroup "/task tests"
    [testCase "getTask" $ do
       tkn <- fmap getResponse $ run $ login Registration{newUserName="max", password="xam"}
       let maxGetTask :<|> _ :<|> _ :<|> _ = taskCrud $ Just tkn
       tkn <- fmap getResponse $ run $ login Registration{newUserName="john", password="a"}
       let johnGetTask :<|> _ :<|> _ :<|> _ = taskCrud $ Just tkn
       assert "minutes" $ maxGetTask $ toSqlKey 1
       expect' (checkHttpErr 403) $ johnGetTask $ toSqlKey 1
    ,testCase "newTask" $ do
      tkn <- fmap getResponse $ run $ login Registration{newUserName="max", password="xam"}
      let maxGetTask :<|> maxNewTask :<|> _ :<|> _ = taskCrud $ Just tkn
      response <- run $ maxNewTask "fooz"
      assert "fooz" $ maxGetTask $ getResponse response
    ,testCase "duplicateTask" $ do
      tkn <- fmap getResponse $ run $ login Registration{newUserName="max", password="xam"}
      let _ :<|> maxNewTask :<|> _ :<|> _ = taskCrud $ Just tkn
      expect (checkHttpErr 409) $ maxNewTask "minutes"]
  ,testGroup "/profile tests"
    [testCase "updateProfile" $ do
      tkn <- fmap getResponse $ run $ login Registration{newUserName="max", password="xam"}
      let maxGetProfile :<|> maxUpdateProfile :<|> maxDeleteProfile = profileCrud $ Just tkn
      assert "max" maxGetProfile
      expect' (checkHttpErr 409) $ maxUpdateProfile "john"]
  ]
