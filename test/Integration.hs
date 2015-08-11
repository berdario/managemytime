{-# LANGUAGE OverloadedStrings #-}

import Data.Either.Combinators (fromRight')
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (runEitherT, EitherT)
import Data.Text (unpack)
import System.Directory (doesFileExist, removeFile)
import Network.HTTP.Types (statusCode)
import Servant.API ((:<|>)(..))
import Servant.API.ResponseHeaders (getHeaders, getResponse)
import Servant.Common.Req (ServantError(..))
import Servant.Client (BaseUrl(..), Scheme(..), client)

import Test.Tasty (defaultMain, testGroup, TestName)
import Test.Tasty.HUnit (testCase, testCaseSteps, (@=?), Assertion, assertFailure)

import ManageMyTime (timeAPI)
import ManageMyTime.Models (User(..), Task(..), get, insert, fromSqlKey, toSqlKey, runDb, doMigrations, connectionString)
import ManageMyTime.Types (AuthLevel(..))

baseUrl = BaseUrl Http "localhost" 3000

getTask :<|> newTask :<|> updateTask :<|> deleteTask = taskCrud
getItem :<|> newItem :<|> updateItem :<|> deleteItem = itemCrud
getPreferredHours :<|> newPreferredHours :<|> updatePreferredHours :<|> deletePreferredHours = preferredHoursCrud
getProfile :<|> register :<|> updateProfile :<|> deleteProfile = profileCrud
getUser :<|> newUser :<|> updateUser :<|> deleteUser = userCrud

taskCrud :<|> itemCrud :<|> preferredHoursCrud :<|> profileCrud :<|> userCrud :<|> getTasks :<|> getItems :<|> getUsers = client timeAPI baseUrl

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
  johnid <-insert $ User "john" Normal "" Nothing
  fooid <- insert $ Task "foo" johnid
  liftIO $ putStrLn "fixture data OK"


taskTests = testGroup "/task tests"
  [testCase "getTask" $ assert "foo" $ getTask $ toSqlKey 1
  ,testCase "newTask" $ do
     response <- run $ newTask "fooz"
     assert "fooz" $ getTask $ getResponse response
  ,testCase "duplicateTask" $ do
     expect (checkHttpErr 409) $ newTask "foo"]
