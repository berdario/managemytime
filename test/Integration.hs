{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Either (runEitherT, EitherT)
import Servant.API ((:<|>)(..))
import Servant.Common.Req (ServantError)
import Servant.Client (BaseUrl(..), Scheme(..), client)

import Test.Tasty (defaultMain, testGroup, TestName)
import Test.Tasty.HUnit (testCase, (@=?), Assertion, assertFailure)

import ManageMyTime (timeAPI, Task(..))

baseUrl = BaseUrl Http "localhost" 3000

getTask :<|> newTask :<|> updateTask :<|> deleteTask = taskCrud
getItem :<|> newItem :<|> updateItem :<|> deleteItem = itemCrud
getPreferredHours :<|> newPreferredHours :<|> updatePreferredHours :<|> deletePreferredHours = preferredHoursCrud
getProfile :<|> register :<|> updateProfile :<|> deleteProfile = profileCrud
getUser :<|> newUser :<|> updateUser :<|> deleteUser = userCrud

taskCrud :<|> itemCrud :<|> preferredHoursCrud :<|> profileCrud :<|> userCrud :<|> getTasks :<|> getItems :<|> getUsers = client timeAPI baseUrl

assert :: (Eq a, Show a) => a -> EitherT ServantError IO a -> Assertion
assert a b = do
  b'<- runEitherT b
  either (assertFailure . show) (a @=?) b'

main :: IO ()
main = defaultMain taskTests

taskTests = testGroup "/task tests"
  [testCase "getTask" $ assert (Task "foo") $ getTask 1]
