{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString             (ByteString, length)
import           Prelude                     hiding (length)
import           System.Environment          (getArgs)

import           Network.Wai.Handler.Warp    (defaultSettings, getPort,
                                              runSettings)
import           Network.Wai.Handler.WarpTLS (runTLS, tlsAllowedVersions,
                                              tlsCiphers, tlsSettingsMemory)

import           Servant.JS                  (jquery, writeJSForAPI)

import           ManageMyTime                (app, timeAPI)
import           ManageMyTime.Models         (doMigrations)

defaultTls = case length cert > 15 of
  True -> tlsSettingsMemory cert key
  False -> tlsSettingsMemory defaultCert defaultKey

latestTLS = head $ tlsAllowedVersions defaultTls
ciphers = take 2 $ tlsCiphers defaultTls -- take the first 2, currently the GCM ciphers
tlsSettings = defaultTls{tlsAllowedVersions=[latestTLS], tlsCiphers=ciphers}

tlsServer = do
  putStrLn $ "accepting " ++ show latestTLS ++ " with ciphers " ++ show ciphers
  runTLS tlsSettings defaultSettings app

testServer = runSettings defaultSettings app

main :: IO ()
main = do
  putStrLn "Writing javascript api to api.js, check its validity before shipping it"
  writeJSForAPI timeAPI jquery "api.js"
  doMigrations
  putStrLn $ "listening on " ++ show (getPort defaultSettings)
  args <- getArgs
  if args == ["test"] then testServer else tlsServer

cert :: ByteString
cert = "{{tls.cert}}"

key :: ByteString
key = "{{tls.key}}"

defaultCert :: ByteString
defaultCert = "-----BEGIN CERTIFICATE-----\nMIIEpDCCAowCCQCXup++VWXA6DANBgkqhkiG9w0BAQsFADAUMRIwEAYDVQQDDAls\nb2NhbGhvc3QwHhcNMTUwODA5MTMzNTMxWhcNMTUwOTA4MTMzNTMxWjAUMRIwEAYD\nVQQDDAlsb2NhbGhvc3QwggIiMA0GCSqGSIb3DQEBAQUAA4ICDwAwggIKAoICAQDa\napgFT7gXpGM6NxIGNmkH/vMKyU8XeoGiK8Tp2efJw+PTQ5jphvDs5FRTcqaXImSB\nTq92+cEOnT29+cNXdHyGzoE97xahWyDNjoGvnBgGaRi0EGCy0FAWCA+AnMRN5H7y\nEvvac9ivDRabF+8vKPT8tdbH5jJYzpWdCjzxE0XoBd9EGVnKRqUKlN7sAcOy32lT\niuq3X/8RiKWxtmCsUaPKVbiQinXaK6GYlk1tbKrLQ/qkQHWkFG0MpnDb4E0UqV3w\nxTfRSxN7DKFtK8V4oiY+UxrZEMNs0yodJOp8rD9OMZNj1eRmSsRqsyPgH0KPYy0h\nbHiUHFdHv1g4Ek8BmBZTdL1JcKOxeRwbp7FHqi2LvrtWj4EmjzKbmKGGj+NP+8Nr\nugD8Iwb6pOmihMxqhbWHos0w0lQn1xI4I3P06FFtts2+HK6SrjiBmSaAkzA3s8tT\nFYrhDAYdp/vUA50igUjouOG5YzjuFLUwudBryo6KSD3BxAJVprMMMnG1ckQz7S9e\nSS/ec/Gh3nMOlUAELUekecT4OrKSd33hWwxPPxJTs+/etCQgZmy3KJaCH6u2we+s\nQBewyZU7jX4+ZaXNXOBQvmEJm9X2msNEh9yyZf3LJAIOfDPhacs0rSYZ7gxKkau6\nUF2Rd+FDiY2Sdepnoz5MQtLntK86vCmQsKpB65p7QQIDAQABMA0GCSqGSIb3DQEB\nCwUAA4ICAQBUYsOH2to+R1z04nb5tkZtzGvUQ7PAXE7DkUd0CG+BaoNU5MYXNLsf\nL3jThBroLh0BP6joirJUYUeZwYq7qzuORhHWIG3Ci9oX70gN7d707IvJzCTFveyC\nW9NHmgGf702FC1/eoKei8FvIbMfwDFmJbcHi4qGHfRR03zvNwVo0vdx/YBOWlE+b\nqWLhX0OFNPImkGe9Jzcyj/Sz/TgHw2hTwv7X7w3Y/6o07FztjqCiNTdWZrUnu6Gg\nVHMO98U+/U1HJRhNvm/AotlLkUh3ZkpuXM61qD29BTD71Fbt9UMBqQnsaVnRSAQt\naHQfYQ5cbLLouBB0SxUcBK3CGJtn6ftWgJ5h+fjfh+HEJ7hVqYDEabEtz8cselDf\ndJxpAsS7zVlvv/JMmHS/9DehVi99zKRuT0zYPWle84J/LsPe5cC7z7irONYQwG57\n3G9QChltM5BS4xzbJnR19T2R3fdA/Vqa77rKVLBXs4drAdLLgaDK99TqqSgilUWt\nEGjmLMrTYaNvgZ8yUAEGXXgI2xBlOqySjXOsCkdz7YK1zIbyvOOTKLoFJh1rcEGZ\nkCoOM5keRESDBIRvxCgBG1X/qApkteTzsYi/CtjXx/ZZfSJbScX6WT/5JX3YFApZ\nau4AqWiSokiL9UgjSUsVoRnurZkXWpbxs+qkgwbi0UfEEBzUUc562g==\n-----END CERTIFICATE-----"

defaultKey :: ByteString
defaultKey = "-----BEGIN RSA PRIVATE KEY-----\n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\n-----END RSA PRIVATE KEY-----"
