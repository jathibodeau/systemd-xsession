{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar
import DBus
import DBus.Client
import Data.Either
import Data.Maybe
import System.Environment
import Text.Printf

xsessionTarget = "xsession.target" :: String

graphicalTarget = "graphical-session.target" :: String

sysdManagerPath = "/org/freedesktop/systemd1"

sysdManagerIface = "org.freedesktop.systemd1.Manager"

sysdManagerDest = "org.freedesktop.systemd1"

main :: IO ()
main = do
  sigVar <- newEmptyMVar
  client <- connectSession
  sysdEnvs <- getProperty client sysdGetEnvironment
  writeFile "/tmp/xsession.debug" $
    printf "Old systemd environment: %s\n" $ show sysdEnvs
  getEnvironment >>= call_ client . sysdSetEnvironment
  call_ client $ sysdUpdateEnvV sysdEnvs
  addMatch client unitRemovedMatch $ putMVar sigVar
  call_ client $ sysdSubscribe
  call_ client $ sysdStartUnit xsessionTarget
  waitForUnitExit sigVar xsessionTarget
  call_ client $ sysdStopUnit graphicalTarget
  return ()

waitForUnitExit :: MVar Signal -> String -> IO ()
waitForUnitExit sigVar unit = takeMVar sigVar >>= waitUnit sigVar unit
  where
    waitUnit sigVar unit sig
      | (getUnitArg sig >>= fromVariant) == Just unit = return ()
      | otherwise = waitForUnitExit sigVar unit
    getUnitArg = listToMaybe . signalBody

sysdManagerCall :: MemberName -> [Variant] -> MethodCall
sysdManagerCall member body =
  (methodCall sysdManagerPath sysdManagerIface member)
    {methodCallDestination = Just sysdManagerDest, methodCallBody = body}

sysdSubscribe :: MethodCall
sysdSubscribe = sysdManagerCall "Subscribe" []

sysdStartUnit :: String -> MethodCall
sysdStartUnit unit =
  sysdManagerCall "StartUnit" $ toVariant <$> [unit, "replace"]

sysdStopUnit :: String -> MethodCall
sysdStopUnit unit = sysdManagerCall "StopUnit" $ toVariant <$> [unit, "replace"]

sysdGetEnvironment :: MethodCall
sysdGetEnvironment = sysdManagerCall "Environment" []

sysdSetEnvironment :: [(String, String)] -> MethodCall
sysdSetEnvironment envs =
  sysdManagerCall
    "SetEnvironment"
    [toVariant $ (\(k, v) -> k ++ "=" ++ v) <$> envs]

sysdUpdateEnvV :: Either MethodError Variant -> MethodCall
sysdUpdateEnvV envs =
  sysdManagerCall
    "UnsetAndSetEnvironment"
    [toVariant ([] :: [String]), fromRight (toVariant ([] :: [String])) envs]

unitRemovedMatch :: MatchRule
unitRemovedMatch =
  matchAny
    { matchPath = Just sysdManagerPath
    , matchInterface = Just sysdManagerIface
    , matchMember = Just "UnitRemoved"
    }
