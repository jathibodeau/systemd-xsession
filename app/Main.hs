{-# LANGUAGE OverloadedStrings #-}

module Main where

import DBus
    ( methodCall,
      MethodCall(methodCallBody, methodCallDestination),
      MethodError,
      Signal(signalBody),
      IsVariant(..),
      MemberName,
      Variant )
import DBus.Client
    ( addMatch,
      call_,
      connectSession,
      getProperty,
      matchAny,
      MatchRule(matchMember, matchPath, matchInterface) )
import Data.Either ( fromRight )
import Data.Maybe ( listToMaybe )
import System.Environment ( getEnvironment )
import Control.Concurrent.STM
    ( atomically,
      retry,
      newEmptyTMVarIO,
      putTMVar,
      readTMVar,
      takeTMVar,
      writeTMVar,
      TMVar )
import Control.Monad ( when )

xsessionTarget = "xsession.target" :: String

graphicalTarget = "graphical-session.target" :: String

sysdManagerPath = "/org/freedesktop/systemd1"

sysdManagerIface = "org.freedesktop.systemd1.Manager"

sysdManagerDest = "org.freedesktop.systemd1"

main :: IO ()
main = do
  sigVar <- newEmptyTMVarIO    :: IO (TMVar String)
  reloadVar <- newEmptyTMVarIO :: IO (TMVar Bool)
  client <- connectSession
  sysdEnvs <- getProperty client sysdGetEnvironment
  getEnvironment >>= call_ client . sysdSetEnvironment
  call_ client $ sysdUpdateEnvV sysdEnvs
  addMatch client unitRemovedMatch
    $ mapM_ (atomically . putTMVar sigVar) . getFirstSignalArg
  addMatch client sysdReloadMatch
    $ mapM_ (atomically . writeTMVar reloadVar) . getFirstSignalArg
  call_ client $ sysdSubscribe
  call_ client $ sysdStartUnit xsessionTarget
  waitForUnitExit sigVar reloadVar xsessionTarget
  call_ client $ sysdStopUnit graphicalTarget
  return ()

-- variantEnvToKV :: Either MethodError Variant -> Maybe [(String, String)]
-- variantEnvToKV evar = do
--   var <- eitherToMaybe evar
--   envList <- concat <$> (fromVariant var :: Maybe [String])
--   return []

getFirstSignalArg :: IsVariant b => Signal -> Maybe b
getFirstSignalArg sig = (listToMaybe . signalBody $ sig) >>= fromVariant

waitForUnitExit :: TMVar String -> TMVar Bool -> String -> IO ()
waitForUnitExit sigVar reloadVar unit = atomically $ do
  removedUnit <- takeTMVar sigVar
  isReloading <- readTMVar reloadVar
  when (removedUnit /= unit || isReloading) retry

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

sysdReloadMatch :: MatchRule
sysdReloadMatch =
  matchAny
    { matchPath = Just sysdManagerPath
    , matchInterface = Just sysdManagerIface
    , matchMember = Just "Reloading"
    }
