{-# LANGUAGE OverloadedStrings #-}

module Main where

import DBus
import DBus.Client
import Data.Either (fromRight)
import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (listToMaybe)
import System.Environment (getEnvironment)
import Control.Concurrent.STM (TMVar, newEmptyTMVarIO, putTMVar, writeTMVar, takeTMVar, readTMVar, atomically)
import Control.Monad (join)

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

variantEnvToKV :: Either MethodError Variant -> Maybe [(String, String)]
variantEnvToKV evar = do
  var <- eitherToMaybe evar
  envList <- concat <$> (fromVariant var :: Maybe [String])
  return []

getFirstSignalArg :: IsVariant b => Signal -> Maybe b
getFirstSignalArg sig = (listToMaybe . signalBody $ sig) >>= fromVariant

waitForUnitExit :: TMVar String -> TMVar Bool -> String -> IO ()
waitForUnitExit sigVar reloadVar unit = join $ atomically $ do
  removedUnit <- takeTMVar sigVar
  isReloading <- readTMVar reloadVar
  if removedUnit == unit && not isReloading
    then return $ return ()
    else return $ waitForUnitExit sigVar reloadVar unit

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
