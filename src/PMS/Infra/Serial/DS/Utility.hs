{-# LANGUAGE OverloadedStrings #-}

module PMS.Infra.Serial.DS.Utility where

import Control.Lens
import System.Exit
import System.IO
import System.Log.FastLogger
import qualified Control.Exception.Safe as E
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import System.Hardware.Serialport
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import qualified Text.Read as R

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM
import PMS.Infra.Serial.DM.Type

-- |
--
runApp :: DM.DomainData -> AppData -> TimedFastLogger -> AppContext a -> IO (Either DM.ErrorData a)
runApp domDat appDat logger ctx =
  DM.runFastLoggerT domDat logger
    $ runExceptT
    $ flip runReaderT domDat
    $ runReaderT ctx appDat


-- |
--
liftIOE :: IO a -> AppContext a
liftIOE f = liftIO (go f) >>= liftEither
  where
    go :: IO b -> IO (Either String b)
    go x = E.catchAny (Right <$> x) errHdl

    errHdl :: E.SomeException -> IO (Either String a)
    errHdl = return . Left . show

---------------------------------------------------------------------------------
-- |
--
toolsCallResponse :: STM.TQueue DM.McpResponse
                  -> DM.JsonRpcRequest
                  -> ExitCode
                  -> String
                  -> String
                  -> IO ()
toolsCallResponse resQ jsonRpc code outStr errStr = do
  let content = [ DM.McpToolsCallResponseResultContent "text" outStr
                , DM.McpToolsCallResponseResultContent "text" errStr
                ]
      result = DM.McpToolsCallResponseResult {
                  DM._contentMcpToolsCallResponseResult = content
                , DM._isErrorMcpToolsCallResponseResult = (ExitSuccess /= code)
                }
      resDat = DM.McpToolsCallResponseData jsonRpc result
      res = DM.McpToolsCallResponse resDat

  STM.atomically $ STM.writeTQueue resQ res

-- |
--
errorToolsCallResponse :: DM.JsonRpcRequest -> String -> AppContext ()
errorToolsCallResponse jsonRpc errStr = do
  let content = [ DM.McpToolsCallResponseResultContent "text" errStr ]
      result = DM.McpToolsCallResponseResult {
                  DM._contentMcpToolsCallResponseResult = content
                , DM._isErrorMcpToolsCallResponseResult = True
                }
      resDat = DM.McpToolsCallResponseData jsonRpc result
      res = DM.McpToolsCallResponse resDat

  resQ <- view DM.responseQueueDomainData <$> lift ask
  liftIOE $ STM.atomically $ STM.writeTQueue resQ res

-- |
--   
bytesToHex :: B.ByteString -> String
bytesToHex = C8.unpack . B16.encode


-- |
--   
createSerialPort :: FilePath -> SerialPortSettings -> IO Handle
createSerialPort = hOpenSerial

-- |
--   
readSizeSerialPort :: Handle -> Int -> IO B.ByteString
readSizeSerialPort = B.hGetSome

-- |
--   
readLineSerialPort :: Handle -> IO B.ByteString
readLineSerialPort = C8.hGetLine


-- |
--   
writeSerialPort :: Handle -> B.ByteString -> IO ()
writeSerialPort serialhdl bs = do
  B.hPut serialhdl bs
  hFlush serialhdl

 

-- |
--   
getCommSpeed :: Int -> AppContext CommSpeed
getCommSpeed n = case R.readMaybe ("CS" ++ show n) of
  Just cs -> return cs
  Nothing -> throwError $ "invalid comm speed: " ++ show n

-- |
-- 
getSerialSetting :: Int -> AppContext SerialPortSettings
getSerialSetting speed = do
  cs <- getCommSpeed speed
  return defaultSerialSettings {
      commSpeed = cs
    , timeout = 1
    }
