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
import Control.Monad 
import qualified Text.Read as R

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM
import PMS.Infra.Serial.DM.Type
import PMS.Infra.Serial.DM.Constant

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
errorToolsCallResponse :: String -> AppContext ()
errorToolsCallResponse errStr = do
  jsonRpc <- view jsonrpcAppData <$> ask
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
createSerialPort :: FilePath -> SerialPortSettings -> IO SerialPort
createSerialPort = openSerial

-- |
--   
readSizeSerialPort :: SerialPort -> Int -> IO B.ByteString
readSizeSerialPort serialp size = do
  msg <- recv serialp size
  if B.null msg
    then E.throwString "Connection closed by remote device"
    else do
      let hexMsg = bytesToHex msg
      hPutStrLn stderr $ "[DEBUG]PMS.Infra.SerialPort.DS.Utility.readSerialPort Received " ++ show (B.length msg) ++ " bytes: " ++ hexMsg
      return msg

-- |
--   
writeSerialPort :: SerialPort -> B.ByteString -> IO ()
writeSerialPort serialp bs = do
  hPutStrLn stderr $ "PMS.Infra.SerialPort.DS.Utility.writeSerialPort " ++ show bs
  _ <- send serialp bs
  flush serialp
 

-- |
--   
respondToIAC :: SerialPort -> B.ByteString -> IO ()
respondToIAC serialp bs = go bs
  where
    go s
      | B.length s < 3 = return ()
      | B.head s == 0xFF =
          let cmd  = B.index s 1
              opt  = B.index s 2
              rest = B.drop 3 s
              rsp  = case cmd of
                       0xFD -> iac <> wont <> B.singleton opt
                       0xFB -> iac <> dont <> B.singleton opt
                       _    -> B.empty
          in do
            let hexRsp = bytesToHex rsp
            hPutStrLn stderr $ "[DEBUG]PMS.Infra.SerialPort.DS.Utility.respondToIAC Sending response: " ++ hexRsp
            unless (B.null rsp) $ send serialp rsp >> return ()
            go rest
      | otherwise = go (B.tail s)


-- |
--   
readTelnetSerialPort :: SerialPort -> IO B.ByteString
readTelnetSerialPort serialp = do
  msg <- recv serialp 4096
  if B.null msg
    then E.throwString "Connection closed by remote device"
    else do
      let hexMsg = bytesToHex msg
      hPutStrLn stderr $ "[DEBUG]PMS.Infra.SerialPort.DS.Utility.readTelnetSerialPort Received " ++ show (B.length msg) ++ " bytes: " ++ hexMsg

      if 0xFF `B.elem` msg
        then do
          respondToIAC serialp msg
          readTelnetSerialPort serialp
        else
          return msg


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
  return defaultSerialSettings { commSpeed = cs }
