{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module PMS.Infra.Serial.DS.Core where

import System.IO
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent as CC 
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import Data.Conduit
import qualified Data.Text as T
import Control.Monad.Except
import qualified Control.Exception.Safe as E
import System.Exit
import qualified Data.Text.Encoding as TE
import Data.Aeson 
import qualified Data.ByteString.Char8 as BS8
import Data.Word 
import qualified Data.ByteString as B
import System.Hardware.Serialport

import qualified PMS.Domain.Model.DS.Utility as DM
import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Infra.Serial.DM.Type
import PMS.Infra.Serial.DS.Utility


-- |
--
app :: AppContext ()
app = do
  $logDebugS DM._LOGTAG "app called."
  runConduit pipeline
  where
    pipeline :: ConduitM () Void AppContext ()
    pipeline = src .| cmd2task .| sink

---------------------------------------------------------------------------------
-- |
--
src :: ConduitT () DM.SerialCommand AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext DM.SerialCommand
    go = do
      queue <- view DM.serialQueueDomainData <$> lift ask
      dat <- liftIO $ STM.atomically $ STM.readTQueue queue
      return dat

---------------------------------------------------------------------------------
-- |
--
cmd2task :: ConduitT DM.SerialCommand (IOTask ()) AppContext ()
cmd2task = await >>= \case
  Just cmd -> flip catchError (errHdl cmd) $ do
    lift (go cmd) >>= yield >> cmd2task
  Nothing -> do
    $logWarnS DM._LOGTAG "cmd2task: await returns nothing. skip."
    cmd2task

  where
    errHdl :: DM.SerialCommand -> String -> ConduitT DM.SerialCommand (IOTask ()) AppContext ()
    errHdl serialCmd msg = do
      let jsonrpc = DM.getJsonRpcSerialCommand serialCmd
      $logWarnS DM._LOGTAG $ T.pack $ "cmd2task: exception occurred. skip. " ++ msg
      lift $ errorToolsCallResponse jsonrpc $ "cmd2task: exception occurred. skip. " ++ msg
      cmd2task

    go :: DM.SerialCommand -> AppContext (IOTask ())
    go (DM.SerialEchoCommand dat)    = genEchoTask dat
    go (DM.SerialOpenCommand dat)    = genSerialOpenTask dat
    go (DM.SerialCloseCommand dat)   = genSerialCloseTask dat
    go (DM.SerialReadCommand dat)    = genSerialReadTask dat
    go (DM.SerialWriteCommand dat)   = genSerialWriteTask dat
    go (DM.SerialMessageCommand dat) = genSerialMessageTask dat

---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT (IOTask ()) Void AppContext ()
sink = await >>= \case
  Just req -> flip catchError errHdl $ do
    lift (go req) >> sink
  Nothing -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    errHdl :: String -> ConduitT (IOTask ()) Void AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "sink: exception occurred. skip. " ++ msg
      sink

    go :: (IO ()) -> AppContext ()
    go task = do
      $logDebugS DM._LOGTAG "sink: start async."
      _ <- liftIOE $ async task
      $logDebugS DM._LOGTAG "sink: end async."
      return ()


---------------------------------------------------------------------------------
-- |
--
genEchoTask :: DM.SerialEchoCommandData -> AppContext (IOTask ())
genEchoTask dat = do
  resQ <- view DM.responseQueueDomainData <$> lift ask
  let val = dat^.DM.valueSerialEchoCommandData

  $logDebugS DM._LOGTAG $ T.pack $ "echoTask: echo : " ++ val
  return $ echoTask resQ dat val


-- |
--
echoTask :: STM.TQueue DM.McpResponse -> DM.SerialEchoCommandData -> String -> IOTask ()
echoTask resQ cmdDat val = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infra.Serial.DS.Core.echoTask run. " ++ val

  toolsCallResponse resQ (cmdDat^.DM.jsonrpcSerialEchoCommandData) ExitSuccess val ""

  hPutStrLn stderr "[INFO] PMS.Infra.Serial.DS.Core.echoTask end."

  where
    errHdl :: E.SomeException -> IO ()
    errHdl e = toolsCallResponse resQ (cmdDat^.DM.jsonrpcSerialEchoCommandData) (ExitFailure 1) "" (show e)

-- |
--
genSerialOpenTask :: DM.SerialOpenCommandData -> AppContext (IOTask ())
genSerialOpenTask cmdDat = do
  let argsBS   = DM.unRawJsonByteString $ cmdDat^.DM.argumentsSerialOpenCommandData

  resQ <- view DM.responseQueueDomainData <$> lift ask
  serialMVar <- view handleAppData <$> ask
  prompts <- view DM.promptsDomainData <$> lift ask
  lockTMVar <- view lockAppData <$> ask
  argsDat <- liftEither $ eitherDecode $ argsBS


  let device = argsDat^.deviceSerialToolParams
      speed  = argsDat^.speedSerialToolParams

  settings <- getSerialSetting speed
  $logDebugS DM._LOGTAG $ T.pack $ "genSerialOpenTask: cmd. " ++ device ++ ":" ++ show settings
 
  return $ serialOpenTask cmdDat resQ serialMVar lockTMVar prompts device settings


-- |
--   
serialOpenTask :: DM.SerialOpenCommandData
              -> STM.TQueue DM.McpResponse
              -> STM.TMVar (Maybe Handle)
              -> STM.TMVar ()  -- lock
              -> [String]      -- prompt list
              -> String        -- device
              -> SerialPortSettings
              -> IOTask ()
serialOpenTask cmdDat resQ serialVar lockTMVar prompts device settings = do
  hPutStrLn stderr $ "[INFO] PMS.Infra.Serial.DS.Core.serialOpenTask start. "
  let waitmicro = 100000
      readsize = 1024
      tout = DM._TIMEOUT_MICROSEC
      promptAdded = prompts ++ ["login:"]
      jsonRpc = cmdDat^.DM.jsonrpcSerialOpenCommandData

  STM.atomically (STM.takeTMVar serialVar) >>= \case
    Just p -> do
      STM.atomically $ STM.putTMVar serialVar $ Just p
      hPutStrLn stderr "[ERROR] PMS.Infrastructure.DS.Core.serialOpenTask: serial is already connected."
      toolsCallResponse resQ (cmdDat^.DM.jsonrpcSerialOpenCommandData) (ExitFailure 1) "" "serial is already running."
    Nothing -> flip E.catchAny errHdl $ do
      hPutStrLn stderr $ "[INFO] PMS.Infra.Serial.DS.Core.serialOpenTask end." ++ (show settings)
      serialhdl <- createSerialPort device settings
      writeSerialPort serialhdl $ BS8.pack $ DM._CR
      CC.threadDelay waitmicro

      STM.atomically $ STM.putTMVar serialVar (Just serialhdl)
      hPutStrLn stderr $ "[INFO] PMS.Infra.Serial.DS.Core.serialOpenTask end. serial connected to " ++ device ++ ":" ++ show settings

  STM.atomically (STM.readTMVar serialVar) >>= \case
    Nothing -> do
      hPutStrLn stderr "[ERROR] PMS.Infra.Serial.DS.Core.serialOpenTask: serial is not started."
      toolsCallResponse resQ jsonRpc (ExitFailure 1) "" "serial is not started."
    Just serialhdl -> do
      race (DM.expect lockTMVar (readSizeSerialPort serialhdl readsize) promptAdded) (CC.threadDelay tout) >>= \case
        Left res -> toolsCallResponse resQ jsonRpc ExitSuccess (maybe "Nothing" id res) ""
        Right _  -> toolsCallResponse resQ jsonRpc (ExitFailure 1) "" "timeout occurred."

  hPutStrLn stderr "[INFO] PMS.Infra.Serial.DS.Core.serialOpenTask end."

  where
    errHdl :: E.SomeException -> IO ()
    errHdl e = do
      STM.atomically $ STM.putTMVar serialVar Nothing
      hPutStrLn stderr $ "[ERROR] PMS.Infra.Serial.DS.Core.serialRunTask: exception occurred. " ++ show e
      toolsCallResponse resQ (cmdDat^.DM.jsonrpcSerialOpenCommandData) (ExitFailure 1) "" (show e)

-- |
--
genSerialCloseTask :: DM.SerialCloseCommandData -> AppContext (IOTask ())
genSerialCloseTask dat = do
  $logDebugS DM._LOGTAG $ T.pack $ "genSerialCloseTask called. "
  serialTMVar <- view handleAppData <$> ask
  resQ <- view DM.responseQueueDomainData <$> lift ask
  return $ serialCloseTask dat resQ serialTMVar

-- |
--
serialCloseTask :: DM.SerialCloseCommandData
                  -> STM.TQueue DM.McpResponse
                  -> STM.TMVar (Maybe Handle)
                  -> IOTask ()
serialCloseTask cmdDat resQ serialTMVar = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infra.Serial.DS.Core.serialCloseTask run. "
  let jsonRpc = cmdDat^.DM.jsonrpcSerialCloseCommandData

  STM.atomically (STM.swapTMVar serialTMVar Nothing) >>= \case
    Nothing -> do
      hPutStrLn stderr "[ERROR] PMS.Infra.Serial.DS.Core.serialCloseTask: serial is not started."
      toolsCallResponse resQ jsonRpc (ExitFailure 1) "" "serial is not started."
    Just serialhdl -> do
      hClose serialhdl
      toolsCallResponse resQ jsonRpc ExitSuccess "" "serial is teminated."
      hPutStrLn stderr $ "[INFO] PMS.Infra.Serial.DS.Core.serialCloseTask closeSerial : "

  hPutStrLn stderr "[INFO] PMS.Infra.Serial.DS.Core.serialCloseTask end."

  where
    -- |
    --
    errHdl :: E.SomeException -> IO ()
    errHdl e = toolsCallResponse resQ (cmdDat^.DM.jsonrpcSerialCloseCommandData) (ExitFailure 1) "" (show e)


-- |
--
genSerialReadTask :: DM.SerialReadCommandData -> AppContext (IOTask ())
genSerialReadTask cmdData = do
  let argsBS = DM.unRawJsonByteString $ cmdData^.DM.argumentsSerialReadCommandData
      tout = 30 * 1000 * 1000
  resQ <- view DM.responseQueueDomainData <$> lift ask
  serialTMVar <- view handleAppData <$> ask
  argsDat <- liftEither $ eitherDecode $ argsBS
  let size = argsDat^.sizeSerialIntToolParams

  $logDebugS DM._LOGTAG $ T.pack $ "genSerialReadTask: args. " ++ show size
  return $ serialReadTask cmdData resQ serialTMVar size tout

-- |
--
serialReadTask :: DM.SerialReadCommandData
                -> STM.TQueue DM.McpResponse
                -> STM.TMVar (Maybe Handle)
                -> Int       -- read size.
                -> Int       -- timeout microsec
                -> IOTask ()
serialReadTask cmdDat resQ serialTMVar size tout = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infra.Serial.DS.Core.serialReadTask run. " ++ show size
    
  STM.atomically (STM.readTMVar serialTMVar) >>= \case
    Nothing -> do
      hPutStrLn stderr "[ERROR] PMS.Infra.Serial.DS.Core.serialReadTask: serial is not started."
      toolsCallResponse resQ jsonRpc (ExitFailure 1) "" "serial is not started."
    Just p -> go p

  hPutStrLn stderr "[INFO] PMS.Infra.Serial.DS.Core.serialReadTask end."

  where
    jsonRpc :: DM.JsonRpcRequest
    jsonRpc = cmdDat^.DM.jsonrpcSerialReadCommandData

    errHdl :: E.SomeException -> IO ()
    errHdl e = toolsCallResponse resQ jsonRpc (ExitFailure 1) "" (show e)

    go :: Handle -> IO ()
    go serialhdl =
      race (readSizeSerialPort serialhdl size) (CC.threadDelay tout) >>= \case
        Left res -> toolsCallResponse resQ jsonRpc ExitSuccess (bytesToHex res) ""
        Right _  -> toolsCallResponse resQ jsonRpc (ExitFailure 1) "" "timeout occurred."


-- |
--
genSerialWriteTask :: DM.SerialWriteCommandData -> AppContext (IOTask ())
genSerialWriteTask cmdData = do
  let argsBS = DM.unRawJsonByteString $ cmdData^.DM.argumentsSerialWriteCommandData
  resQ <- view DM.responseQueueDomainData <$> lift ask
  serialTMVar <- view handleAppData <$> ask
  argsDat <- liftEither $ eitherDecode $ argsBS
  let args = argsDat^.dataSerialWord8ArrayToolParams

  $logDebugS DM._LOGTAG $ T.pack $ "genSerialWriteTask: args. " ++ show args
  return $ serialWriteTask cmdData resQ serialTMVar args

-- |
--
serialWriteTask :: DM.SerialWriteCommandData
                -> STM.TQueue DM.McpResponse
                -> STM.TMVar (Maybe Handle)
                -> [Word8]
                -> IOTask ()
serialWriteTask cmdDat resQ serialTMVar args = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infra.Serial.DS.Core.serialWriteTask run. " ++ show args
    
  STM.atomically (STM.readTMVar serialTMVar) >>= \case
    Nothing -> do
      hPutStrLn stderr "[ERROR] PMS.Infra.Serial.DS.Core.serialWriteTask: serial is not started."
      toolsCallResponse resQ jsonRpc (ExitFailure 1) "" "serial is not started."
    Just p -> go p

  hPutStrLn stderr "[INFO] PMS.Infra.Serial.DS.Core.serialWriteTask end."

  where
    jsonRpc :: DM.JsonRpcRequest
    jsonRpc = cmdDat^.DM.jsonrpcSerialWriteCommandData

    errHdl :: E.SomeException -> IO ()
    errHdl e = toolsCallResponse resQ jsonRpc (ExitFailure 1) "" (show e)

    go :: Handle -> IO ()
    go serialhdl = do
      let bsDat = B.pack args

      hPutStrLn stderr $ "[INFO] PMS.Infra.Serial.DS.Core.serialWriteTask writeSerial (hex): " ++ bytesToHex bsDat

      writeSerialPort serialhdl bsDat

      toolsCallResponse resQ jsonRpc ExitSuccess ("write data to serial. "++ bytesToHex bsDat) ""


-- |
--
genSerialMessageTask :: DM.SerialMessageCommandData -> AppContext (IOTask ())
genSerialMessageTask cmdData = do
  let argsBS = DM.unRawJsonByteString $ cmdData^.DM.argumentsSerialMessageCommandData
      tout = DM._TIMEOUT_MICROSEC
  prompts <- view DM.promptsDomainData <$> lift ask
  resQ <- view DM.responseQueueDomainData <$> lift ask
  serialTMVar <- view handleAppData <$> ask
  lockTMVar <- view lockAppData <$> ask
  argsDat <- liftEither $ eitherDecode $ argsBS
  let args = argsDat^.argumentsSerialStringToolParams

  $logDebugS DM._LOGTAG $ T.pack $ "genSerialMessageTask: args. " ++ args
  return $ serialMessageTask cmdData resQ serialTMVar lockTMVar args prompts tout

-- |
--
serialMessageTask :: DM.SerialMessageCommandData
                -> STM.TQueue DM.McpResponse
                -> STM.TMVar (Maybe Handle)
                -> STM.TMVar ()
                -> String  -- arguments line
                -> [String]  -- prompt list
                -> Int       -- timeout microsec
                -> IOTask ()
serialMessageTask cmdDat resQ serialTMVar lockTMVar args prompts tout = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infra.Serial.DS.Core.serialMessageTask run. " ++ args
    
  STM.atomically (STM.readTMVar serialTMVar) >>= \case
    Nothing -> do
      hPutStrLn stderr "[ERROR] PMS.Infra.Serial.DS.Core.serialMessageTask: serial is not started."
      toolsCallResponse resQ jsonRpc (ExitFailure 1) "" "serial is not started."
    Just p -> go p

  hPutStrLn stderr "[INFO] PMS.Infra.Serial.DS.Core.serialMessageTask end."

  where
    jsonRpc :: DM.JsonRpcRequest
    jsonRpc = cmdDat^.DM.jsonrpcSerialMessageCommandData

    errHdl :: E.SomeException -> IO ()
    errHdl e = toolsCallResponse resQ jsonRpc (ExitFailure 1) "" (show e)

    go :: Handle -> IO ()
    go serialhdl = do

      msg <- DM.validateMessage args
      let cmd = TE.encodeUtf8 $ T.pack $ msg ++ DM._CR
          waitmicro = 100000
          size = 1024
      hPutStrLn stderr $ "[INFO] PMS.Infra.Serial.DS.Core.serialMessageTask writeSerial : " ++ BS8.unpack cmd

      writeSerialPort serialhdl cmd
      CC.threadDelay waitmicro
      race (DM.expect lockTMVar (readSizeSerialPort serialhdl size) prompts) (CC.threadDelay tout) >>= \case
        Left res -> toolsCallResponse resQ jsonRpc ExitSuccess (maybe "Nothing" id res) ""
        Right _  -> toolsCallResponse resQ jsonRpc (ExitFailure 1) "" "timeout occurred."


