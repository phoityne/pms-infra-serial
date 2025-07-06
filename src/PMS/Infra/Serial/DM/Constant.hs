module PMS.Infra.Serial.DM.Constant where

import qualified Data.ByteString as B
import Data.Word

--------------------------------------------------------------------------------
-- |
--
_LOG_FILE_NAME :: String
_LOG_FILE_NAME = "pms-infra-serial.log"

iac :: B.ByteString
iac = B.singleton (255 :: Word8)

will :: B.ByteString
will = B.singleton (251 :: Word8)

wont :: B.ByteString
wont = B.singleton (252 :: Word8)

do_ :: B.ByteString
do_ = B.singleton (253 :: Word8)

dont :: B.ByteString
dont = B.singleton (254 :: Word8)

telopt_echo :: B.ByteString
telopt_echo = B.singleton (1 :: Word8)

telopt_suppress_ga :: B.ByteString
telopt_suppress_ga = B.singleton (3 :: Word8)

telopt_ttype :: B.ByteString
telopt_ttype = B.singleton (24 :: Word8)

telopt_naws :: B.ByteString
telopt_naws = B.singleton (31 :: Word8)

telopt_new_environ :: B.ByteString
telopt_new_environ = B.singleton (39 :: Word8)
