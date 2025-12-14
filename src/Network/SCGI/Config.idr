module Network.SCGI.Config

import Data.ByteString
import Data.Nat
import Data.Vect
import Network.SCGI.Error
import Network.SCGI.Logging

%default total

--------------------------------------------------------------------------------
--          Config
--------------------------------------------------------------------------------

public export
record Config where
  [noHints]
  constructor C
  ||| Address to which the server socket should be bound
  address             : Vect 4 Bits8

  ||| Port to use for the server
  port                : Bits16

  ||| Maximum number of bytes allowed in SCGI-message header 
  maxHeaderSize       : Nat

  ||| Maximum number of bytes allowed in the message content
  maxMsgSize          : Nat

  ||| Number of concurrent requests
  concurrent          : Nat

  ||| Logging facility
  baseLogger          : Logger

  ||| Minimal log level
  level               : LogLevel

  {auto 0 prf         : IsSucc concurrent}

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

||| Default config for testing the cyby server locally
export
local : (lg : ConsoleOut) => Config
local = C {
    address        = [0,0,0,0]
  , port           = 4000
  , maxMsgSize     = 0xffffff
  , maxHeaderSize  = 0xfff0
  , concurrent     = 10
  , baseLogger     = colorConsoleLogger lg
  , level          = Info
  }

export
(.logger) : Config -> Logger
c.logger = filter c.level c.baseLogger
