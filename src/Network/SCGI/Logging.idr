module Network.SCGI.Logging

import Derive.Prelude
import Network.SCGI.Prog
import Network.SCGI.Service
import Text.ANSI

%default total
%language ElabReflection

public export
data LogLevel = Trace | Debug | Info | Warning | Error

%runElab derive "LogLevel" [Show,Eq,Ord]

export
Interpolation LogLevel where interpolate = toLower . show

--------------------------------------------------------------------------------
--          ConsoleOut
--------------------------------------------------------------------------------

||| Record representing a console with
||| standard output and error output
public export
record ConsoleOut where
  constructor MkConsoleOut
  close_    : SCGIProg [] ()
  putStr_   : String -> SCGIProg [] ()
  putErr_   : String -> SCGIProg [] ()

export %inline
Resource SCGIProg ConsoleOut where cleanup = close_

parameters {default 100 capacity : Nat}

  ||| Creates a console for writing messages and errors to.
  |||
  ||| To make this available to many fibers, this is run as a service
  ||| in the background using an internal buffer that can hold up to
  ||| `capacity` messages.
  export covering
  console : (putstr, puterr : String -> SCGIProg [] ()) -> SCGIProg es ConsoleOut
  console putstr puterr = do
    srv <- stateless (const ()) putPair
    pure $ MkConsoleOut (cleanup srv) (send srv . (True,)) (send srv . (False,))

    where
      putPair : (Bool,String) -> SCGIProg [] ()
      putPair (True,s)  = putstr s
      putPair (False,s) = puterr s

  ||| The default console, printing to standard out and standard err.
  |||
  ||| Note: Since many fibers might be writing to the console at the same
  |||       this uses a bounded channel with a buffer of the given
  |||       capacity internally.
  export covering
  stdOut : SCGIProg es ConsoleOut
  stdOut = console stdout stderr

parameters {auto con : ConsoleOut}

  ||| Put a string to the console's standard output.
  export %inline
  cputStr : String -> SCGIProg es ()
  cputStr s = widenErrors $ con.putStr_ s
  
  ||| Put a string plus trailing line break
  ||| to the console's standard output.
  export %inline
  cputStrLn : String -> SCGIProg es ()
  cputStrLn s = cputStr $ s ++ "\n"
  
  ||| Print a value to the console's standard output.
  export %inline
  cprint : Show a => a -> SCGIProg es ()
  cprint = cputStr . show
  
  ||| Print a value plus trailing lne break
  ||| to the console's standard output.
  export
  cprintLn : Show a => a -> SCGIProg es ()
  cprintLn = cputStrLn . show
  
  ||| Put a string to the console's error output.
  export
  cputErr : String -> SCGIProg es ()
  cputErr s = widenErrors $ con.putErr_ s
  
  ||| Put a string plus trailing line break
  ||| to the console's error output.
  export
  cputErrLn : String -> SCGIProg es ()
  cputErrLn s = cputErr $ s ++ "\n"
  
  ||| Print a value to the console's error output.
  export
  cprintErr : Show a => a -> SCGIProg es ()
  cprintErr = cputErr . show
  
  ||| Print a value plus trailing lne break
  ||| to the console's error output.
  export
  cprintErrLn : Show a => a -> SCGIProg es ()
  cprintErrLn = cputErrLn . show

--------------------------------------------------------------------------------
--          Record
--------------------------------------------------------------------------------

public export
record Logger where
  constructor MkLogger
  log : LogLevel -> Lazy String -> SCGIProg [] ()

||| Only log message of at least the given logging level.
export
filter : LogLevel -> Logger -> Logger
filter lvl x = MkLogger $ \l,s => case l >= lvl of
  True  => x.log l s
  False => pure ()

export
Semigroup Logger where
  x <+> y = MkLogger $ \l,s => x.log l s >> y.log l s

export
Monoid Logger where
  neutral = MkLogger $ \_,_ => pure ()

export
consoleLogger : ConsoleOut -> (LogLevel -> Lazy String -> String) -> Logger
consoleLogger c f =
  MkLogger $ \l,s => case l of
    Error => c.putErr_ (f l s ++ "\n")
    _     => c.putStr_ (f l s ++ "\n")

export
basicConsoleLogger : ConsoleOut -> Logger
basicConsoleLogger c = consoleLogger c $ \l,s => "[\{l}] \{s}"

col : LogLevel -> String
col Trace   = show $ colored White "trace"
col Debug   = show $ colored Cyan "debug"
col Info    = show $ colored Green "info"
col Warning = show $ colored Yellow "warning"
col Error   = show $ colored Red "error"

space : LogLevel -> String
space Trace   = "   "
space Debug   = "   "
space Info    = "    "
space Warning = " "
space Error   = "   "

||| A console logger with colored log level tags
export
colorConsoleLogger : ConsoleOut -> Logger
colorConsoleLogger c =
  consoleLogger c $ \l,s => "[\{col l}]\{space l}\{s}"

--------------------------------------------------------------------------------
--          Syslog
--------------------------------------------------------------------------------

severity : LogLevel -> Nat
severity Trace   = 7
severity Debug   = 7
severity Info    = 6
severity Warning = 5
severity Error   = 3

||| A logger using syslog priority codes. This can be used with
||| systemd services.
export
syslogLogger : ConsoleOut -> Logger
syslogLogger c = consoleLogger c $ \l,s => "<\{show $ severity l}> \{s}"

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

parameters {auto lg  : Logger}

  export
  log : LogLevel -> Lazy String -> SCGIProg es ()
  log l s = weakenErrors $ lg.log l s
  
  export %inline
  trace : Lazy String -> SCGIProg es ()
  trace = log Trace
  
  export %inline
  debug : Lazy String -> SCGIProg es ()
  debug = log Debug
  
  export %inline
  info : Lazy String -> SCGIProg es ()
  info = log Info
  
  export %inline
  warn : Lazy String -> SCGIProg es ()
  warn = log Warning
  
  export %inline
  error : Lazy String -> SCGIProg es ()
  error = log Error
  
  export %inline
  ierror : Interpolation a => a -> SCGIProg es ()
  ierror x = error (interpolate x)
