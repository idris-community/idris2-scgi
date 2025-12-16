module Main

import Data.List
import Data.SortedMap
import Data.Vect
import IO.Async.Loop.Epoll
import Network.SCGI
import Network.SCGI.Error
import Network.SCGI.Logging
import HTTP.API

%default total
%hide Data.Linear.(.)

SCGI : (ps : List Part) -> RequestPath (PartsTypes ps)
SCGI ps = Path ("scgi-example"::ps)

0 MyServer : APIs
MyServer =
  [ [GET [Text] String, SCGI ["inc", Capture Nat]]
  , [GET [Text] String, SCGI ["add", Capture Nat]]
  , [GET [Text] String, SCGI ["sum", Capture (List Nat)]]
  , [GET [Text] String, SCGI ["hello", Capture String]]
  ]

parameters {auto log : Logger}
  inc : Nat -> SCGIProg ServerErrs String
  inc x = pure "New number: \{show $ x+1}\n"

  sum : List Nat -> SCGIProg ServerErrs String
  sum xs = pure "Total: \{show $ Prelude.sum xs}\n"

  add : IORef Nat -> Nat -> SCGIProg ServerErrs String
  add ref x = Prelude.do
    v <- lift1 $ modAndRead1 ref (+x)
    pure "Added \{show x}. New number is: \{show v}\n"

  hello : String -> SCGIProg ServerErrs String
  hello s = pure "hello \{s}!\n"

  server : IORef Nat -> Request -> SCGIProg ServerErrs Response
  server ref = serveAll MyServer [inc, add ref, sum, hello]

settings : Config -> List String
settings c =
  [ ""
  , "SCGI server ready"
  , "  Address:  \{show $ c.address}"
  , "  Port:     \{show c.port}"
  ]

covering
prog : SCGIProg [] ()
prog =
  use [stdOut] $ \[console] => Prelude.do
    let log := filter Info $ colorConsoleLogger console
    traverse_ (\x => info x) (settings local)
    ref <- newref Z
    serve local (server ref)

covering
main : IO ()
main = epollApp prog
