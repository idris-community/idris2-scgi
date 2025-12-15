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
  , [GET [Text] String, SCGI ["hello", Capture String]]
  ]

inc : Logger => HList [Nat] -> SCGIProg ServerErrs (HList [String])
inc [x] = pure ["New number: \{show $ x+1}\n"]

add : Logger => IORef Nat -> HList [Nat] -> SCGIProg ServerErrs (HList [String])
add ref [x] = Prelude.do
  v <- lift1 $ modAndRead1 ref (+x)
  pure ["Added \{show x}. New number is: \{show v}\n"]

hello : Logger => HList [String] -> SCGIProg ServerErrs (HList [String])
hello [s] = pure ["hello \{s}!\n"]

server : Logger => IORef Nat -> Request -> SCGIProg ServerErrs Response
server ref req = serveAll MyServer [inc, add ref, hello] req

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
