module Main

import Data.List
import Data.SortedMap
import Data.Vect
import IO.Async.Loop.Epoll
import Network.SCGI
import Network.SCGI.Error
import Network.SCGI.Logging
import HTTP.API.Serve
import HTTP.API.Path

%default total
%hide Data.Linear.(.)

0 MyServer : APIs
MyServer =
  [ [Path ["scgi-example", "inc", Capture Nat]]
  , [Path ["scgi-example", "hello"]]
  ]

doServe : Logger => HList [Nat] -> SCGIProg ServerErrs (HList [])
doServe [x] = info "Got a number: \{show x}" $> []

printHello : Logger => HList [] -> SCGIProg ServerErrs (HList [])
printHello [] = info "hello world!" $> []

server : Logger => Request -> SCGIProg ServerErrs Response
server = serveAll MyServer [doServe, printHello]

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
    let conf := {level := Info} (local @{console})
        log  := conf.logger
    traverse_ (\x => info x) (settings conf)
    serve conf server

covering
main : IO ()
main = epollApp prog
