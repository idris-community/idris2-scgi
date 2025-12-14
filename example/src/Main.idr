module Main

import Data.Vect
import IO.Async.Loop.Epoll
import Network.SCGI
import Network.SCGI.Logging

%default total

settings : Config -> List String
settings c =
  [ ""
  , "SCGI server ready"
  , "  Address:  \{show $ c.address}"
  , "  Port:     \{show c.port}"
  ]

server : Logger => Request -> SCGIProg ServerErrs Response
server r = Prelude.do
  info "Got a connection"
  pure (response1 [statusOK, plain] "hello SCGI-world\n")

covering
prog : SCGIProg [] ()
prog =
  use [stdOut] $ \[console] => Prelude.do
    let conf := local @{console}
        log  := conf.logger
    traverse_ (\x => info x) (settings conf)
    serve conf server

covering
main : IO ()
main = epollApp prog
