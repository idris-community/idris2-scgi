module Main

import Data.List
import Data.SortedMap
import Data.Vect
import IO.Async.Loop.Epoll
import Network.SCGI
import Network.SCGI.Logging

%default total
%hide Data.Linear.(.)

settings : Config -> List String
settings c =
  [ ""
  , "SCGI server ready"
  , "  Address:  \{show $ c.address}"
  , "  Port:     \{show c.port}"
  ]

dispURI : URI -> String
dispURI u =
  let p := fastConcat . intersperse "/" . map toString $ u.path
      q := fastConcat . intersperse "&" . map toQuery . kvList $ u.queries
   in "\{p}?\{q}"
  where
    toQuery : (ByteString,ByteString) -> String
    toQuery (x,y) = "\{toString x}=\{toString y}"

server : Logger => Request -> SCGIProg ServerErrs Response
server r = Prelude.do
  info "Got a connection at: \{dispURI r.uri}"
  trace "Headers:"
  for_ (kvList r.headers) $ \(h,v) => trace "\{toString h}: \{toString v}"
  pure (response1 [statusOK, plain] "hello SCGI-world\n")

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
