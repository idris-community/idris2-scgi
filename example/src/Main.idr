module Main

import Data.Vect
import IO.Async.Loop.Epoll
import Network.SCGI
import HTTP.API
import User

%default total
%hide Data.Linear.(.)

SCGI : (ps : List Part) -> RequestPath (PartsTypes ps)
SCGI ps = Path ("scgi-example"::ps)

0 MyServer : APIs
MyServer =
  [ [SCGI ["inc", Capture Nat], GET [Text] String]
  , [SCGI ["add", Capture Nat], GET [Text] String]
  , [SCGI ["sum", Capture (List Nat)], GET [Text] String]
  , [SCGI ["hello", Capture String], GET [Text] String]
  , [SCGI ["login"], Query ["user" ?? String, "password" ?? String], GET [Text] String]
  , [SCGI ["users","add1"], Body [JSON] User, POST']
  , [SCGI ["users","add"], Body [JSON] (List User), POST']
  , [SCGI ["users"], GET [TSV,CSV,JSON] (List User)]
  ]

parameters {auto log : Logger}
           (tot      : IORef Nat)
           (users    : IORef (SnocList User))

  inc : Nat -> Handler String
  inc x = pure "New number: \{show $ x+1}\n"

  sum : List Nat -> Handler String
  sum xs = pure "Total: \{show $ Prelude.sum xs}\n"

  add : Nat -> Handler String
  add x = Prelude.do
    v <- lift1 $ modAndRead1 tot (+x)
    pure "Added \{show x}. New number is: \{show v}\n"

  hello : String -> Handler String
  hello s = pure "hello \{s}!\n"

  login : String -> String -> Handler String
  login "stefan" "hoeck" = pure "Authentication successful\n"
  login un       pw      = pure "Unknown user or invalid password\n"

  addUser : User -> Handler ()
  addUser u = mod users (:< u) >> info "user added"

  addUsers : List User -> Handler ()
  addUsers us = mod users (<>< us) >> info "\{show $ length us} users added"

  getUsers : Handler (List User)
  getUsers = (<>> []) <$> readref users

  server : Request -> SCGIProg [] Response
  server =
    serveAll MyServer
      [ inc
      , add
      , sum
      , hello
      , login
      , addUser
      , addUsers
      , getUsers
      ]

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
    use <- newref [<]
    serve local (server ref use)

covering
main : IO ()
main = epollApp prog
