module Main

import Book
import Data.Vect
import IO.Async.Console
import IO.Async.Loop.Epoll
import Network.SCGI
import HTTP.API.Server
import User

%default total
%hide Data.Linear.(.)

0 MyServer : Endpoints
MyServer =
  [ [SCGI ["inc", Capture Nat], Get [Text] String]
  , [SCGI ["add", Capture Nat], Get [Text] String]
  , [SCGI ["sum", Capture (List Nat)], Get [Text] String]
  , [SCGI ["hello", Capture String], Get [Text] String]
  , [SCGI ["login"], Query ["user" ?? String, "password" ?? String], Get [Text] String]
  , [SCGI ["users","add1"], JSONContent User, Post']
  , [SCGI ["users","add"], JSONContent (List User), Post']
  , [SCGI ["users"], Get [TSV,CSV,JSON] (List User)]
  ] ++ Books

parameters {auto log : HTTPLogger}
           (tot      : IORef Nat)
           (users    : IORef (SnocList User))
           (bks      : IORef (IDMap Book))

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

  server : Request -> Handler Response
  server =
    serveAll MyServer $ 
      [ inc
      , add
      , sum
      , hello
      , login
      , addUser
      , addUsers
      , getUsers
      ] ++ books bks

settings : Config -> List String
settings c =
  [ ""
  , "SCGI server ready"
  , "  Address:  \{show $ c.address}"
  , "  Port:     \{show c.port}"
  ]

covering
prog : HTTPProg [] ()
prog =
  use [stdOut] $ \[console] => Prelude.do
    let log := filter Info $ colorConsoleLogger console
    traverse_ (\x => info x) (settings local)
    ref <- newref Z
    use <- newref [<]
    bks <- newref empty
    serve local (server ref use bks)

covering
main : IO ()
main = epollApp prog
