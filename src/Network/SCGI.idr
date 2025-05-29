||| Copyright (c) 2020 - 2025 Center for Organic and Medicinal Chemistry
|||               Zurich University of Applied Sciences
|||               Wädenswil, Switzerland
|||
||| The CyBy application consists of a web client running in the
||| browsers of CyBy's users, and a server application dealing
||| with the application logic.
|||
||| However, the CyBy server is not a full-fledged web server:
||| Serving static content such as CyBy's start page, JS-script,
||| and images is handled by HTTP server such as
||| [Apache HTTP Server](https://httpd.apache.org/) or [nginx](https://nginx.org/en/).
||| In addition, the HTTP server takes care of security-related stuff
||| like serving the CyBy web page over an encrypted connection.
|||
||| To access the CyBy-specific functionality such as data queries or
||| updates, user authentification and authorization, the HTTP server
||| acts as a proxy forwarding all CyBy specific requests to the
||| CyBy server running locally on the same machine as the HTTP server
||| using the [SCGI](https://en.wikipedia.org/wiki/Simple_Common_Gateway_Interface).
|||
||| This module and its submodules provide the functionality necessary
||| to run a server behind a HTTP proxy.
module Network.SCGI

import Data.ByteString
import Data.SortedMap as SM
import FS.Socket
import System
import System.Clock

import IO.Async.Loop.Epoll

import public FS.Posix
import public IO.Async.Loop.Posix
import public IO.Async.Posix
import public Network.SCGI.Config
import public Network.SCGI.Error
import public Network.SCGI.Request
import public Network.SCGI.Response

%default total

||| An SCGI server programm
public export
0 SCGIProg : List Type -> Type -> Type
SCGIProg = Async Poll

||| A CyBy server programm represented as a `Pull`
public export
0 SCGIPull : Type -> List Type -> Type -> Type
SCGIPull = Pull SCGIProg

||| A CyBy server programm represented as a `Stream`
public export
0 SCGIStream : List Type -> Type -> Type
SCGIStream = Stream SCGIProg

0 Bytes : List Type -> Type
Bytes es = SCGIStream es ByteString

public export
0 ServerErrs : List Type
ServerErrs = [Errno, SCGIErr]

parameters {auto conf : Config}
           {auto has  : Has SCGIErr es}

  -- An SCGI request starts with the header size (in decimal)
  -- followed by a colon (ASCII: 58): "75:" followed by a header of
  -- name-value pairs of the given number of bytes (here: 75)
  -- separated by zero bytes.
  headerSize : Bytes es -> SCGIPull o es (Nat, Bytes es)
  headerSize p =
       C.forceBreakFull InvalidRequest DropHit (58 ==) p
    |> C.limit (LargeHeader conf.maxHeaderSize) 10
    |> P.foldPair (<+>) empty
    |> map (mapFst $ fromMaybe 0 . ByteString.parseDecimalNat)

  -- The header consists of name-value pairs separated by zero bytes.
  header : Nat -> Bytes es -> SCGIPull o es (Headers, Bytes es)
  header n p =
       C.splitAt n p           -- keep the given number of bytes
    |> C.split (0 ==)          -- split them at 0
    |> P.foldPair (++) []      -- accumulated everything in a single list
    |> map (mapFst $ go empty) -- put name-value pairs in a sorted map

    where
      go : Headers -> List ByteString -> Headers
      go hs (x::y::t) = go (insert (toString x) (toString y) hs) t
      go hs _         = hs

  request : Bytes es -> SCGIPull o es Request
  request p = Prelude.do
    timestamp   <- liftIO (clockTime UTC)
    (hsz, rem1) <- headerSize p
    when (hsz > conf.maxHeaderSize) (throw $ LargeHeader conf.maxHeaderSize)
    (head,rem2) <- header hsz rem1
    cl          <- contentLength head
    u           <- requestURI head
    body        <- foldGet (:<) [<] (C.take cl $ C.drop 1 rem2)
    pure $ RQ
      head
      (requestMethod head)
      u
      cl
      (contentType head)
      (RT timestamp)
      (fastConcat $ body <>> [])

export %inline
ierror : Interpolation a => a -> SCGIProg es ()
ierror x = stderrLn (interpolate x)

||| This is the end of the world where we serve the
||| SCGI-application. All we need is a bit of information to get going:
|||
||| @ config   : application configuration
||| @ run      : core SCGI application converting SCGI request to
|||              HTTP responses
export covering
serve : Config -> (Request -> SCGIProg ServerErrs Response) -> SCGIProg [] ()
serve c@(C a p _ _ co) run =
  mpull $ handle handlers $ 
    foreachPar co doServe (acceptOn AF_INET SOCK_STREAM $ IP4 a p)

  where
    handlers : All (\e => e -> SCGIPull Void [] ())  ServerErrs
    handlers = [exec . ierror, exec . ierror]

    doServe : Socket AF_INET -> SCGIProg [] ()
    doServe cli =
      mpull $ finally (close' cli) $ handle handlers $ Prelude.do
        req  <- request (bytes cli 0xffff)
        resp <- exec (run req)
        writeTo cli (emits $ responseBytes resp)

||| Simplified version of `serve` used for wrapping a simple `IO`
||| converter.
|||
||| Don't use this if you are planning to serve more than a handful
||| connections concurrently.
export covering
serveIO : Config -> (Request -> IO Response) -> IO ()
serveIO c run = simpleApp (serve c $ liftIO . run)
