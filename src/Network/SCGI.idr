module Network.SCGI

import public HTTP.API.Server
import public Network.SCGI.Config
import public Network.SCGI.Logging

import Data.SortedMap as SM
import FS.Socket
import IO.Async.Loop.Epoll
import System

%default total

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

largeBody : Nat -> RequestErr
largeBody n =
  {message := "Maximum content size is \{show n} bytes"} $
    requestErr contentTooLarge413

largeHeader : Nat -> RequestErr
largeHeader n =
  {message := "Maximum header size is \{show n} bytes"} $
    requestErr requestHeaderFieldsTooLarge431

badRequest : RequestErr
badRequest = requestErr badRequest400

0 Bytes : List Type -> Type
Bytes es = HTTPStream es ByteString

--------------------------------------------------------------------------------
-- Headers
--------------------------------------------------------------------------------

CONTENT_LENGTH : ByteString
CONTENT_LENGTH = "CONTENT_LENGTH"

REQUEST_URI : ByteString
REQUEST_URI = "REQUEST_URI"

REQUEST_METHOD : ByteString
REQUEST_METHOD = "REQUEST_METHOD"

CONTENT_TYPE : ByteString
CONTENT_TYPE = "CONTENT_TYPE"

HTTP_PRE : ByteString
HTTP_PRE = "HTTP_"

scgiHeader : ByteString -> ByteString -> Headers -> Headers
scgiHeader n v hs =
  if HTTP_PRE `isPrefixOf` n then insertHeader (drop 5 n) v hs
  else if n == CONTENT_LENGTH then insertHeader Content_Size v hs
  else if n == CONTENT_TYPE then insertHeader Content_Type v hs
  else if n == REQUEST_URI || n == REQUEST_METHOD then insertHeader n v hs
  else hs

parameters {auto conf : Config}
           {auto has  : Has RequestErr es}
           {auto log  : Logger}

  contentLength : Headers -> HTTPPull o es Nat
  contentLength hs =
   let c := contentSize hs
    in case c > conf.maxMsgSize of
         False => pure c
         True  => throw (largeBody conf.maxMsgSize)

  parseRequestURI : Headers -> HTTPPull o es URI
  parseRequestURI hs =
    maybe (throw badRequest) requestURI $
      lookupUpperCaseHeader REQUEST_URI hs

  parseRequestMethod : Headers -> HTTPPull o es Method
  parseRequestMethod hs =
    maybe (throw badRequest) requestMethod $
      lookupUpperCaseHeader REQUEST_METHOD hs

  -- An SCGI request starts with the header size (in decimal)
  -- followed by a colon (ASCII: 58): "75:" followed by a header of
  -- name-value pairs of the given number of bytes (here: 75)
  -- separated by zero bytes.
  headerSize : Bytes es -> HTTPPull o es (Nat, Bytes es)
  headerSize p =
       C.forceBreakFull badRequest DropHit (58 ==) p
    |> C.limit (largeHeader conf.maxHeaderSize) 10
    |> P.foldPair (<+>) empty
    |> map (mapFst $ fromMaybe 0 . ByteString.parseDecimalNat)

  -- The header consists of name-value pairs separated by zero bytes.
  header : Nat -> Bytes es -> HTTPPull o es (Headers, Bytes es)
  header n p =
       C.splitAt n p                  -- keep the given number of bytes
    |> C.split (0 ==)                 -- split them at 0
    |> P.foldPair (++) []             -- accumulated everything in a single list
    |> map (mapFst $ go emptyHeaders) -- put name-value pairs in a sorted map

    where
      go : Headers -> List ByteString -> Headers
      go hs (x::y::t) = go (scgiHeader x y hs) t
      go hs _         = hs

  request : Bytes es -> HTTPPull o es Request
  request p = Prelude.do
    (hsz, rem1) <- headerSize p
    when (hsz > conf.maxHeaderSize) (throw $ largeHeader conf.maxHeaderSize)
    (head,rem2) <- header hsz rem1
    exec $ for_ (kvList head) $ \(k,v) => debug "\{k}: \{v}"
    cl          <- contentLength head
    m           <- parseRequestMethod head
    u           <- parseRequestURI head
    exec $ info "Got a request at \{u} (\{show cl} bytes)"
    body        <- foldGet (:<) [<] (C.take cl $ C.drop 1 rem2)
    pure $ RQ m head u (fastConcat $ body <>> [])

logErr : Logger => RequestErr -> HTTPPull o es ()
logErr re =
  exec $ case re.path of
    "" => Prelude.do
      warn "Invalid request (status code \{show re.status}): \{re.error}"
      warn "Message: \{re.message}"
    u  => Prelude.do
      info "Invalid request at \{u} (status code \{show re.status}): \{re.error}"
      info "Message: \{re.message}"
      when ("" /= re.details) $ info "Details: \{re.details}"

||| This is the end of the world where we serve the
||| SCGI-application. All we need is a bit of information to get going:
|||
||| @ config   : application configuration
||| @ run      : core SCGI application converting SCGI request to
|||              HTTP responses
export covering
serve : Logger => Config -> (Request -> Handler Response) -> HTTPProg [] ()
serve c@(C a p _ _ co) run =
  mpull $ handle handlers $ 
    foreachPar co doServe (acceptOn AF_INET SOCK_STREAM $ IP4 a p)

  where
    handlers : All (\e => e -> HTTPPull Void [] ())  [Errno]
    handlers = [exec . ierror]

    %inline
    request' : Socket AF_INET -> HTTPPull o [RequestErr,Errno] Request 
    request' cli = request $ bytes cli 0xffff

    send : Socket AF_INET -> Response -> HTTPStream [Errno] Void
    send cli resp = writeTo cli (emits $ responseBytes resp)

    doServe : Socket AF_INET -> HTTPProg [] ()
    doServe cli =
      mpull $ finally (close' cli) $ handle handlers $
        extractErr RequestErr (request' cli) >>= \case
          Left x  => logErr x >> send cli (fromError Nothing emptyHeaders x)
          Right req => exec (weakenErrors $ extractErr RequestErr $ run req) >>= \case
            Left x  => logErr x >> send cli (fromError (Just req.uri) req.headers x)
            Right resp => send cli resp

||| Simplified version of `serve` used for wrapping a simple `IO`
||| converter.
|||
||| Don't use this if you are planning to serve more than a handful
||| connections concurrently.
export covering
serveIO : Logger => Config -> (Request -> IO Response) -> IO ()
serveIO c run = simpleApp (serve c $ liftIO . run)
