module Network.SCGI

import public HTTP.API.Server
import public Network.SCGI.Config

import Data.SortedMap as SM
import Data.String
import FS.Socket
import IO.Async.Loop.Epoll
import System

%default total

prettyNS : Integer -> String
prettyNS n = "\{secs}\{msecs}\{usecs}\{nsecs}"
  where
    secs, msecs, usecs, nsecs : String
    secs =
      case n `div` 1_000_000_000 of
        0 => ""
        s => "\{show s} s "

    msecs =
      case n `div` 1_000_000 of
        0 => ""
        s => "\{show $ s `mod` 1000} ms "

    usecs =
      case n `div` 1_000 of
        s => "\{show $ s `mod` 1000} us "

    nsecs = "\{show $ n `mod` 1000} ns"

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

queryLine : (ByteString,QueryVal) -> String
queryLine (n,QVal v) = "\{n}: \{v}"
queryLine (n,QEmpty) = "\{n}"

--------------------------------------------------------------------------------
-- Headers
--------------------------------------------------------------------------------

CONTENT_LENGTH : String
CONTENT_LENGTH = "CONTENT_LENGTH"

REQUEST_URI : String
REQUEST_URI = "REQUEST_URI"

REQUEST_METHOD : String
REQUEST_METHOD = "REQUEST_METHOD"

CONTENT_TYPE : String
CONTENT_TYPE = "CONTENT_TYPE"

HTTP_PRE : ByteString
HTTP_PRE = "HTTP_"

adjhname : ByteString -> String
adjhname bs =
  ByteString.toString $ if HTTP_PRE `isPrefixOf` bs then drop (size HTTP_PRE) bs else bs

scgiHeader : String -> ByteString -> Headers -> Headers
scgiHeader n v hs =
  if      n == CONTENT_LENGTH then insertHeader Content_Length v hs
  else if n == CONTENT_TYPE then insertHeader Content_Type v hs
  else insertHeader n v hs

parameters {auto conf : Config}
           {auto has  : Has RequestErr es}
           {auto log  : HTTPLogger}

  contentLength : Headers -> HTTPPull o es Nat
  contentLength hs =
   let c := contentLength hs
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
    |> P.observe (\xs => debugML $ map (\x => "Header part: \{x}") xs)
    |> P.foldPair (++) []             -- accumulated everything in a single list
    |> map (mapFst $ go emptyHeaders) -- put name-value pairs in a sorted map

    where
      go : Headers -> List ByteString -> Headers
      go hs (x::y::t) = go (scgiHeader (adjhname x) y hs) t
      go hs _         = hs

  request : Bytes es -> HTTPPull o es Request
  request p = Prelude.do
    (hsz, rem1) <- headerSize p
    when (hsz > conf.maxHeaderSize) (throw $ largeHeader conf.maxHeaderSize)
    (head,rem2) <- header hsz rem1
    exec $ debugML ((\(k,v) => "\{k}: \{v}") <$> kvList head)
    cl          <- contentLength head
    m           <- parseRequestMethod head
    u           <- parseRequestURI head
    exec $ info "Got a request at \{encodePath u} (\{show cl} bytes)"
    exec $ debugML ("queries:" :: map queryLine u.queries)
    body        <- foldGet (:<) [<] (C.take cl $ C.drop 1 rem2)
    pure $ RQ m head u (fastConcat $ body <>> [])

logErr : HTTPLogger => RequestErr -> HTTPPull o es ()
logErr (RE s e m d p) =
  exec $ if "" == p then warnML msgLines else infoML msgLines
  where
    msg, dts : List String
    msg = if "" == m then [] else ["message: \{m}"]

    dts = case d of
      "" => []
      _  => "details:" :: map (indent 2) (String.lines d)

    msgLines : List String
    msgLines =
     let u := if "" == p then "" else "at \{p}"
         m := "invalid request \{u} (status code \{show s}): \{e}"
      in m :: msg ++ dts

||| This is the end of the world where we serve the
||| SCGI-application. All we need is a bit of information to get going:
|||
||| @ config   : application configuration
||| @ run      : core SCGI application converting SCGI request to
|||              HTTP responses
export covering
serve : HTTPLogger => Config -> (Request -> Handler Response) -> HTTPProg [] ()
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
    doServe cli = Prelude.do
      d <- delta $ mpull $ finally (close' cli) $ handle handlers $
             extractErr RequestErr (request' cli) >>= \case
               Left x  => logErr x >> send cli (fromError Nothing emptyHeaders x)
               Right req => exec (weakenErrors $ extractErr RequestErr $ run req) >>= \case
                 Left x  => logErr x >> send cli (fromError (Just req.uri) req.headers x)
                 Right resp => send cli resp
      debug "request served in \{prettyNS $ toNano d}"

||| Simplified version of `serve` used for wrapping a simple `IO`
||| converter.
|||
||| Don't use this if you are planning to serve more than a handful
||| connections concurrently.
export covering
serveIO : HTTPLogger => Config -> (Request -> IO Response) -> IO ()
serveIO c run = simpleApp (serve c $ liftIO . run)
