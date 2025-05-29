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
import FS.Posix
import FS.Socket
import System
import System.Clock

import public Network.SCGI.Config
import public Network.SCGI.Error
import public Network.SCGI.Request
import public Network.SCGI.Response

%default total

-- 0 Bytes : List Type -> Type
-- Bytes es = CStream es ByteString
-- 
-- public export
-- 0 ServerErrs : List Type
-- ServerErrs = [Errno, SCGIErr, SqlError, JSONError]
-- 
-- parameters {auto conf : Config}
--            {auto has  : Has SCGIErr es}
-- 
--   -- An SCGI request starts with the header size (in decimal)
--   -- followed by a colon (ASCII: 58): "75:" followed by a header of
--   -- name-value pairs of the given number of bytes (here: 75)
--   -- separated by zero bytes.
--   headerSize : Bytes es -> CPull o es (Nat, Bytes es)
--   headerSize p =
--        C.forceBreakFull InvalidRequest DropHit (58 ==) p
--     |> C.limit (LargeHeader conf.maxHeaderSize) 10
--     |> P.foldPair (<+>) empty
--     |> map (mapFst $ fromMaybe 0 . ByteString.parseDecimalNat)
-- 
--   -- The header consists of name-value pairs separated by zero bytes.
--   header : Nat -> Bytes es -> CPull o es (Headers, Bytes es)
--   header n p =
--        C.splitAt n p           -- keep the given number of bytes
--     |> C.split (0 ==)          -- split them at 0
--     |> P.foldPair (++) []      -- accumulated everything in a single list
--     |> map (mapFst $ go empty) -- put name-value pairs in a sorted map
-- 
--     where
--       go : Headers -> List ByteString -> Headers
--       go hs (x::y::t) = go (insert (toString x) (toString y) hs) t
--       go hs _         = hs
-- 
--   request : Bytes es -> CPull o es Request
--   request p = Prelude.do
--     timestamp   <- liftIO (clockTime UTC)
--     (hsz, rem1) <- headerSize p
--     when (hsz > conf.maxHeaderSize) (throw $ LargeHeader conf.maxHeaderSize)
--     (head,rem2) <- header hsz rem1
--     cl          <- contentLength head
--     u           <- requestURI head
--     body        <- foldGet (:<) [<] (C.take cl $ C.drop 1 rem2)
--     pure $ RQ
--       head
--       (requestMethod head)
--       u
--       cl
--       (contentType head)
--       (RT timestamp)
--       (fastConcat $ body <>> [])
-- 
-- ||| This is the end of the world where we serve the
-- ||| CyBy-application. All we need is a bit of information to get going:
-- |||
-- ||| @ config   : application configuration
-- ||| @ cyby     : core CyBy application converting SCGI request to
-- |||              HTTP responses
-- export covering
-- serve : Config -> (Request -> CProg ServerErrs Response) -> CProg [] ()
-- serve c@(C a p _ _ _ _ co) cyby =
--   mpull $ handle handlers $ 
--     foreachPar co doServe (acceptOn AF_INET SOCK_STREAM $ IP4 a p)
-- 
--   where
--     handlers : All (\e => e -> CPull Void [] ())  ServerErrs
--     handlers = [exec . ierror, exec . ierror, exec . ierror, exec . ierror]
-- 
--     doServe : Socket AF_INET -> CProg [] ()
--     doServe cli =
--       mpull $ finally (close' cli) $ handle handlers $ Prelude.do
--         req  <- request (bytes cli 0xffff)
--         resp <- exec (cyby req)
--         writeTo cli (emits $ responseBytes resp)
-- 
-- --------------------------------------------------------------------------------
-- --          Encoding and Decoding JSON Data
-- --------------------------------------------------------------------------------
-- 
-- parameters {auto merr : MErr f}
-- 
--   export
--   decode : Has JSONError es => FromJSON a => Request -> f es a
--   decode req =
--     let s := stringContent req
--      in injectEither (mapFst (ContentError s) $ decode s)
-- 
--   export
--   decodeMP : Has JSONError es => FromJSON a => Parts -> f es a
--   decodeMP mp =
--     let s := toString mp.json
--      in injectEither (mapFst (ContentError s) $ decode s)
-- 
-- export %inline
-- encode : ToJSON a => a -> Response
-- encode = RP jsonOK . pure . fromString . encode
