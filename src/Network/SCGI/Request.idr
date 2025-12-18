module Network.SCGI.Request

import Control.Monad.MErr
import Data.Buffer
import Data.ByteString
import Data.SortedMap
import Data.String
import Derive.Prelude
import Network.SCGI.Config
import Network.SCGI.Error
import public Network.SCGI.URI
import Text.ILex
import System.Clock

%default total
%hide Data.Linear.(.)
%language ElabReflection

--------------------------------------------------------------------------------
--          Headers
--------------------------------------------------------------------------------

public export
record MimeType where
  constructor MT
  type  : String
  param : Maybe (String,String)

toMimeType : ByteString -> Maybe MimeType
toMimeType bs =
  case trim <$> split 59 bs of -- 59 = ;
    [mt,p] => case (ByteString.toString . trim) <$> split 61 p of
      [pa,v] => Just (MT (toLower $ toString mt) (Just (toLower pa, v)))
      _      => Nothing
    [mt]   => Just (MT (toLower $ toString mt) Nothing)
    _      => Nothing

||| Alias for a sorted map (dictionary) mapping header names to
||| header values.
public export
0 Headers : Type
Headers = SortedMap ByteString ByteString

export %inline
contentType : Headers -> Maybe MimeType
contentType hs = lookup "CONTENT_TYPE" hs >>= toMimeType

export
hasContentType : Headers -> String -> Bool
hasContentType hs s = Just s == map type (contentType hs)

export
accept : Headers -> List MimeType
accept =
    mapMaybe toMimeType
  . maybe [] (map trim . split 44)
  . lookup "HTTP_ACCEPT"

export
acceptsMedia : Headers -> String -> Bool
acceptsMedia hs s =
 let ts := accept hs
  in any ((s ==) . type) ts || any (("*/*" ==) . type) ts 

--------------------------------------------------------------------------------
--          RequestTime
--------------------------------------------------------------------------------

||| Time stamp of the HTTP request we currently process.
|||
||| This is used when checking whether a user's CyBy session has expired but
||| also to set the time of creation or modification when editing an item
||| in the database.
public export
record RequestTime where
  [noHints]
  constructor RT
  time : Clock UTC

--------------------------------------------------------------------------------
--          Request
--------------------------------------------------------------------------------

||| A request sent from the client listing the SCGI headers provided by
||| the proxy server, the total content size, and an IO action for
||| streaming the content.
public export
record Request where
  [noHints]
  constructor RQ

  ||| The SCGI headers as sent by the proxy server
  headers     : Headers

  ||| URI of the request
  uri         : URI

  ||| The total size of the content in case of a POST request
  contentSize : Nat

  ||| Timestamp when the request arrived at the server
  timestamp   : RequestTime

  ||| Content
  content     : ByteString

--------------------------------------------------------------------------------
--          Multipart Requests
--------------------------------------------------------------------------------

requestStr : String
requestStr = #"form-data; name="request""#

crlf : ByteString
crlf = "\r\n"

public export
record Parts where
  constructor MkParts
  json  : ByteString
  bytes : ByteString

pair : ByteString -> (String,String)
pair bs =
  let (x,y) := ByteString.break (58 ==) bs
   in (toLower $ toString x, toLower . toString . trimLeft $ drop 1 y)

part : Parts -> ByteString -> Parts
part p bs =
  let (x,y) := breakAtSubstring (crlf <+> crlf) bs
      hs    := splitAtSubstring crlf x
      m     := SortedMap.fromList $ map pair hs
   in case lookup "content-disposition" m of
        Just x  =>
          if x == requestStr
             then {json := drop 4 y} p
             else {bytes := drop 4 y} p
        Nothing => p

export
multipart : (sep : String) -> ByteString -> Parts
multipart sep =
  let sepBS := ByteString.fastConcat ["--", fromString sep, crlf]
   in foldl part (MkParts empty empty) . splitAtSubstring sepBS

export
requestURI : Headers -> Maybe ByteString
requestURI hs = lookup "REQUEST_URI" hs

export
requestPath : Headers -> Maybe ByteString
requestPath hs = Prelude.do
  u <- requestURI hs
  case split 63 u of
    x::_ => Just x
    _    => Nothing

--------------------------------------------------------------------------------
--          Parsers
--------------------------------------------------------------------------------

parameters {auto c    : Config}
           {auto has  : Has RequestErr es}
           {auto merr : MErr f}

  ||| Gets the content length (number of bytes in the content part of
  ||| the SCGI message) from the headers.
  export
  contentLength : Headers -> f es Nat
  contentLength hs =
    let sz := maybe Z (cast . decimal) $ lookup "CONTENT_LENGTH" hs
     in if sz > c.maxMsgSize then throw (largeBody c.maxMsgSize) else pure sz

  ||| Gets the request URI from the headers.
  export
  parseRequestURI : Headers -> f es URI
  parseRequestURI hs =
    maybe (throw badRequest) pure $ requestURI hs >>= parseURI
