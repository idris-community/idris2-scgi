module Network.SCGI.Request

import Control.Monad.MErr
import Data.Buffer
import Data.ByteString
import Data.FilePath
import Data.SortedMap
import Data.String
import Derive.Prelude
import Network.SCGI.Config
import Network.SCGI.Error
import System.Clock

%default total
%language ElabReflection

--------------------------------------------------------------------------------
--          Headers
--------------------------------------------------------------------------------

||| Alias for a sorted map (dictionary) mapping header names to
||| header values.
public export
0 Headers : Type
Headers = SortedMap String String

--------------------------------------------------------------------------------
--          Method
--------------------------------------------------------------------------------

public export
data Method = GET | POST | UNKNOWN

%runElab derive "Method" [Show,Eq,Ord]

export %inline
Interpolation Method where interpolate = show


||| Gets the request method from the headers.
export
requestMethod : Headers -> Method
requestMethod hs =
  case lookup "REQUEST_METHOD" hs of
    Just "GET"  => GET
    Just "POST" => POST
    _           => UNKNOWN

--------------------------------------------------------------------------------
--          URI
--------------------------------------------------------------------------------

public export
record URI where
  constructor MkURI
  path   : Path Abs
  query  : SortedMap String String

||| Drops the unineresting prefixes from an absolute URI
||| path.
|||
||| In CyBy, a URI that gets sent to the server application looks
||| like so: "/some/prefix/scgi-bin/rest?queries" where "/some/prefix"
||| depends on the specifics of the system we are running on. When interpreting
||| URIs, we are only interested in the remainder, so this is what we keep here
||| here.
export
afterScgibin : Path Abs -> List Body
afterScgibin (PAbs _ sb) = go [] sb
  where
    go : List Body -> SnocList Body -> List Body
    go xs [<]                = xs
    go xs (sx :< "scgi-bin") = xs
    go xs (sx :< x)          = go (x::xs) sx

query : String -> Maybe (SortedMap String String)
query = map SortedMap.fromList . traverse pair . forget . split ('&' ==)

  where
    pair : String -> Maybe (String,String)
    pair s =
      let (x,y)         := break ('=' ==) s
          StrCons '=' t := strM y | _ => Nothing
       in Just (x,t)

parsePth : String -> Maybe (Path Abs)
parsePth s =
  case [<] <>< unpack s of
    ss :< '/' => parse $ pack (ss <>> [])
    _         => parse s

export
parseURI : String -> Maybe URI
parseURI s =
  case forget $ split ('?' ==) s of
    [p,q] => [| MkURI (parsePth p) (query q) |]
    [p]   => [| MkURI (parsePth p) (pure empty) |]
    _     => Nothing

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

public export
data ContentType : Type where
  JSON      : ContentType
  Multipart : (sep : String) -> ContentType
  Other     : String -> ContentType
  None      : ContentType

export
Interpolation ContentType where
  interpolate JSON            = "application/json"
  interpolate (Multipart sep) = "multipart/form-data; boundary=\{sep}"
  interpolate (Other s)       = s
  interpolate None            = "none"

export
contentType : Headers -> ContentType
contentType hs =
  case lookup "CONTENT_TYPE" hs of
    Just "application/json" => JSON
    Just o                  =>
      if "multipart/form-data; boundary=" `isPrefixOf` o
         then Multipart $ pack . snd . splitAt 30 $ unpack o
         else Other o
    Nothing                 => None

||| A request sent from the client listing the SCGI headers provided by
||| the proxy server, the total content size, and an IO action for
||| streaming the content.
public export
record Request where
  [noHints]
  constructor RQ

  ||| The SCGI headers as sent by the proxy server
  headers     : Headers

  ||| http method of the request
  method      : Method

  ||| URI of the request
  uri         : URI

  ||| The total size of the content in case of a POST request
  contentSize : Nat

  ||| The content type (if any) of the payload
  contentType : ContentType

  ||| Timestamp when the request arrived at the server
  timestamp   : RequestTime

  ||| Content
  content     : ByteString

export %inline
stringContent : Request -> String
stringContent r = toString r.content

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

--------------------------------------------------------------------------------
--          Parsers
--------------------------------------------------------------------------------

parameters {auto c    : Config}
           {auto has  : Has SCGIErr es}
           {auto merr : MErr f}

  ||| Gets the content length (number of bytes in the content part of
  ||| the SCGI message) from the headers.
  export
  contentLength : Headers -> f es Nat
  contentLength hs =
    let sz := maybe Z cast $ lookup "CONTENT_LENGTH" hs
     in if sz > c.maxMsgSize then throw (LargeBody c.maxMsgSize) else pure sz

  ||| Gets the request URI from the headers.
  export
  requestURI : Headers -> f es URI
  requestURI hs =
    maybe (throw InvalidRequest) pure $ lookup "REQUEST_URI" hs >>= parseURI
