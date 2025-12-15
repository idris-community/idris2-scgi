module Network.SCGI.Response

import Data.Buffer
import Data.ByteString
import Data.List
import Data.SortedMap as SM
import Network.SCGI.Request

%default total

--------------------------------------------------------------------------------
--          Response
--------------------------------------------------------------------------------

||| HTTP Response
|||
||| Currently, we include the content / body as a whole. This might
||| be changed to a stream of data if we ever decide to stream
||| large amounts of content.
public export
record Response where
  constructor RP
  headers : Headers
  content : List ByteString

export
addHeader : (ByteString,ByteString) -> Response -> Response
addHeader (x,y) = {headers $= insert x y}

crlf : ByteString
crlf = "\r\n"

export
responseBytes : Response -> List ByteString
responseBytes (RP hs bs) =
  case kvList hs of
    [] => crlf :: crlf :: bs 
    ps => (ps >>= \(h,v) => [h,":",v,crlf]) ++ crlf :: bs

--------------------------------------------------------------------------------
--          Common Responses
--------------------------------------------------------------------------------

export %inline
response : List (ByteString,ByteString) -> List ByteString -> Response
response ps = RP (SM.fromList ps)

export %inline
response1 : List (ByteString,ByteString) -> ByteString -> Response
response1 ps = response ps . pure

export
statusOK : (ByteString,ByteString)
statusOK = ("Status", "200 OK")

export
html : (ByteString,ByteString)
html = ("content-type", "text/html")

sdf, ods, csv : (ByteString,ByteString)
sdf = ("content-type", "chemical/x-mdl-sdfile")
ods = ("content-type", "application/vnd.oasis.opendocument.spreadsheet")
csv = ("content-type", "text/csv")

export
ok : Response
ok = addHeader statusOK $ RP empty []

export
notFound : Response
notFound =
  response1 [("Status", "404 Not Found"), html]
    """
    <!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
    <html>
    <head>
    <title>404 NotFound</title>
    </head>
    <body><h1>Resource Not Found</h1><p>The resource was not found.</p>
    </body>
    </html>
    """

export
forbidden : Response
forbidden =
  response1 [("Status", "403 Forbidden"), html]
    """
    <!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
    <html>
    <head>
    <title>403 Forbidden</title>
    </head>
    <body><h1>Access Denied</h1><p>You are not authorized to access the given resource.</p>
    </body>
    </html>
    """
