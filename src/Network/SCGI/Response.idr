module Network.SCGI.Response

import Data.ByteString
import Data.SortedMap as SM
import Data.String
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

crlf : String
crlf = "\r\n"

export
responseBytes : Response -> List ByteString
responseBytes (RP hs bs) =
  let h := intersperse crlf $ map (\(h,v) => "\{h}:\{v}") (SM.toList hs)
   in fromString "\{fastConcat h}\r\n\r\n" :: bs

--------------------------------------------------------------------------------
--          Common Responses
--------------------------------------------------------------------------------

export %inline
response : List (String,String) -> List ByteString -> Response
response ps = RP (SM.fromList ps)

export %inline
response1 : List (String,String) -> ByteString -> Response
response1 ps = response ps . pure

export
statusOK : (String,String)
statusOK = ("Status", "200 OK")

export
json : (String,String)
json = ("content-type", "application/json")

export
plain : (String,String)
plain = ("content-type", "text/plain")

export
html : (String,String)
html = ("content-type", "text/html")

sdf, ods, csv : (String,String)
sdf = ("content-type", "chemical/x-mdl-sdfile")
ods = ("content-type", "application/vnd.oasis.opendocument.spreadsheet")
csv = ("content-type", "text/csv")

export
jsonOK : Headers
jsonOK = SM.fromList [statusOK, json]

export
bytesOK : Headers
bytesOK = SM.fromList [statusOK]

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
