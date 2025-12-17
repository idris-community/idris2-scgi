module Network.SCGI.Response

import Data.Buffer
import Data.ByteString
import Data.List
import Data.SortedMap as SM
import Network.SCGI.Request

%default total

export
record Status where
  constructor MkStatus
  status : ByteString

export
mkStatus : (code : Nat) -> (msg : String) -> Status
mkStatus code msg = MkStatus $ fromString "\{show code} \{msg}"

export
ok200 : Status
ok200 = mkStatus 200 "OK"

export
created201 : Status
created201 = mkStatus 201 "Created"

export
accepted202 : Status
accepted202 = mkStatus 202 "Accepted"

export
noContent204 : Status
noContent204 = mkStatus 204 "No Content"

export
badRequest400 : Status
badRequest400 = mkStatus 400 "Bad Request"

export
unauthorized401 : Status
unauthorized401 = mkStatus 401 "Unauthorized"

export
forbidden403 : Status
forbidden403 = mkStatus 403 "Forbidden"

export
notFound404 : Status
notFound404 = mkStatus 404 "Not Found"

export
methodNotAllowed405 : Status
methodNotAllowed405 = mkStatus 405 "Method Not Allowed" 

export
internalServerError500 : Status
internalServerError500 = mkStatus 500 "Internal Server Error"

export
notImplemented501 : Status
notImplemented501 = mkStatus 501 "Not Implemented"

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
empty : Response
empty = RP empty []

export
addHeader :  ByteString -> ByteString  -> Response -> Response
addHeader x y = {headers $= insert x y}

export
setStatus : Status -> Response -> Response
setStatus s = addHeader "status" s.status

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

export
notFound : Response
notFound = setStatus notFound404 empty

export
forbidden : Response
forbidden = setStatus forbidden403 empty
