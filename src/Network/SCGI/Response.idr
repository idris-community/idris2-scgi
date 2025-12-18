module Network.SCGI.Response

import Data.Buffer
import Data.ByteString
import Data.List
import Data.List.Quantifiers
import Data.SortedMap as SM
import HTTP.API.Decode
import HTTP.API.Error
import JSON.Simple
import Network.SCGI.Error
import Network.SCGI.Prog
import Network.SCGI.Request

%default total

public export
0 Handler : Type -> Type
Handler = SCGIProg [RequestErr]

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
setStatus s = addHeader "status" (cast s)

crlf : ByteString
crlf = "\r\n"

export
responseBytes : Response -> List ByteString
responseBytes (RP hs bs) =
  case kvList hs of
    [] => crlf :: crlf :: bs 
    ps => (ps >>= \(h,v) => [h,":",v,crlf]) ++ crlf :: bs

export
setContentType : EncodeVia f t -> Response -> Response
setContentType e = addHeader "content-type" (fromString $ mediaType @{e})

export
encodeBody :
     Status
  -> t
  -> Headers
  -> All (EncodeVia t) ts
  -> Response
  -> Response
encodeBody s v hs []        rs = setStatus s rs
encodeBody s v hs (e :: es) rs =
  case acceptsMedia hs (mediaType @{e}) of
    False => encodeBody s v hs es rs
    True  => {content := encodeVia v e} rs |> setContentType e |> setStatus s

--------------------------------------------------------------------------------
--          Common Responses
--------------------------------------------------------------------------------

encErr : All (EncodeVia RequestErr) [JSON, Text]
encErr = %search

export
fromError : Headers -> RequestErr -> Response
fromError hs re@(RE st err _ _ _) =
 let u := maybe "" toString $ requestPath hs
  in encodeBody (MkStatus st err) ({path := u} re) hs encErr empty

export
fromStatus : Headers -> Status -> Response
fromStatus hs = fromError hs . requestErr

export
notFound : Headers -> Response
notFound hs = fromStatus hs notFound404

export
forbidden : Headers -> Response
forbidden hs = fromStatus hs forbidden403
