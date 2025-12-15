module HTTP.API.Method

import Data.ByteString
import Data.SortedMap
import HTTP.API.Serve
import Network.SCGI.Request

%default total

public export
interface DecodeVia (0 to, from : Type) where
  parse   : ByteString -> Maybe from
  convert : from -> Maybe to

export %inline
parseConvert : (0 from,to : Type) -> DecodeVia to from => ByteString -> Maybe to
parseConvert from to @{c} bs = parse @{c} bs >>= convert @{c}

public export
interface EncodeVia (0 from, to : Type) where
  encodeAs : from -> to
  toBytes  : to -> List ByteString

export %inline
encodeVia : (0 to : Type) -> (v : f) -> EncodeVia f to => List ByteString
encodeVia to v @{c} = toBytes @{c} $ encodeAs @{c} v

public export
record Method (formats : List Type) (val : Type) where
  constructor M
  method : ByteString

public export
GET : (0 formats : List Type) -> (0 val : Type) -> Method formats val
GET _ _ = M "GET"

public export
POST : (0 formats : List Type) -> (0 val : Type) -> Method formats val
POST _ _ = M "POST"

encode :
     (0 ts : List Type)
  -> (val : t)
  -> All (EncodeVia t) ts
  -> ByteString

public export
All (EncodeVia t) ts => Serve (Method ts t) where
  InTypes  = []
  OutTypes = [t]
  outs     = %search
  fromRequest (M m) r =
    pure $ if Just m == lookup "REQUEST_METHOD" r.headers then Just [] else Nothing
  adjResponse _ _ = pure
