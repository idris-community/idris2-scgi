module HTTP.API.Method

import Data.ByteString
import Data.SortedMap
import HTTP.API.Decode
import HTTP.API.Serve
import JSON.Simple

%default total

--------------------------------------------------------------------------------
-- Method
--------------------------------------------------------------------------------

public export
record Method (formats : List Type) (val : Type) where
  constructor M
  method : ByteString
  status : Status

public export
GET : (0 formats : List Type) -> (0 val : Type) -> Method formats val
GET _ _ = M "GET" ok200

public export
POST : (0 formats : List Type) -> (0 val : Type) -> Method formats val
POST _ _ = M "POST" ok200

public export
PUT : (0 formats : List Type) -> (0 val : Type) -> Method formats val
PUT _ _ = M "PUT" ok200

public export
record Method' where
  constructor M'
  method : ByteString

public export
GET' : Method'
GET' = M' "GET"

public export
POST' : Method'
POST' = M' "POST"

public export
PUT' : Method'
PUT' = M' "PUT"

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

canUseMethod : Method ts t -> All (EncodeVia t) ts -> Request -> Bool
canUseMethod (M m _) all r =
  Just m == lookup "REQUEST_METHOD" r.headers &&
  any (acceptsMedia r) (forget $ mapProperty (\x => mediaType @{x}) all)

encode : Status -> t -> Request -> All (EncodeVia t) ts -> Response -> Response
encode s v r []        rs = rs -- impossible
encode s v r (e :: es) rs =
  case acceptsMedia r (mediaType @{e}) of
    False => encode s v r es rs
    True  => {content := encodeVia v e} rs |> setContentType e |> setStatus s

public export
(all : All (EncodeVia t) ts) => Serve (Method ts t) where
  InTypes  = []
  OutTypes = [t]
  outs     = %search
  fromRequest m r = pure $ if canUseMethod m all r then Just [] else Nothing
  adjResponse m [v] req = pure . encode m.status v req all

public export
Serve Method' where
  InTypes  = []
  OutTypes = [()]
  outs     = %search
  fromRequest (M' m) r =
    pure $ if Just m == lookup "REQUEST_METHOD" r.headers then Just [] else Nothing
  adjResponse m _ _   = pure . setStatus noContent204
