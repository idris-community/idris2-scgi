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

canUseMethod : All (EncodeVia t) ts -> Request -> Handler (HList [])
canUseMethod a r =
  case any (acceptsMedia r.headers) (forget $ mapProperty (\x => mediaType @{x}) a) of
    True  => pure []
    False => throw $ requestErr unsupportedMediaType415

public export
(all : All (EncodeVia t) ts) => Serve (Method ts t) where
  InTypes  = []
  OutTypes = [t]
  outs     = %search
  canHandle (M m _) r = Just m == lookup "REQUEST_METHOD" r.headers
  fromRequest m r = canUseMethod all r
  adjResponse m [v] req = pure . encodeBody m.status v req.headers all

public export
Serve Method' where
  InTypes  = []
  OutTypes = [()]
  outs     = %search
  canHandle (M' m) r = Just m == lookup "REQUEST_METHOD" r.headers
  fromRequest (M' m) r = pure []
  adjResponse m _ _   = pure . setStatus noContent204
