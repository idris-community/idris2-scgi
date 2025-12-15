module HTTP.API.Method

import Data.ByteString
import Data.SortedMap
import HTTP.API.Serve
import JSON.Simple
import Network.SCGI.Request

%default total

--------------------------------------------------------------------------------
-- Encodevia
--------------------------------------------------------------------------------

public export
interface EncodeVia (0 from, to : Type) where
  encodeAs : from -> to
  toBytes  : to -> List ByteString
  mediaType : String

export %inline
encodeVia : (v : f) -> EncodeVia f t -> List ByteString
encodeVia v c = toBytes @{c} $ encodeAs @{c} v

export %inline
EncodeVia String String where
  encodeAs  = id
  toBytes   = pure . fromString
  mediaType = "text/plain"

export %inline
EncodeVia ByteString ByteString where
  encodeAs  = id
  toBytes   = pure
  mediaType = "application/octett-stream"

export %inline
ToJSON a => EncodeVia a JSON where
  encodeAs  = toJSON
  toBytes   = pure . fromString . show
  mediaType = "application/json"

--------------------------------------------------------------------------------
-- Method
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

canUseMethod : Method ts t -> All (EncodeVia t) ts -> Request -> Bool
canUseMethod (M m) all r =
  Just m == lookup "REQUEST_METHOD" r.headers &&
  any (acceptsMedia r) (forget $ mapProperty (\x => mediaType @{x}) all)

encode : (val : t) -> Request -> All (EncodeVia t) ts -> List ByteString
encode v r []        = [] -- impossible
encode v r (e :: es) =
  if acceptsMedia r (mediaType @{e}) then encodeVia v e else encode v r es

public export
(all : All (EncodeVia t) ts) => Serve (Method ts t) where
  InTypes  = []
  OutTypes = [t]
  outs     = %search
  fromRequest m r = pure $ if canUseMethod m all r then Just [] else Nothing
  adjResponse m [v] req = pure . {content := encode v req all}

public export
Serve Method' where
  InTypes  = []
  OutTypes = []
  outs     = %search
  fromRequest (M' m) r =
    pure $ if Just m == lookup "REQUEST_METHOD" r.headers then Just [] else Nothing
  adjResponse _  _ _   = pure
