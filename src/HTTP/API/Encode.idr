module HTTP.API.Encode

import public Data.ByteString
import JSON.Simple

%default total
%language ElabReflection

public export
0 Text : Type
Text = String

public export
0 Octett : Type
Octett = ByteString

public export
interface EncodeVia (0 from, to : Type) where
  encodeAs : from -> to
  toBytes  : to -> List ByteString
  mediaType : String

export %inline
encodeVia : (v : f) -> EncodeVia f t -> List ByteString
encodeVia v c = toBytes @{c} $ encodeAs @{c} v

export %inline
Interpolation a => EncodeVia a String where
  encodeAs  = interpolate
  toBytes   = pure . fromString
  mediaType = "text/plain"

export %inline
Cast a ByteString => EncodeVia a ByteString where
  encodeAs  = cast
  toBytes   = pure
  mediaType = "application/octett-stream"

export %inline
ToJSON a => EncodeVia a JSON where
  encodeAs  = toJSON
  toBytes   = pure . fromString . show
  mediaType = "application/json"
