module HTTP.API.Decode

import public Data.ByteString
import Data.Either
import JSON.Simple
import Network.SCGI.Response
import Text.ILex

%default total

||| An interface for decoding value from a sequence of raw bytes.
public export
interface Decode (0 a : Type) where
  decode : ByteString -> Maybe a

||| Utiliy alias for `decode` that allows to explicitly specify the
||| target type.
public export %inline
decodeAs : (0 a : Type) -> Decode a => ByteString -> Maybe a
decodeAs _ = decode

export %inline
Decode ByteString where decode = Just

export %inline
Decode String where decode = Just . toString

export
Decode Nat where
  decode (BS 0 _) = Nothing
  decode bs = if all isDigit bs then Just (cast $ decimal bs) else Nothing

export
Decode Bits8 where decode = map cast . decodeAs Nat

export
Decode Bits16 where decode = map cast . decodeAs Nat

export
Decode Bits32 where decode = map cast . decodeAs Nat

export
Decode Bits64 where decode = map cast . decodeAs Nat

export
Decode Integer where
  decode (BS 0 _) = Nothing
  decode bs@(BS (S k) bv) =
    case head bv of
      45 => map (negate . cast) (decodeAs Nat (BS k $ tail bv))
      43 => map cast (decodeAs Nat (BS k $ tail bv))
      _  => map cast $ decodeAs Nat bs

export
Decode Int8 where decode = map cast . decodeAs Integer

export
Decode Int16 where decode = map cast . decodeAs Integer

export
Decode Int32 where decode = map cast . decodeAs Integer

export
Decode Int64 where decode = map cast . decodeAs Integer

||| An interface for decoding values by reading a prefix
||| of a list of bytestrings such as a path in a URL.
public export
interface DecodeMany (0 a : Type) where
  decodeMany : List ByteString -> Maybe (List ByteString, a)

export
Decode a => DecodeMany a where
  decodeMany []      = Nothing
  decodeMany (b::bs) = (bs,) <$> decode b

decsnoc :
     {auto dec : Decode a}
  -> SnocList a
  -> List ByteString
  -> (List ByteString, SnocList a)
decsnoc sx []        = ([], sx)
decsnoc sx (x :: xs) =
  case decodeAs a x of
    Just v  => decsnoc (sx:<v) xs
    Nothing => (x::xs, sx)

export
Decode a => DecodeMany (SnocList a) where
  decodeMany bs = Just $ decsnoc [<] bs

export
Decode a => DecodeMany (List a) where
  decodeMany bs = map (<>> []) <$> decodeMany bs

--------------------------------------------------------------------------------
-- EncodeVia
--------------------------------------------------------------------------------

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

export
setContentType : EncodeVia f t -> Response -> Response
setContentType e = addHeader "content-type" (fromString $ mediaType @{e})

--------------------------------------------------------------------------------
-- DecodeVia
--------------------------------------------------------------------------------

namespace DecodeVia
  public export
  interface DecodeVia (0 from, to : Type) where
    fromBytes  : ByteString -> Maybe from
    decodeFrom : from -> Maybe to
    mediaType  : String

export
decodeVia : (d : DecodeVia from to) => ByteString -> Maybe to
decodeVia bs = fromBytes @{d} bs >>= decodeFrom

export %inline
FromJSON a => DecodeVia JSON a where
  fromBytes  = eitherToMaybe . runBytes json
  decodeFrom = eitherToMaybe . fromJSON
  mediaType  = "application/json"
