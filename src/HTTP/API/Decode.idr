module HTTP.API.Decode

import Data.Either
import Data.List.Quantifiers
import JSON.Simple
import Text.ILex
import public Data.ByteString
import public HTTP.API.Encode
import public HTTP.API.Error
import public HTTP.API.Status

%default total

||| An interface for decoding value from a sequence of raw bytes.
public export
interface Decode (0 a : Type) where
  decode : ByteString -> Either DecodeErr a

||| Utiliy alias for `decode` that allows to explicitly specify the
||| target type.
public export %inline
decodeAs : (0 a : Type) -> Decode a => ByteString -> Either DecodeErr a
decodeAs _ = decode

export %inline
Decode ByteString where decode = Right

export %inline
Decode String where decode = Right . toString

export
Decode Nat where
  decode (BS 0 _) = Left $ invalidVal "natural number" ""
  decode bs =
    if all isDigit bs
       then Right (cast $ decimal bs)
       else Left $ invalidVal "natural number" (toString bs)

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
  decode (BS 0 _) = Left (invalidVal "integer" "")
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
  simulateDecode : List ByteString -> Maybe (List ByteString)

  decodeMany : List ByteString -> Either DecodeErr (List ByteString, a)

export
Decode a => DecodeMany a where
  simulateDecode []      = Nothing
  simulateDecode (b::bs) = Just bs

  decodeMany []      = Left (Msg "Unexpected end of URL path")
  decodeMany (b::bs) = (bs,) <$> decode b

export
decodeAll :
     SnocList a
  -> Decode a
  -> List ByteString
  -> Either DecodeErr (List ByteString,SnocList a)
decodeAll sx d []        = Right ([],sx)
decodeAll sx d (x :: xs) =
  case decode @{d} x of
    Right v  => decodeAll (sx:<v) d xs
    Left err => Left err

export
Decode a => DecodeMany (SnocList a) where
  simulateDecode bs = Just []
  decodeMany = decodeAll [<] %search

export
Decode a => DecodeMany (List a) where
  simulateDecode bs = Just []
  decodeMany bs = map (<>> []) <$> decodeAll [<] %search bs

--------------------------------------------------------------------------------
-- DecodeVia
--------------------------------------------------------------------------------

namespace DecodeVia
  public export
  interface DecodeVia (0 from, to : Type) where
    fromBytes  : ByteString -> Either DecodeErr from
    decodeFrom : from -> Either DecodeErr to
    mediaType  : String

export
decodeVia : (d : DecodeVia from to) => ByteString -> Either DecodeErr to
decodeVia bs = fromBytes @{d} bs >>= decodeFrom

export
FromJSON a => DecodeVia JSON a where
  fromBytes  = mapFst (invalidBody "JSON value") . parseBytes json Virtual
  decodeFrom = mapFst (invalidBody "JSON value" . JErr) . fromJSON
  mediaType  = "application/json"
