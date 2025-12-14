module Network.SCGI.URI

import Data.Buffer
import Data.SortedMap
import Text.ILex

%default total

escape : Bits8 -> List Bits8
escape b = 37 :: BV.unpack (fromString $ toHex b)

export
uriEscape : ByteString -> ByteString
uriEscape (BS sz bv) = go [] sz
  where
    go : List Bits8 -> (n : Nat) -> (0 p : LTE n sz) => ByteString 
    go bs 0     = pack bs
    go bs (S k) =
      case atNat bv k of
        45  => go (45::bs) k
        46  => go (46::bs) k
        95  => go (95::bs) k
        126 => go (126::bs) k
        b   => if isAlphaNum b then go (b::bs) k else go (escape b ++ bs) k

unescape : List Bits8 -> List Bits8
unescape (x::y::t) = (cast $ hexdigit x * 16 + hexdigit y) :: t
unescape bs        = bs

export
uriUnescape : ByteString -> ByteString
uriUnescape (BS sz bv) = go [] sz
  where
    go : List Bits8 -> (n : Nat) -> (0 p : LTE n sz) => ByteString 
    go bs 0     = pack bs
    go bs (S k) =
      case atNat bv k of
        37 => go (unescape bs) k
        b  => go (b::bs) k

||| Alias for a sorted map (dictionary) mapping header names to
||| header values.
public export
0 Queries : Type
Queries = SortedMap ByteString ByteString

public export
record URI where
  constructor MkURI
  path    : List ByteString
  queries : SortedMap ByteString ByteString

query : ByteString -> Maybe (SortedMap ByteString ByteString)
query = map SortedMap.fromList . traverse pair . split 38
  where
    pair : ByteString -> Maybe (ByteString,ByteString)
    pair bs =
      case break (61 ==) bs of
        (x, BS (S k) bv) => Just (uriUnescape x, uriUnescape $ BS k $ tail bv)
        _                => Nothing

dropFirst : Bits8 -> List ByteString -> List ByteString
dropFirst 47 (_::t) = t
dropFirst _  bs     = bs

parsePth : ByteString -> List ByteString
parsePth (BS 0 _)      = []
parsePth (BS (S k) bv) =
  map uriUnescape $ case last bv of
    47 => dropFirst (head bv) $ split 47 (init bv)
    _  => dropFirst (head bv) $ split 47 bv

export
parseURI : ByteString -> Maybe URI
parseURI bs =
  case split 63 bs of
    [p,q] => [| MkURI (Just $ parsePth p) (query q) |]
    [p]   => [| MkURI (Just $ parsePth p) (Just empty) |]
    _     => Nothing

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

unreserved : RExp True
unreserved = alphaNum <|> oneof ['-','.','_','~']

scheme : RExp True
scheme = alpha >> star (alphaNum <|> oneof ['+','-','.'])
