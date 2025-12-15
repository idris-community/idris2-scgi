module HTTP.API.Path

import Data.ByteString
import HTTP.API.Serve
import Text.ILex

%default total

public export
interface Decode (0 a : Type) where
  decode : ByteString -> Maybe a

export %inline
Decode ByteString where decode = Just

export %inline
Decode String where decode = Just . toString

export %inline
Decode Integer where
  decode (BS 0 _) = Nothing
  decode bs       = if all isDigit bs then Just (decimal bs) else Nothing

export %inline
Decode Nat where
  decode = map integerToNat . decode

public export
data Part : Type where
  PStr : String -> Part
  Capture : (0 t : Type) -> Decode t => Part

public export
FromString Part where fromString = PStr

public export
0 PartsTypes : List Part -> List Type
PartsTypes []                = []
PartsTypes (PStr _    :: xs) = PartsTypes xs
PartsTypes (Capture t :: xs) = t :: PartsTypes xs

public export
data RequestPath : List Type -> Type where
  Path : (parts : List Part) -> RequestPath (PartsTypes parts)

convertRequest :
     (ps : List Part)
  -> List ByteString
  -> (Maybe $ HList $ PartsTypes ps)
convertRequest []                []      = Just []
convertRequest (PStr s    :: ys) (b::bs) =
  if fromString s == b then convertRequest ys bs else Nothing
convertRequest (Capture t :: ys) (b::bs) =
  [| decode {a = t} b :: convertRequest ys bs |]
convertRequest _                 _       = Nothing

convertPRequest : RequestPath ps -> List ByteString -> Maybe $ HList ps
convertPRequest (Path parts) = convertRequest parts

public export
Serve (RequestPath ts) where
  InTypes  = ts
  OutTypes = []
  outs     = []
  fromRequest ps r = pure $ convertPRequest ps r.uri.path
  adjResponse _ _  = pure
