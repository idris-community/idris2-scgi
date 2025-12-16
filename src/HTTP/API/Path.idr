module HTTP.API.Path

import public HTTP.API.Decode
import HTTP.API.Serve

%default total

public export
data Part : Type where
  PStr : String -> Part
  Capture : (0 t : Type) -> Part

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
  -> All DecodeMany (PartsTypes ps)
  -> List ByteString
  -> (Maybe $ HList $ PartsTypes ps)
convertRequest [] []  [] = Just []
convertRequest (PStr s    :: ys) as (b::bs) =
  if fromString s == b then convertRequest ys as bs else Nothing
convertRequest (Capture t :: ys) (a::as) bs = Prelude.do
  (bs2,v) <- decodeMany @{a} bs
  vs      <- convertRequest ys as bs2
  pure $ v::vs
convertRequest _ _ _ = Nothing

%inline
convertPRequest :
     RequestPath ps
  -> All DecodeMany ps
  -> List ByteString
  -> Maybe $ HList ps
convertPRequest (Path parts) = convertRequest parts

public export
(all : All DecodeMany ts) => Serve (RequestPath ts) where
  InTypes            = ts
  OutTypes           = []
  outs               = []
  fromRequest ps r   = pure $ convertPRequest ps all r.uri.path
  adjResponse _ _  _ = pure
