module HTTP.API.Path

import public HTTP.API.Decode
import HTTP.API.Serve

%default total

public export
data Part : Type where
  PStr       : String -> Part
  Capture    : (0 t : Type) -> Part

public export
FromString Part where fromString = PStr

public export
0 PartsTypes : List Part -> List Type
PartsTypes []                   = []
PartsTypes (PStr _       :: xs) = PartsTypes xs
PartsTypes (Capture    t :: xs) = t :: PartsTypes xs

public export
data RequestPath : List Type -> Type where
  Path : (parts : List Part) -> RequestPath (PartsTypes parts)

canHandlePath :
     (ps : List Part)
  -> All DecodeMany (PartsTypes ps)
  -> List ByteString
  -> Bool
canHandlePath [] [] [] = True
canHandlePath (PStr s :: ys) xs (p :: ps) =
  s == toString p && canHandlePath ys xs ps
canHandlePath (Capture t :: ys) (x::xs) ps =
  case simulateDecode @{x} ps of
    Just ps2 => canHandlePath ys xs ps2
    Nothing  => False
canHandlePath _ _ _ = False

convertRequest :
     (ps : List Part)
  -> All DecodeMany (PartsTypes ps)
  -> List ByteString
  -> Either DecodeErr (HList $ PartsTypes ps)
convertRequest [] []  [] = Right []
convertRequest (PStr s    :: ys) as (b::bs) = convertRequest ys as bs
convertRequest (Capture t :: ys) (a::as) bs = Prelude.do
  (bs2,v) <- decodeMany @{a} bs
  vs      <- convertRequest ys as bs2
  pure $ v::vs
convertRequest _ _ _ = Left (Msg "Unexpected end of URI path")

%inline
convertPRequest :
     RequestPath ps
  -> All DecodeMany ps
  -> List ByteString
  -> Either DecodeErr (HList ps)
convertPRequest (Path parts) = convertRequest parts

%inline
canHandleP : RequestPath ps -> All DecodeMany ps -> List ByteString -> Bool
canHandleP (Path parts) = canHandlePath parts

public export
(all : All DecodeMany ts) => Serve (RequestPath ts) where
  InTypes            = ts
  OutTypes           = []
  outs               = []
  canHandle   ps r   = canHandleP ps all r.uri.path
  adjResponse _ _  _ = pure
  fromRequest ps r   =
    either
      (throw . decodeErr badRequest400)
      pure
      (convertPRequest ps all r.uri.path)
