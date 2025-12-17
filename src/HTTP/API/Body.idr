module HTTP.API.Body

import Data.ByteString
import Data.SortedMap
import HTTP.API.Serve
import JSON.Simple

%default total

public export
data ReqBody : (formats : List Type) -> (val : Type) -> Type where
  Body : (0 fs : List Type) -> (0 res : Type) -> ReqBody fs res

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

decodeBody : All (`DecodeVia` t) ts -> Request -> Either RequestErr (HList [t])
decodeBody []        r = Left $ requestErr unsupportedMediaType415
decodeBody (d :: ds) r =
  case hasContentType r (mediaType @{d}) of
    False => decodeBody ds r
    True  =>
     bimap
       (decodeErr unsupportedMediaType415)
       (\x => [x])
       (decodeVia @{d} r.content)

public export
(all : All (`DecodeVia` t) ts) => Serve (ReqBody ts t) where
  InTypes              = [t]
  OutTypes             = []
  outs                 = %search
  canHandle   _ r      = True
  fromRequest _ r      = injectEither $ decodeBody all r
  adjResponse _ [] req = pure
