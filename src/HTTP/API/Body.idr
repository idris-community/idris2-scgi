module HTTP.API.Body

import Data.ByteString
import Data.SortedMap
import HTTP.API.Decode
import HTTP.API.Serve
import Network.SCGI.Request

%default total

public export
data ReqBody : (formats : List Type) -> (val : Type) -> Type where
  Body : (0 fs : List Type) -> (0 res : Type) -> ReqBody fs res

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

decodeBody : All (`DecodeVia` t) ts -> Request -> Maybe (HList [t])
decodeBody []        r = Nothing
decodeBody (d :: ds) r =
  if hasContentType r (mediaType @{d})
     then (\x => [x]) <$> decodeVia @{d} r.content
     else decodeBody ds r

public export
(all : All (`DecodeVia` t) ts) => Serve (ReqBody ts t) where
  InTypes  = [t]
  OutTypes = []
  outs     = %search
  fromRequest m r = pure $ decodeBody all r
  adjResponse m [] req = pure
