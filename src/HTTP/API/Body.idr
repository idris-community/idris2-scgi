module HTTP.API.Body

import Data.ByteString
import Data.SortedMap
import HTTP.API.Serve
import Network.SCGI.Request

%default total

public export
interface DecodeVia (0 to, from : Type) where
  parse   : ByteString -> Maybe from
  convert : from -> Maybe to

export %inline
parseConvert : (0 from,to : Type) -> DecodeVia to from => ByteString -> Maybe to
parseConvert from to @{c} bs = parse @{c} bs >>= convert @{c}
