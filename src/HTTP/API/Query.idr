module HTTP.API.Query

import Data.SortedMap
import HTTP.API.Serve
import JSON.Simple
import public HTTP.API.Decode

%default total
%hide Data.Linear.(.)

export
infix 3 ??

public export
data QField : (t : Type) -> Type where
  (??) : (name : ByteString) -> (0 type : Type) -> QField type

public export
data RequestQuery : (ts : List Type) -> Type where
  Query : All QField ts -> RequestQuery ts

convertQ : All QField ts -> All Decode ts -> Queries -> Either DecodeErr (HList ts)
convertQ []               []        qs = Right []
convertQ ((n ?? t) :: xs) (y :: ys) qs = Prelude.do
  let Just bs := lookup n qs | Nothing => Left (Msg "Missing query parameter: '\{n}'")
  v  <- decodeAs t bs
  vs <- convertQ xs ys qs
  Right $ v::vs

public export
(all : All Decode ts) => Serve (RequestQuery ts) where
  InTypes                 = ts
  OutTypes                = []
  outs                    = []
  canHandle _ r           = True
  fromRequest (Query q) r =
    either (throw . decodeErr badRequest400) pure $ convertQ q all r.uri.queries
  adjResponse _ _ _       = pure
