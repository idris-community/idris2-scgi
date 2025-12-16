module HTTP.API.Query

import Data.SortedMap
import HTTP.API.Serve
import public HTTP.API.Decode

%default total

export
infix 3 ??

public export
data QField : (t : Type) -> Type where
  (??) : (name : ByteString) -> (0 type : Type) -> QField type

public export
data RequestQuery : (ts : List Type) -> Type where
  Query : All QField ts -> RequestQuery ts

convertQ : All QField ts -> All Decode ts -> Queries -> Maybe (HList ts)
convertQ []               []        qs = Just []
convertQ ((n ?? t) :: xs) (y :: ys) qs = Prelude.do
  bs <- lookup n qs
  v  <- decodeAs t bs
  vs <- convertQ xs ys qs
  Just $ v::vs

public export
(all : All Decode ts) => Serve (RequestQuery ts) where
  InTypes                 = ts
  OutTypes                = []
  outs                    = []
  fromRequest (Query q) r = pure $ convertQ q all r.uri.queries
  adjResponse _ _ _       = pure
