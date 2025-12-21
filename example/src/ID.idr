module ID

import Data.SortedMap
import Derive.Prelude
import HTTP.API.Decode
import JSON.Simple.Derive

%default total
%language ElabReflection

public export
record ID (t : Type) where
  constructor MkID
  value : Nat

%runElab deriveIndexed "ID" [Show,Eq,Ord,FromInteger,FromJSON,ToJSON]

export
next : SortedMap (ID k) v -> ID k
next m =
  case rightMost m of
    Nothing         => MkID 0
    Just (MkID v,_) => MkID (1+v)

export
Decode (ID t) where
  decode = map MkID . decode

public export
0 IDMap : Type -> Type
IDMap t = SortedMap (ID t) t

export %inline
delval : ID v -> Lazy e -> IDMap v -> (IDMap v, Either e ())
delval x err m =
  maybe (m,Left err) (const $ (delete x m, Right ())) (lookup x m)

export %inline
modval : ID v -> (v -> Either e v) -> Lazy e -> IDMap v -> (IDMap v, Either e ())
modval x ptch err m =
  case lookup x m of
    Nothing => (m, Left err)
    Just v  => case ptch v of
      Left x   => (m, Left x)
      Right v2 => (insert x v2 m, Right ())

export %inline
newval : (ID v -> Either e v) -> IDMap v -> (IDMap v, Either e $ ID v)
newval new m =
 let x := next m
  in case new x of
       Left x   => (m, Left x)
       Right v2 => (insert x v2 m, Right x)
