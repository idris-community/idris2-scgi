module HTTP.API.Serve

import Data.SortedMap
import public Data.List.Quantifiers
import public HTTP.API.Decode
import public HTTP.API.Error
import public Network.SCGI.Error
import public Network.SCGI.Logging
import public Network.SCGI.Prog
import public Network.SCGI.Request
import public Network.SCGI.Response

%default total

||| A list of erased (!) types.
|||
||| This is represents the size of a heterogeneous list (it is just
||| a natural number at runtime) and is used to split off a
||| predefined prefix off a heterogeneous list.
public export
data TList : List Type -> Type where
  Nil  : TList []
  (::) : (0 t : Type) -> TList ts -> TList (t::ts)

||| Interface for building up a server API from a
||| heterogeneous list of values.
public export
interface Serve (0 a : Type) where
  0 InTypes   : List Type
  0 OutTypes  : List Type
  outs        : TList OutTypes
  canHandle   : a -> Request -> Bool
  fromRequest : a -> Request -> Handler (HList InTypes)
  adjResponse : a -> HList OutTypes -> Request -> Response -> Handler Response

public export
data Sing : List a -> Type where
  IsSing : Sing [v]

public export
getSing : (as : List a) -> Sing as => a
getSing [v] = v

public export
0 AllInTypes : All Serve ts -> List Type
AllInTypes []       = []
AllInTypes (x :: t) = InTypes @{x} ++ AllInTypes t

public export
0 AllOutTypes : All Serve ts -> List Type
AllOutTypes []       = []
AllOutTypes (x :: t) = OutTypes @{x} ++ AllOutTypes t

public export
0 Fun : List Type -> Type -> Type
Fun []        r = r
Fun (t :: ts) r = t -> Fun ts r

public export
0 API : (al : All Serve ts) -> Sing (AllOutTypes al) => Type
API al = Fun (AllInTypes al) (Handler (getSing $ AllOutTypes al))

getIns :
     (all : All Serve ts)
  -> HList ts
  -> Request
  -> Handler (HList $ AllInTypes all)
getIns []      []      req = pure []
getIns (a::as) (v::vs) req = Prelude.do
  rs  <- fromRequest @{a} v req
  rem <- getIns as vs req 
  pure (rs ++ rem)

splitHList : TList ts -> HList (ts ++ rem) -> (HList ts, HList rem)
splitHList []        vs      = ([],vs)
splitHList (t :: ts) (v::vs) =
  let (xs,ys) := splitHList ts vs
   in (v::xs,ys)

wrap : (0 ts : List Type) -> (0 prf : Sing ts) => getSing ts -> HList ts
wrap [t] @{IsSing} x = [x]

applyAPI :
     HList ts
  -> (0 os : List Type)
  -> {auto 0 prf : Sing os}
  -> Fun ts (Handler (getSing os))
  -> Handler (HList os)
applyAPI []        os r = map (wrap os) r
applyAPI (v :: vs) os f = applyAPI vs os (f v)

putOuts :
     (all : All Serve ts)
  -> HList ts
  -> HList (AllOutTypes all)
  -> Request
  -> Response
  -> Handler Response
putOuts []        []      [] req resp = pure resp
putOuts (a :: as) (x::xs) vs req resp = Prelude.do
  let (ts,rem) := splitHList (outs @{a}) vs
  r2 <- adjResponse @{a} x ts req resp
  putOuts as xs rem req r2

namespace APIs

  public export
  data APIs : Type where
    Nil  : APIs
    (::) : {0 ts : List Type} -> HList ts -> APIs -> APIs

namespace Server

  public export
  data Server : APIs -> Type where
    Nil  : Server []
    (::) :
         {0 ts       : List Type}
      -> {0 as       : APIs}
      -> {hl         : HList ts}
      -> {auto all   : All Serve ts}
      -> {auto 0 prf : Sing (AllOutTypes all)}
      -> API all
      -> Server as
      -> Server (hl :: as)

canServe : {auto all : All Serve ts} -> HList ts -> Request -> Bool
canServe @{[]}    []      req = True
canServe @{s::ss} (v::vs) req = canHandle @{s} v req && canServe vs req

serve1 :
     {auto all   : All Serve ts}
  -> {auto 0 prf : Sing (AllOutTypes all)}
  -> (api        : HList ts)
  -> API all
  -> Request
  -> Async Poll [] Response
serve1 @{all} api f req = 
  handleErrors (pure . fromError req) $ Prelude.do
    ins  <- getIns all api req
    outs <- applyAPI ins (AllOutTypes all) f
    putOuts all api outs req empty

export
serveAll :
     (0 apis   : APIs)
  -> Server apis
  -> Request
  -> Async Poll [] Response
serveAll []         []              req = ?notfound -- pure notFound
serveAll (hl :: as) ((::) {hl} f x) req =
  case canServe hl req of
    True  => serve1 hl f req
    False => serveAll as x req
