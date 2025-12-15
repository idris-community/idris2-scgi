module HTTP.API.Serve

import public Data.List.Quantifiers
import public Network.SCGI.Error
import public Network.SCGI.Logging
import public Network.SCGI.Prog
import public Network.SCGI.Request
import public Network.SCGI.Response

%default total

public export
data TList : List Type -> Type where
  Nil  : TList []
  (::) : (0 t : Type) -> TList ts -> TList (t::ts)

public export
interface Serve (0 a : Type) where
  0 InTypes   : List Type
  0 OutTypes  : List Type
  outs        : TList OutTypes
  fromRequest : a -> Request -> SCGIProg ServerErrs (Maybe $ HList InTypes)
  adjResponse : a -> HList OutTypes -> Response -> SCGIProg ServerErrs Response

public export
0 AllInTypes : All Serve ts -> List Type
AllInTypes []       = []
AllInTypes (x :: t) = InTypes @{x} ++ AllInTypes t

public export
0 AllOutTypes : All Serve ts -> List Type
AllOutTypes []       = []
AllOutTypes (x :: t) = OutTypes @{x} ++ AllOutTypes t

public export
0 API : All Serve ts -> Type
API ts = HList (AllInTypes ts) -> SCGIProg ServerErrs (HList $ AllOutTypes ts)

getIns :
     (all : All Serve ts)
  -> HList ts
  -> Request
  -> SCGIProg ServerErrs (Maybe $ HList $ AllInTypes all)
getIns []      []      req = pure (Just [])
getIns (a::as) (v::vs) req = Prelude.do
  Just rs  <- fromRequest @{a} v req | _ => pure Nothing
  Just rem <- getIns as vs req       | _ => pure Nothing
  pure (Just $ rs ++ rem)

splitHList : TList ts -> HList (ts ++ rem) -> (HList ts, HList rem)
splitHList []        vs      = ([],vs)
splitHList (t :: ts) (v::vs) =
  let (xs,ys) := splitHList ts vs
   in (v::xs,ys)

putOuts :
     (all : All Serve ts)
  -> HList ts
  -> HList (AllOutTypes all)
  -> Response
  -> SCGIProg ServerErrs Response
putOuts []        []      [] resp = pure resp
putOuts (a :: as) (x::xs) vs resp = Prelude.do
  let (ts,rem) := splitHList (outs @{a}) vs
  oa <- adjResponse @{a} x ts resp
  putOuts as xs rem oa

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
         {0 ts     : List Type}
      -> {0 as     : APIs}
      -> {hl       : HList ts}
      -> {auto all : All Serve ts}
      -> API all
      -> Server as
      -> Server (hl :: as)

serve1 :
     {auto all : All Serve ts}
  -> {auto log : Logger}
  -> (api      : HList ts)
  -> API all
  -> Request
  -> SCGIProg ServerErrs (Maybe Response)
serve1 @{all} api f req = Prelude.do
  Just ins  <- getIns all api req | _ => pure Nothing
  outs      <- f ins
  Just <$> putOuts all api outs ok

export
serveAll :
     {auto log : Logger}
  -> (0 apis   : APIs)
  -> Server apis
  -> Request
  -> SCGIProg ServerErrs Response
serveAll []         []              req = pure notFound
serveAll (hl :: as) ((::) {hl} f x) req = Prelude.do
  Just res <- serve1 hl f req | _ => serveAll as x req
  pure res


data Foo = MkFoo

data Bar = MkBar

Serve Foo where
  InTypes       = [Nat]
  OutTypes      = []
  outs          = %search
  fromRequest _ _ = pure (Just [12])
  adjResponse _ _ = pure

msg : Bool -> ByteString
msg True  = "Goobye World\n"
msg False = "Hello World\n"

Serve Bar where
  InTypes                = [Bool,String]
  OutTypes               = [Bool]
  outs                   = %search
  fromRequest _ _        = pure (Just [False, "bar"])
  adjResponse _ [b] resp = pure $ {content := [msg b]} (addHeader plain resp)

runTest : Logger => HList [Nat,Bool,String] -> SCGIProg ServerErrs (HList [Bool])
runTest [n,b,s] = Prelude.do
  info "Number is: \{show n}"
  info "String is: \{s}"
  pure [not b]

0 TestServer : APIs
TestServer = [[MkFoo,MkBar]]

export
test : Logger => Request -> SCGIProg ServerErrs Response
test = serveAll TestServer [runTest]
