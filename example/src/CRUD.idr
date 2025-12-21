module CRUD

import Data.Linear.Ref1
import Data.SortedMap
import HTTP.API.Server
import ID
import JSON.Simple

%default total

public export
SCGI : (ps : List Part) -> ReqPath
SCGI ps = Path ("scgi-example"::ps)

public export
CRUDEndpoints : (0 new, val, patch : Type) -> (name : String) -> Endpoints
CRUDEndpoints new val patch name =
  [ [SCGI [PStr name, Capture (ID val)], Get [JSON] val]
  , [SCGI [PStr name], Get [JSON] (List val)]
  , [SCGI [PStr name, Capture (ID val)], Delete]
  , [SCGI [PStr name, Capture (ID val)], JSONContent patch, Patch']
  , [SCGI [PStr name], JSONContent new, Post [JSON] (ID val)]
  ]

parameters (0 new, val, patch : Type)
           (name     : String)
           {auto tjv : ToJSON val}
           {auto fjn : FromJSON new}
           {auto fjp : FromJSON patch}
           (doPatch  : patch -> val -> Either RequestErr val)
           (mkVal    : new -> ID val -> Either RequestErr val)
           (ref      : IORef (IDMap val))

  getVal : ID val -> Handler val
  getVal x = Prelude.do
    Just v <- lookup x <$> readref ref | _ => throw (requestErr notFound404)
    pure v

  getVals : Handler (List val)
  getVals = values <$> readref ref

  deleteVal : ID val -> Handler ()
  deleteVal x =
    update ref (delval x $ requestErr notFound404) >>= injectEither

  patchVal : ID val -> patch -> Handler ()
  patchVal x p =
    update ref (modval x (doPatch p) $ requestErr notFound404) >>= injectEither

  newVal : new -> Handler (ID val)
  newVal n = update ref (newval $ mkVal n) >>= injectEither

  export
  crudServer : Server (CRUDEndpoints new val patch name)
  crudServer = [getVal, getVals, deleteVal, patchVal, newVal]
