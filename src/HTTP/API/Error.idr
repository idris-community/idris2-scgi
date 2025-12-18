module HTTP.API.Error

import Data.List.Quantifiers
import Derive.Prelude
import HTTP.API.Encode
import HTTP.API.Status
import JSON.Simple
import JSON.Simple.Derive

%default total
%language ElabReflection

public export
record RequestErr where
  constructor RE
  status  : Nat
  error   : String 
  message : String
  details : String
  path    : String

%runElab derive "RequestErr" [Show,Eq,FromJSON,ToJSON]

export
Interpolation RequestErr where
  interpolate (RE s e m d p) =
    """
    Error Details:
    status  : \{show s}
    error   : \{e}
    message : \{m}
    details : \{d}
    path    : \{p}
    """

public export
data ErrorContext : Type where
  Hdr : (name : String) -> ErrorContext 
  Qry : (name : String) -> ErrorContext 
  Pth : (name : String) -> ErrorContext
  Bdy : ErrorContext

%runElab derive "ErrorContext" [Show,Eq,FromJSON,ToJSON]

public export
data DecodeErr : Type where
  DE  : (type, value, details : Maybe String) -> DecodeErr
  Msg : (message : String) -> DecodeErr

%runElab derive "DecodeErr" [Show,Eq,FromJSON,ToJSON]

export
invalidVal : (type, value : String) -> DecodeErr
invalidVal type value = DE (Just type) (Just value) Nothing

export
invalidBody : (type : String) -> Interpolation a => a -> DecodeErr
invalidBody type = DE (Just type) Nothing . Just . interpolate 

tpeString : Maybe String -> String
tpeString = fromMaybe "value"

valString : Maybe String -> String
valString = maybe "" (\x => ": \{x}")

export
Interpolation DecodeErr where
  interpolate (DE t s _) = "Invalid \{tpeString t}\{valString s}."
  interpolate (Msg msg)  = msg

--------------------------------------------------------------------------------
-- Error Utilities
--------------------------------------------------------------------------------

export
requestErr : Status -> RequestErr
requestErr (MkStatus c e) = RE c e "" "" ""

export
decodeErr : Status -> DecodeErr -> RequestErr
decodeErr s de@(DE _ _ $ Just d) =
  {message := "\{de}", details := d} (requestErr s)
decodeErr s de = {message := "\{de}"} (requestErr s)
