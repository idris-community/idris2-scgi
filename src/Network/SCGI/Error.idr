module Network.SCGI.Error

%default total

--------------------------------------------------------------------------------
--          Error
--------------------------------------------------------------------------------

public export
data SCGIErr : Type where
  InvalidRequest : SCGIErr
  LargeHeader    : (max : Nat) -> SCGIErr
  LargeBody      : (max : Nat) -> SCGIErr

export
Interpolation SCGIErr where
  interpolate InvalidRequest  = "Invalid request sent by web-server"
  interpolate (LargeHeader n) =
    "The maximum header size of \{show n} bytes was exceeded"
  interpolate (LargeBody n)   =
    "The maximum content size of \{show n} bytes was exceeded"
