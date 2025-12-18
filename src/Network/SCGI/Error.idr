module Network.SCGI.Error

import IO.Async.Posix
import public HTTP.API.Error
import public HTTP.API.Status

%default total

export
largeBody : Nat -> RequestErr
largeBody n =
  {message := "Maximum content size is \{show n} bytes"} $
    requestErr contentTooLarge413

export
largeHeader : Nat -> RequestErr
largeHeader n =
  {message := "Maximum header size is \{show n} bytes"} $
    requestErr requestHeaderFieldsTooLarge431

export
badRequest : RequestErr
badRequest = requestErr badRequest400
