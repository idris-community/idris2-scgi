module HTTP.API.Status

import Data.ByteString

%default total

public export
record Status where
  constructor MkStatus
  code    : Nat
  message : String

export %inline
Cast Status ByteString where
  cast (MkStatus c m) = fromString "\{show c} \{m}"

export
ok200 : Status
ok200 = MkStatus 200 "OK"

export
created201 : Status
created201 = MkStatus 201 "Created"

export
accepted202 : Status
accepted202 = MkStatus 202 "Accepted"

export
noContent204 : Status
noContent204 = MkStatus 204 "No Content"

export
badRequest400 : Status
badRequest400 = MkStatus 400 "Bad Request"

export
unauthorized401 : Status
unauthorized401 = MkStatus 401 "Unauthorized"

export
forbidden403 : Status
forbidden403 = MkStatus 403 "Forbidden"

export
notFound404 : Status
notFound404 = MkStatus 404 "Not Found"

export
methodNotAllowed405 : Status
methodNotAllowed405 = MkStatus 405 "Method Not Allowed" 

export
notAcceptable406 : Status
notAcceptable406 = MkStatus 406 "Not Acceptable" 

export
unsupportedMediaType415 : Status
unsupportedMediaType415 = MkStatus 415 "Unsupported Media Type"

export
internalServerError500 : Status
internalServerError500 = MkStatus 500 "Internal Server Error"

export
notImplemented501 : Status
notImplemented501 = MkStatus 501 "Not Implemented"
