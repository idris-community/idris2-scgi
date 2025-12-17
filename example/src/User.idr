module User

import Derive.Prelude
import JSON.Simple.Derive
import HTTP.API.Decode
import public JSON.Simple

%default total
%language ElabReflection

public export
record User where
  constructor MkUser
  name  : String
  email : String

%runElab derive "User" [Show,Eq,FromJSON,ToJSON]

public export
record CSV where
  constructor MkCSV
  lines : List ByteString

public export
record TSV where
  constructor MkTSV
  lines : List ByteString

sepLine : (sep : String) -> User -> ByteString
sepLine sep (MkUser n e) = fromString "\{n}\{sep}\{e}\n"

sepLines : (sep : String) -> List User -> List ByteString
sepLines sep us = fromString "name\{sep}email\n" :: map (sepLine sep) us

export
EncodeVia (List User) TSV where
  encodeAs  = MkTSV . sepLines "\t"
  toBytes   = lines
  mediaType = "text/tab-separated-values"

export
EncodeVia (List User) CSV where
  encodeAs  = MkCSV . sepLines ","
  toBytes   = lines
  mediaType = "text/csv"
