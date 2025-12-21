module Derive

import public Derive.Barbie
import public JSON.Simple.Derive
import public Control.Barbie
import public Derive.Prelude
import public JSON.Simple

%default total

export
jsonOptions : Options
jsonOptions =
  MkOptions {
    sum                        = ObjectWithSingleField
  , unwrapUnary                = False
  , replaceMissingKeysWithNull = True
  , unwrapRecords              = True
  , constructorTagModifier     = id
  , fieldNameModifier          = id
  }

||| Interfaces to derive for a regular CyBy sum- or product type
export
ExampleBarbie : List (List Name -> ParamTypeInfo -> Res (List TopLevel))
ExampleBarbie =
  [ ShowVis Export
  , EqVis Export
  , customFromJSON Export jsonOptions
  , customToJSON Export jsonOptions
  , BarbieVis Export
  ]
