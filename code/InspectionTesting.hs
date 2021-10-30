{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}

module InspectionTesting where

import Data.Aeson
import JSONSchema
import Test.Inspection

mySchema :: Value
mySchema = schema @Person

inspect $ hasNoGenerics 'mySchema

