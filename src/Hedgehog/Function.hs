module Hedgehog.Function
  ( module GHC.Generics
  , Fn
  , apply
  , fn
  , fnWith
  , gbuild
  , buildIntegral
  , Arg(..)
  , CoGenT
  , gvary
  , varyIntegral
  , Vary(..)
  )
where

import GHC.Generics
import Hedgehog.Function.Internal
