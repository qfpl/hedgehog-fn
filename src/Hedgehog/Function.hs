-- |
-- Here's an example of how to use the library to test the "fmap composition" law.
--
-- @ScopedTypeVariables@ and @TypeApplications@ are recommended for ease of use. @RankNTypes@
-- is only necessary for this example.
--
-- @
-- {-\# language RankNTypes \#-}
-- {-\# language ScopedTypeVariables, TypeApplications \#-}
--
-- import Hedgehog
-- import Hedgehog.Function
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
--
-- map_compose
--   :: forall f a b c
--    . ( Functor f
--      , Show (f a)
--      , Show a, Arg a, Vary a
--      , Show b, Arg b, Vary b
--      , Show c
--      , Eq (f c)
--      , Show (f c)
--      )
--   => (forall x. Gen x -> Gen (f x))
--   -> Gen a
--   -> Gen b
--   -> Gen c
--   -> Property
-- map_compose genF genA genB genC =
--   property $ do
--     g <- forAllFn $ fn \@a genB
--     f <- forAllFn $ fn \@b genC
--     xs <- forAll $ genF genA
--     fmap (f . g) xs === fmap f (fmap g xs)
--
-- prop_map_list :: Property
-- prop_map_list =
--   map_compose
--     (Gen.list (Range.constant 0 100))
--     Gen.bool
--     Gen.bool
--     Gen.bool
-- @

module Hedgehog.Function
  ( module GHC.Generics
  , module Data.Functor.Contravariant
  , module Data.Functor.Contravariant.Divisible
  , Fn
  -- * Generation
  , forAllFn
  , apply
  , fn
  , fnWith
  -- * Building
  , gbuild
  , buildIntegral
  , Arg(..)
  -- * Varying
  , CoGenT
  , CoGen
  , gvary
  , varyIntegral
  , Vary(..)
  )
where

import GHC.Generics
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Hedgehog.Function.Internal
