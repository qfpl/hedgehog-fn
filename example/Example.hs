{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
{-# language RankNTypes #-}
module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog.Function

fun_idempotent
  :: forall m a
  . (Monad m, Arg a, Vary a, Eq a, Show a)
  => Gen a
  -> PropertyT m ()
fun_idempotent ga = do
  a <- forAll ga
  f <- fmap apply . forAll $ fn @a ga
  f a === f (f a)

prop_unit_fun_idempotent :: Property
prop_unit_fun_idempotent =
  property $
    fun_idempotent $ Gen.choice [Right <$> Gen.bool :: Gen (Either () Bool)]

-- | map (f . g) xs = map f (map g xs)
map_compose
  :: forall f a b c
   . ( Functor f
     , Show (f a)
     , Show a, Arg a, Vary a
     , Show b, Arg b, Vary b
     , Show c
     , Eq (f c)
     , Show (f c)
     )
  => (forall x. Gen x -> Gen (f x))
  -> Gen a
  -> Gen b
  -> Gen c
  -> Property
map_compose genF genA genB genC =
  property $ do
    g <- forAllFn $ fn @a genB
    f <- forAllFn $ fn @b genC
    xs <- forAll $ genF genA
    fmap (f . g) xs === fmap f (fmap g xs)

prop_map_list :: Property
prop_map_list =
  map_compose
    (Gen.list (Range.constant 0 100))
    Gen.bool
    Gen.bool
    Gen.bool

main :: IO Bool
main = checkParallel $$(discover)
