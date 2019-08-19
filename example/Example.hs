{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Hedgehog
import qualified Hedgehog.Gen      as Gen
import qualified Hedgehog.Range    as Range
import           Hedgehog.Function

funIdempotent
  :: forall m a
  . (Monad m, Arg a, Vary a, Eq a, Show a)
  => Gen a
  -> PropertyT m ()
funIdempotent genA = do
  a <- forAll genA
  f <- forAllFn $ fn @a genA
  f a === f (f a)

prop_unit_funIdempotent :: Property
prop_unit_funIdempotent =
  property $
    funIdempotent $ Gen.choice [Right <$> Gen.bool :: Gen (Either () Bool)]

funCongEquality
  :: forall m a
  . (Monad m, Arg a, Vary a, Eq a, Show a)
  => Gen a
  -> Gen a
  -> PropertyT m ()
funCongEquality genA genB = do
  a <- forAll genA
  b <- forAll genB
  f <- forAllFn $ fn @a genA
  if a == b
    then f a === f b
    else pure ()

prop_funCongEquality :: Property
prop_funCongEquality =
  property $
    funCongEquality (Gen.int (Range.linear 1 10)) (Gen.int (Range.linear 1 10))

-- | map (f . g) xs = map f (map g xs)
mapCompose
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
mapCompose genF genA genB genC =
  property $ do
    g <- forAllFn $ fn @a genB
    f <- forAllFn $ fn @b genC
    xs <- forAll $ genF genA
    fmap (f . g) xs === fmap f (fmap g xs)

prop_map_list :: Property
prop_map_list =
  mapCompose
    (Gen.list (Range.constant 0 100))
    Gen.bool
    Gen.bool
    Gen.bool

main :: IO Bool
main = checkParallel $$discover
