{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

fun_idempotent
  :: forall m a
  . (Monad m, Arg a, Vary a, Eq a, Show a)
  => Gen a
  -> PropertyT m ()
fun_idempotent ga = do
  f <- fmap apply' . forAll $ fn'' @a ga
  a <- forAll ga
  f a === f (f a)

prop_unit_fun_idempotent :: Property
prop_unit_fun_idempotent =
  property $
    fun_idempotent $ pure ()

main :: IO Bool
main = checkParallel $$(discover)
