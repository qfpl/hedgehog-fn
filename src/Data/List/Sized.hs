{-# language GADTs, DataKinds, KindSignatures, PolyKinds, StandaloneDeriving #-}
{-# language ExistentialQuantification #-}
{-# language ConstraintKinds #-}
module Data.List.Sized where

import Prelude hiding (replicate)
import Data.Semigroup (Semigroup(..))
import GHC.Generics (Generic)

data Nat = Z | S Nat
data SNat :: Nat -> * where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

data ExistsN c f a where
  ExistsN :: c (Sized n a) => f n -> ExistsN c f a

data Sized :: Nat -> a -> * where
  Empty :: Sized 'Z a
  Cons :: a -> Sized n a -> Sized ('S n) a
deriving instance Eq a => Eq (Sized n a)
deriving instance Show a => Show (Sized n a)

instance Functor (Sized n) where
  fmap f Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable (Sized n) where
  foldr f z Empty = z
  foldr f z (Cons x xs) = x `f` foldr f z xs

replicate :: SNat n -> a -> Sized n a
replicate SZ a = Empty
replicate (SS n) a = Cons a $ replicate n a

snoc :: Sized n a -> a -> Sized ('S n) a
snoc Empty a = Cons a Empty
snoc (Cons x xs) a = Cons x $ snoc xs a

uncons :: Sized ('S n) a -> (a, Sized n a)
uncons (Cons x xs) = (x, xs)
