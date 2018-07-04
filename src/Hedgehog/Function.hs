{-# language GADTs, RankNTypes #-}
{-# language FlexibleContexts, DefaultSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
module Hedgehog.Function
  ( module GHC.Generics
  , Fn
  , apply
  , fn
  , fnWith
  , Arg(..)
  , Vary(..)
  )
where

import Data.Void (absurd)
import Data.Maybe (fromMaybe, fromJust)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Functor.Contravariant.Divisible (Divisible(..), Decidable(..), divided, chosen)
import Data.Int (Int64)
import Generics.Eot (HasEot(..), Void)
import Hedgehog.Internal.Gen (GenT(..))
import Hedgehog.Internal.Seed (Seed(..))

import qualified GHC.Generics
import qualified Hedgehog.Gen as Gen

infixr 5 :->
data a :-> c where
  Unit :: c -> () :-> c
  Nil :: Void :-> c
  Pair :: a :-> b :-> c -> (a, b) :-> c
  Lft :: a :-> c -> Either a b :-> c
  Rgt :: b :-> c -> Either a b :-> c
  App :: a :-> c -> a :-> c -> a :-> c
  Map
    :: (a -> b)
    -> (b -> a)
    -> b :-> c
    -> a :-> c

table :: a :-> c -> [(a, c)]
table (Unit c) = [((), c)]
table Nil = []
table (App a b) = table a ++ table b
table (Pair f) = do
  (a, bc) <- table f
  (b, c) <- table bc
  pure ((a, b), c)
table (Lft a) = [(Left a, c) | (a, c) <- table a]
table (Rgt b) = [(Right b, c) | (b, c) <- table b]
table (Map _ f a) = do
  (b, c) <- table a
  pure (f b, c)

showTable :: (Show a, Show b) => [(a, b)] -> String
showTable [] = "<empty function>\n"
showTable (x : xs) = unlines (showCase <$> x : xs)
  where
    showCase (lhs, rhs) = show lhs ++ " -> " ++ show rhs

class Arg a where
  build :: Monad m => (a -> GenT m c) -> GenT m (a :-> c)
  default build :: (Monad m, HasEot a, Arg (Eot a)) => (a -> GenT m c) -> GenT m (a :-> c)
  build f = Map toEot fromEot <$> build (f . fromEot)

instance Arg Void where
  build f = pure Nil

instance Arg () where
  build f = Unit <$> f ()

instance (Arg a, Arg b) => Arg (a, b) where
  build f = fmap Pair . build $ \a -> build $ \b -> f (a, b)

instance (Arg a, Arg b) => Arg (Either a b) where
  build f = App <$> (Lft <$> build (f . Left)) <*> (Rgt <$> build (f . Right))

variant :: Int64 -> GenT m b -> GenT m b
variant n (GenT f) = GenT $ \sz sd -> f sz (sd { seedValue = seedValue sd + n})

variant' :: Int64 -> CoGenT m b -> CoGenT m b
variant' n (CoGenT f) =
  CoGenT $ \a -> variant n . f a

class Vary a where
  vary :: CoGenT m a
  default vary :: (HasEot a, Vary (Eot a)) => CoGenT m a
  vary = CoGenT $ \a -> applyCoGenT vary (toEot a)

instance Vary Void where
  vary = conquer

instance Vary () where
  vary = conquer

instance (Vary a, Vary b) => Vary (Either a b) where
  vary = chosen (variant' 0 vary) (variant' 1 vary)

instance (Vary a, Vary b) => Vary (a, b) where
  vary = divided (variant' 0 vary) (variant' 1 vary)

-- CoGen ~ Op (Endo (Gen b))
newtype CoGenT m a = CoGenT { applyCoGenT :: forall b. a -> GenT m b -> GenT m b }

instance Contravariant (CoGenT m) where
  contramap f (CoGenT g) = CoGenT (g . f)

instance Divisible (CoGenT m) where
  divide f (CoGenT gb) (CoGenT gc) =
    CoGenT $ \a ->
    let (b, c) = f a in gc c . gb b
  conquer = CoGenT $ const id

instance Decidable (CoGenT m) where
  choose f (CoGenT gb) (CoGenT gc) =
    CoGenT $ \a ->
    case f a of
      Left b -> gb b . variant 0
      Right c -> gc c . variant 1
  lose f = CoGenT $ \a -> absurd (f a)

instance (Show a, Show b) => Show (a :-> b) where
  show = show . table

apply' :: a :-> b -> a -> Maybe b
apply' (Unit c) () = Just c
apply' Nil a =
  case a of
    _ -> error "impossible"
apply' (Pair f) (a, b) = do
  f' <- apply' f a
  apply' f' b
apply' (Lft f) (Left a) = apply' f a
apply' (Rgt f) (Right a) = apply' f a
apply' (App f g) a = maybe (apply' g a) Just (apply' f a)
apply' (Map f _ g) a = apply' g (f a)
apply' _ _ = Nothing

unsafeApply :: a :-> b -> a -> b
unsafeApply f = fromJust . apply' f

data Fn a b = Fn b (a :-> b)

instance (Show a, Show b) => Show (Fn a b) where
  show (Fn b a) =
    case table a of
      [] -> "_ -> " ++ show b
      ta -> showTable ta ++ "_ -> " ++ show b

shrinkFn :: a :-> b -> [a :-> b]
shrinkFn = shrinkFn' (const [])
  where
    shrinkFn' :: (b -> [b]) -> a :-> b -> [a :-> b]
    shrinkFn' shr (Unit a) = Unit <$> shr a
    shrinkFn' _ Nil = []
    shrinkFn' shr (Pair f) = Pair <$> shrinkFn' (shrinkFn' shr) f
    shrinkFn' shr (Lft f) = Lft <$> shrinkFn' shr f
    shrinkFn' shr (Rgt f) = Rgt <$> shrinkFn' shr f
    shrinkFn' shr (App a b) =
      [a, b] ++
      fmap (`App` b) (shrinkFn' shr a) ++
      fmap (a `App`) (shrinkFn' shr b)
    shrinkFn' shr (Map f g h) = Map f g <$> shrinkFn' shr h

apply :: Fn a b -> a -> b
apply (Fn b f) = fromMaybe b . apply' f

fnWith :: (Monad m, Arg a) => CoGenT m a -> GenT m b -> GenT m (Fn a b)
fnWith cg gb =
  Gen.shrink (\(Fn b f) -> Fn b <$> shrinkFn f) $
  Fn <$> gb <*> build (\a -> applyCoGenT cg a gb)

fn :: (Monad m, Arg a, Vary a) => GenT m b -> GenT m (Fn a b)
fn = fnWith vary

instance Vary Bool
instance Vary Ordering
instance Vary a => Vary (Maybe a)
instance Vary a => Vary [a]
instance Arg Bool
instance Arg Ordering
instance Arg a => Arg (Maybe a)
instance Arg a => Arg [a]
