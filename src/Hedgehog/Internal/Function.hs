{-# language DataKinds #-}
{-# language GADTs, RankNTypes #-}
{-# language FlexibleContexts, DefaultSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language LambdaCase #-}
{-# language TypeApplications #-}
{-# language EmptyCase #-}
{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds, KindSignatures #-}
module Hedgehog.Internal.Function where

import Data.Constraint (Dict(..))
import Data.Functor.Contravariant (Contravariant(..))
import Data.Functor.Contravariant.Divisible (Divisible(..), Decidable(..), divided, chosen)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (uncons)
import Data.Maybe (fromMaybe, fromJust)
import Data.Semigroup ((<>))
import Data.Void (Void, absurd)
import Hedgehog.Internal.Gen (GenT(..))
import Hedgehog.Internal.Seed (Seed(..))
import Data.Proxy (Proxy)
import Data.Kind (Constraint)

import GHC.Generics

import Data.List.Sized
import qualified Hedgehog.Gen as Gen

infixr 5 :->
data a :-> c where
  Unit :: c -> () :-> c
  Nil :: a :-> c
  Pair :: a :-> b :-> c -> (a, b) :-> c
  Sum :: a :-> c -> b :-> c -> Either a b :-> c
  Map
    :: (a -> b)
    -> (b -> a)
    -> b :-> c
    -> a :-> c

table :: a :-> c -> [(a, c)]
table (Unit c) = [((), c)]
table Nil = []
table (Pair f) = do
  (a, bc) <- table f
  (b, c) <- table bc
  pure ((a, b), c)
table (Sum a b) = [(Left a, c) | (a, c) <- table a] ++ [(Right b, c) | (b, c) <- table b]
table (Map _ f a) = do
  (b, c) <- table a
  pure (f b, c)

showTable :: (Show a, Show b) => [(a, b)] -> String
showTable [] = "<empty function>\n"
showTable (x : xs) = unlines (showCase <$> x : xs)
  where
    showCase (lhs, rhs) = show lhs ++ " -> " ++ show rhs

class GArg a where
  gbuild' :: Monad m => (a x -> GenT m c) -> GenT m (a x :-> c)

via
  :: Monad m
  => ((b -> GenT m c) -> GenT m (b :-> c))
  -> (a -> b)
  -> (b -> a)
  -> (a -> GenT m c)
  -> GenT m (a :-> c)
via bld ab ba f = Map ab ba <$> bld (f . ba)

instance GArg V1 where
  gbuild' = via build (\case) absurd

instance GArg U1 where
  gbuild' = via build (\U1 -> ()) (\() -> U1)

instance (GArg a, GArg b) => GArg (a :*: b) where
  gbuild' =
    via
      (\f -> fmap Pair . gbuild' $ \a -> gbuild' $ \b -> f (a, b))
      (\(a :*: b) -> (a, b))
      (\(a, b) -> a :*: b)

instance (GArg a, GArg b) => GArg (a :+: b) where
  gbuild' =
    via
      (\f ->
         Sum <$>
         gbuild' (f . Left) <*>
         gbuild' (f . Right))
      (\case; L1 a -> Left a; R1 a -> Right a)
      (either L1 R1)

instance GArg c => GArg (M1 a b c) where
  gbuild' f = Map unM1 M1 <$> gbuild' (f . M1)

instance Arg b => GArg (K1 a b) where
  gbuild' f =
    Gen.recursive
      Gen.choice
      [ pure Nil ]
      [ Map unK1 K1 <$> build (f . K1) ]

{-# inline gbuild #-}
gbuild :: (Monad m, Generic a, GArg (Rep a)) => (a -> GenT m c) -> GenT m (a :-> c)
gbuild f = Map from to <$> gbuild' (f . to)

class Arg a where
  build :: Monad m => (a -> GenT m c) -> GenT m (a :-> c)
  default build :: (Monad m, Generic a, GArg (Rep a)) => (a -> GenT m c) -> GenT m (a :-> c)
  build = gbuild

toBools :: Integral a => SNat n -> a -> (Bool, Sized n Bool)
toBools sn n
  | n >= 0 = (True, go sn n)
  | otherwise = (False, go sn $ -n - 1)
  where
    go :: Integral a => SNat n -> a -> Sized n Bool
    go sn n =
      case sn of
        SZ -> Empty
        SS k
          | n == 0 -> Data.List.Sized.replicate sn False
          | otherwise ->
              let
                (q, r) = quotRem n 2
              in
                go k q `snoc` (if toInteger r == 1 then True else False)

fromBools :: Integral a => SNat n -> (Bool, Sized n Bool) -> a
fromBools sn (pos, bts)
  | pos = go bts
  | otherwise = negate $ go bts + 1
  where
    go =
      snd .
      foldr
        (\a (pow, val) -> (pow+1, if a then val + 2 ^ pow else val))
        (0, 0)

intSNat :: forall a. Arg a => Int -> ExistsN Arg SNat a
intSNat 0 = ExistsN SZ
intSNat n =
  case intSNat (n-1) :: ExistsN Arg SNat a of
    ExistsN n' -> ExistsN (SS n')

{-# inline buildIntegral #-}
-- | Width of the integral type
buildIntegral
  :: forall m a c
   . (Monad m, Arg a, Integral a)
  => Int
  -> (a -> GenT m c)
  -> GenT m (a :-> c)
buildIntegral width f =
  case intSNat width :: ExistsN Arg SNat Bool of
    ExistsN (sn :: SNat n) ->
      Map (toBools sn) (fromBools sn) <$> build (f . fromBools sn)

variant :: Int64 -> GenT m b -> GenT m b
variant n (GenT f) = GenT $ \sz sd -> f sz (sd { seedValue = seedValue sd + n})

variant' :: Int64 -> CoGenT m b -> CoGenT m b
variant' n (CoGenT f) =
  CoGenT $ \a -> variant n . f a

class GVary a where
  gvary' :: CoGenT m (a x)

instance GVary V1 where
  gvary' = conquer

instance GVary U1 where
  gvary' = conquer

instance (GVary a, GVary b) => GVary (a :+: b) where
  gvary' =
    choose
      (\case; L1 a -> Left a; R1 a -> Right a)
      (variant' 0 gvary')
      (variant' 1 gvary')

instance (GVary a, GVary b) => GVary (a :*: b) where
  gvary' =
    divide
      (\(a :*: b) -> (a, b))
      (variant' 0 gvary')
      (variant' 1 gvary')

instance GVary c => GVary (M1 a b c) where
  gvary' = contramap unM1 gvary'

instance Vary b => GVary (K1 a b) where
  gvary' = contramap unK1 vary

{-# inline gvary #-}
gvary :: (Generic a, GVary (Rep a)) => CoGenT m a
gvary = CoGenT $ \a -> applyCoGenT gvary' (from a)

class Vary a where
  vary :: CoGenT m a
  default vary :: (Generic a, GVary (Rep a)) => CoGenT m a
  vary = gvary

{-# inline varyIntegral #-}
varyIntegral :: Integral a => CoGenT m a
varyIntegral = CoGenT $ variant . fromIntegral

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
apply' Nil _ = Nothing
apply' (Pair f) (a, b) = do
  f' <- apply' f a
  apply' f' b
apply' (Sum f _) (Left a) = apply' f a
apply' (Sum _ g) (Right a) = apply' g a
apply' (Map f _ g) a = apply' g (f a)

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
    shrinkFn' shr (Sum a b) =
      [ Sum a Nil, Sum Nil b ] ++
      fmap (`Sum` b) (shrinkFn' shr a) ++
      fmap (a `Sum`) (shrinkFn' shr b)
    shrinkFn' shr (Map f g h) = Map f g <$> shrinkFn' shr h

apply :: Fn a b -> a -> b
apply (Fn b f) = fromMaybe b . apply' f

fnWith :: (Monad m, Arg a) => CoGenT m a -> GenT m b -> GenT m (Fn a b)
fnWith cg gb =
  Fn <$> gb <*> Gen.shrink shrinkFn (build $ \a -> applyCoGenT cg a gb)

fn :: (Arg a, Vary a, Monad m) => GenT m b -> GenT m (Fn a b)
fn = fnWith vary

instance Vary ()
instance (Vary a, Vary b) => Vary (Either a b)
instance (Vary a, Vary b) => Vary (a, b)
instance Vary Void
instance Vary Bool
instance Vary Ordering
instance Vary a => Vary (Maybe a)
instance Vary a => Vary [a]
instance Vary Int where; vary = varyIntegral
instance Vary Int8 where; vary = varyIntegral
instance Vary Int16 where; vary = varyIntegral
instance Vary Int32 where; vary = varyIntegral
instance Vary Int64 where; vary = varyIntegral
instance Vary a => Vary (Sized n a) where
  vary = CoGenT $ \a ->
    case a of
      Empty -> variant 0
      Cons x xs -> variant 1 . applyCoGenT vary xs . applyCoGenT vary x

instance Arg Void where; build f = pure Nil
instance Arg () where; build f = Unit <$> f ()

instance (Arg a, Arg b) => Arg (a, b) where
  build f = fmap Pair . build $ \a -> build $ \b -> f (a, b)

instance (Arg a, Arg b) => Arg (Either a b) where
  build f =
    Sum <$>
    build (f . Left) <*>
    build (f . Right)

instance Arg Bool
instance Arg Ordering
instance Arg a => Arg (Maybe a)
instance Arg a => Arg [a]
instance Arg Int where; build = buildIntegral 31
instance Arg Int8 where; build = buildIntegral 7
instance Arg Int16 where; build = buildIntegral 15
instance Arg Int32 where; build = buildIntegral 32
instance Arg Int64 where; build = buildIntegral 63

instance Arg a => Arg (Sized 'Z a) where
  build = via build (\case; Empty -> ()) (\() -> Empty)

instance (Arg a, Arg (Sized n a)) => Arg (Sized ('S n) a) where
  build = via build (Data.List.Sized.uncons) (uncurry Cons)
