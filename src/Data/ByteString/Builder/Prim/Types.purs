module Data.ByteString.Builder.Prim.Types
  ( FixedPrim
  , fixedPrim
  , size
  , runFP
  , emptyFP
  , liftToBounded

  , BoundedPrim
  , boundedPrim
  , sizeBound
  , runBP
  , emptyBP
  , condB
  ) where

import Prelude

import Data.ArrayBuffer.Types (Uint8)
import Data.ByteString.Internal (Ptr, plusPtr)
import Data.ByteString.Builder.Internal (RefinedEff)
import Data.Functor.Contravariant (class Contravariant)
import Data.Decide (class Decide, choose)
import Data.Divide (class Divide)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Function.Uncurried as Fn

-- | A builder primitive that always results in a sequence of bytes of a
-- | pre-determined, fixed size.
data FixedPrim a = FP Int (Fn.Fn2 a (Ptr Uint8) (RefinedEff Unit))

size :: forall a. FixedPrim a -> Int
size (FP s _) = s

fixedPrim :: forall a. Int -> Fn.Fn2 a (Ptr Uint8) (RefinedEff Unit) -> FixedPrim a
fixedPrim = FP

runFP :: forall a. FixedPrim a -> (Fn.Fn2 a (Ptr Uint8) (RefinedEff Unit))
runFP (FP _ eff) = eff

emptyFP :: forall a. FixedPrim a
emptyFP = FP 0 (Fn.mkFn2 \_ _ -> pure unit)

liftToBounded :: forall a. FixedPrim a -> BoundedPrim a
liftToBounded (FP l k) = BP l (Fn.mkFn2 \x op -> Fn.runFn2 k x op $> op `plusPtr` l)

-- | A builder primitive that always results in sequence of bytes that is no longer
-- | than a pre-determined bound.
data BoundedPrim a = BP Int (Fn.Fn2 a (Ptr Uint8) (RefinedEff (Ptr Uint8)))

boundedPrim :: forall a. Int -> Fn.Fn2 a (Ptr Uint8) (RefinedEff (Ptr Uint8)) -> BoundedPrim a
boundedPrim = BP

sizeBound :: forall a. BoundedPrim a -> Int
sizeBound (BP b _) = b

emptyBP :: forall a. BoundedPrim a
emptyBP = BP 0 $ Fn.mkFn2 \_ op -> pure op

runBP :: forall a. BoundedPrim a -> (Fn.Fn2 a (Ptr Uint8) (RefinedEff (Ptr Uint8)))
runBP (BP _ eff) = eff

condB :: forall a. (a -> Boolean) -> BoundedPrim a -> BoundedPrim a -> BoundedPrim a
condB f = choose (\a -> if f a then Left a else Right a)

instance contravariantFixedPrim :: Contravariant FixedPrim where
  cmap f (FP b k) = FP b (Fn.mkFn2 \a p -> Fn.runFn2 k (f a) p)

instance divideFixedPrim :: Divide FixedPrim where
  divide k (FP l f) (FP j g) = FP (l + j) (Fn.mkFn2 \a op -> case k a of
    Tuple a' b' -> Fn.runFn2 f a' op *> Fn.runFn2 g b' (op `plusPtr` l))

instance contravariantBoundedPrim :: Contravariant BoundedPrim where
  cmap f (BP l k) = BP l (Fn.mkFn2 \a p -> Fn.runFn2 k (f a) p)

instance divideBoundedPrim :: Divide BoundedPrim where
  divide k (BP l f) (BP j g) = BP (l + j) (Fn.mkFn2 \a op -> case k a of
    Tuple a' b' -> Fn.runFn2 f a' op >>= \i -> Fn.runFn2 g b' i)

instance decideBoundedPrim :: Decide BoundedPrim where
  choose f (BP l fb) (BP j fc) = BP (max l j) (Fn.mkFn2 \a op -> case f a of
    Left a'  -> Fn.runFn2 fb a' op
    Right b' -> Fn.runFn2 fc b' op)
