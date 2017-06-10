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

import Data.ArrayBuffer.TypedArray (Ptr, Uint8, plusPtr)
import Data.ByteString.Builder.Internal (RefinedEff)
import Data.Functor.Contravariant (class Contravariant)
import Data.Decide (class Decide, choose)
import Data.Divide (class Divide)
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..))

-- | A builder primitive that always results in a sequence of bytes of a
-- | pre-determined, fixed size.
data FixedPrim a = FP Int (a -> Ptr Uint8 -> RefinedEff Unit)

size :: forall a. FixedPrim a -> Int
size (FP s _) = s

fixedPrim :: forall a. Int -> (a -> Ptr Uint8 -> RefinedEff Unit) -> FixedPrim a
fixedPrim = FP

runFP :: forall a. FixedPrim a -> a -> Ptr Uint8 -> RefinedEff Unit
runFP (FP _ eff) = eff

emptyFP :: forall a. FixedPrim a
emptyFP = FP 0 (\_ _ -> pure unit)

liftToBounded :: forall a. FixedPrim a -> BoundedPrim a
liftToBounded (FP l k) = BP l (\x op -> k x op $> op `plusPtr` l)

-- | A builder primitive that always results in sequence of bytes that is no longer
-- | than a pre-determined bound.
data BoundedPrim a = BP Int (a -> Ptr Uint8 -> RefinedEff (Ptr Uint8))

boundedPrim :: forall a. Int -> (a -> Ptr Uint8 -> RefinedEff (Ptr Uint8)) -> BoundedPrim a
boundedPrim = BP

sizeBound :: forall a. BoundedPrim a -> Int
sizeBound (BP b _) = b

emptyBP :: forall a. BoundedPrim a
emptyBP = BP 0 \_ op -> pure op

runBP :: forall a. BoundedPrim a -> a -> Ptr Uint8 -> RefinedEff (Ptr Uint8)
runBP (BP _ eff) = eff

condB :: forall a. (a -> Boolean) -> BoundedPrim a -> BoundedPrim a -> BoundedPrim a
condB f = choose (\a -> if f a then Left a else Right a)

instance contravariantFixedPrim :: Contravariant FixedPrim where
  cmap f (FP b k) = FP b (k <<< f)

instance divideFixedPrim :: Divide FixedPrim where
  divide k (FP l f) (FP j g) = FP (l + j) \a op -> case k a of
    Tuple a' b' -> f a' op *> g b' (op `plusPtr` l)

instance contravariantBoundedPrim :: Contravariant BoundedPrim where
  cmap f (BP l k) = BP l (k <<< f)

instance divideBoundedPrim :: Divide BoundedPrim where
  divide k (BP l f) (BP j g) = BP (l + j) \a op -> case k a of
    Tuple a' b' -> f a' op >>= g b'

instance decideBoundedPrim :: Decide BoundedPrim where
  choose f (BP l fb) (BP j fc) = BP (max l j) (either fb fc <<< f)
