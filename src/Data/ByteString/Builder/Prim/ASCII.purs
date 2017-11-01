module Data.ByteString.Builder.Prim.ASCII
  ( char7
  , int8Dec
  , int16Dec
  , int32Dec
  , word8Dec
  , word16Dec
  , word32Dec
  , word8Hex
  , word16Hex
  , word32Hex
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Data.ArrayBuffer.Types (Uint8Array, Uint8)
import Data.ByteString.Internal (Ptr(..))
import Data.ByteString.Builder.Internal (RefinedEff)
import Data.ByteString.Builder.Prim.Types (BoundedPrim, FixedPrim, boundedPrim)
import Data.ByteString.Builder.Prim.Binary (uint8BE)
import Data.Char (toCharCode)
import Data.Functor.Contravariant ((>$<))
import Data.Function.Uncurried as Fn
import Data.Int.Bits ((.&.))

encodeIntDecimal :: Fn.Fn2 Int (Ptr Uint8) (RefinedEff (Ptr Uint8))
encodeIntDecimal = Fn.mkFn2 \val (Ptr off av) -> liftEff do
  { buffer, offset } <- Fn.runFn3 _intDecEff val off av
  pure (Ptr offset buffer)

char7 :: FixedPrim Char
char7 = (\c -> toCharCode c .&. 0x7F) >$< uint8BE

-- | The input should be in range -128 to 127
int8Dec :: BoundedPrim Int
int8Dec = boundedPrim 4 encodeIntDecimal

-- |
int16Dec :: BoundedPrim Int
int16Dec = boundedPrim 6 encodeIntDecimal

int32Dec :: BoundedPrim Int
int32Dec = boundedPrim 11 encodeIntDecimal

word8Dec :: BoundedPrim Int
word8Dec = boundedPrim 3 encodeUintDecimal

word16Dec :: BoundedPrim Int
word16Dec = boundedPrim 5 encodeUintDecimal

word32Dec :: BoundedPrim Int
word32Dec = boundedPrim 10 encodeUintDecimal

encodeUintDecimal :: Fn.Fn2 Int (Ptr Uint8) (RefinedEff (Ptr Uint8))
encodeUintDecimal = Fn.mkFn2 \val (Ptr off av) -> liftEff do
  { buffer, offset } <- Fn.runFn3 _uintDecEff val off av
  pure (Ptr offset buffer)

--------------------------------------------------------------------------------
-- Hexadecimal encoding --------------------------------------------------------
--------------------------------------------------------------------------------
word8Hex :: BoundedPrim Int
word8Hex = boundedPrim 2 encodeWordHex

word16Hex :: BoundedPrim Int
word16Hex = boundedPrim 4 encodeWordHex

word32Hex :: BoundedPrim Int
word32Hex = boundedPrim 8 encodeWordHex

encodeWordHex :: Fn.Fn2 Int (Ptr Uint8) (RefinedEff (Ptr Uint8))
encodeWordHex = Fn.mkFn2 \val (Ptr off av) -> liftEff do
  { buffer, offset } <- Fn.runFn3 _uintHexEff val off av
  pure (Ptr offset buffer)

foreign import _intDecEff
  :: forall eff
   . Fn.Fn3 Int Int Uint8Array (Eff eff ({ buffer :: Uint8Array, offset :: Int }))

foreign import _uintDecEff
  :: forall eff
   . Fn.Fn3 Int Int Uint8Array (Eff eff ({ buffer :: Uint8Array, offset :: Int }))

foreign import _uintHexEff
  :: forall eff
   . Fn.Fn3 Int Int Uint8Array (Eff eff ({ buffer :: Uint8Array, offset :: Int }))