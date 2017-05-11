module Data.ByteString.Builder.Prim
  ( primFixed
  , primBounded
  , primMapArrayBounded
  , primUnfoldrBounded
  , primMapListBounded
  , char7
  , charUtf8
  , module Data.ByteString.Builder.Prim.Types
  , module Data.ByteString.Builder.Prim.Binary
  ) where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Rec.Class (tailRecM2, Step(..))

import Data.Array as A
import Data.Int.Bits ((.&.), shr)
import Data.Char (toCharCode)
import Data.ByteString.Internal (Octet)
import Data.ArrayBuffer.TypedArray (plusPtr, pokeByteOff)
import Data.ByteString.Builder.Internal
  (Builder, BuildStep, BufferRange(..), ensureFree, builder, bufferFull)
import Data.ByteString.Builder.Prim.Types
  ( FixedPrim, BoundedPrim, size, runFP, emptyFP, liftToBounded, boundedPrim, sizeBound
  , runBP, emptyBP )
import Data.ByteString.Builder.Prim.Types hiding (FixedPrim, BoundedPrim) as I
import Data.ByteString.Builder.Prim.Binary
  ( int8BE, int16BE, int32BE, int8LE, int16LE , int32LE, uint8BE, uint16BE, uint32BE
  , uint8LE, uint16LE, uint32LE)
import Data.Functor.Contravariant ((>$<))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))


primFixed :: forall a. FixedPrim a -> (a -> Builder)
primFixed = primBounded <<< I.liftToBounded

primBounded :: forall a. BoundedPrim a -> (a -> Builder)
primBounded w x = ensureFree (I.sizeBound w) <> builder step
  where
  step :: forall r. BuildStep r -> BuildStep r
  step k (BufferRange op ope) = do
    op' <- I.runBP w x op
    let br' = BufferRange op' ope
    k br'

primMapListBounded :: forall a. BoundedPrim a -> L.List a -> Builder
primMapListBounded w xs0 = builder (step xs0)
  where
  step :: forall r. L.List a -> BuildStep r -> BuildStep r
  step xs1 k (BufferRange op0 ope0) = tailRecM2 go xs1 op0
    where
    go L.Nil op = Done <$> k (BufferRange op ope0)
    go xs@(L.Cons y ys) op
      | op `plusPtr` bound <= ope0 = Loop <<< { a: ys, b:_ } <$> I.runBP w y op
      | otherwise                  = pure $ Done (bufferFull bound op (step xs k))

    bound = I.sizeBound w

primMapArrayBounded :: forall a. BoundedPrim a -> Array a -> Builder
primMapArrayBounded w xs0 = builder (step xs0)
  where
  step :: forall r. Array a -> BuildStep r -> BuildStep r
  step xs1 k (BufferRange op0 ope0) = tailRecM2 go xs1 op0
    where
    go ys op = case A.uncons ys of
      Nothing -> Done <$> k (BufferRange op ope0)
      Just { head, tail }
        | op `plusPtr` bound <= ope0 -> Loop <<< { a: tail, b:_ } <$> I.runBP w head op
        | otherwise                  -> pure $ Done (bufferFull bound op (step ys k))

  bound = I.sizeBound w

primUnfoldrBounded :: forall a b. BoundedPrim b -> (a -> Maybe (Tuple b a)) -> a -> Builder
primUnfoldrBounded w f x0 = builder (fillWith x0)
  where
  fillWith :: forall r. a -> BuildStep r -> BuildStep r
  fillWith x k (BufferRange op0 ope0) = tailRecM2 go (f x) op0
    where
    go Nothing op =
      let br' = BufferRange op ope0
      in Done <$> k br'
    go (Just (Tuple y x')) op
      | op `plusPtr` bound <= ope0 = Loop <<< { a: f x', b:_ } <$> I.runBP w y op
      | otherwise = pure $ Done $ bufferFull bound op $
                      \(BufferRange opNew opeNew) -> do
                        opNew' <- I.runBP w y opNew
                        fillWith x' k (BufferRange opNew' opeNew)
  bound = I.sizeBound w

char7 :: FixedPrim Char
char7 = (\c -> toCharCode c .&. 0x7F) >$< uint8BE

charUtf8 :: BoundedPrim Char
charUtf8 = I.boundedPrim 4 (encodeCharUtf8 f1 f2 f3 f4)
  where
  pokeN n io op  = io op *> pure (op `plusPtr` n)
  f1 x1    = pokeN 1 \op -> liftEff $ pokeByteOff op 0 x1
  f2 x1 x2 = pokeN 2 $ \op -> liftEff do
    pokeByteOff op 0 x1
    pokeByteOff op 1 x2
  f3 x1 x2 x3 = pokeN 3 \op -> liftEff do
    pokeByteOff op 0 x1
    pokeByteOff op 1 x2
    pokeByteOff op 2 x3
  f4 x1 x2 x3 x4 = pokeN 4 \op -> liftEff do
    pokeByteOff op 0 x1
    pokeByteOff op 1 x2
    pokeByteOff op 2 x3
    pokeByteOff op 3 x4

encodeCharUtf8
  :: forall a
   . (Octet -> a)
  -> (Octet -> Octet -> a)
  -> (Octet -> Octet -> Octet -> a)
  -> (Octet -> Octet -> Octet -> Octet -> a)
  -> Char
  -> a
encodeCharUtf8 f1 f2 f3 f4 ch = case toCharCode ch of
  x | x <= 0x7F   -> f1 x
    | x <= 0x07FF ->
        let x1 = (x `shr` 6)  + 0xC0
            x2 = (x .&. 0x3F) + 0x80
        in f2 x1 x2
    | x <= 0xFFFF ->
        let x1 = (x `shr` 12) + 0xE0
            x2 = ((x `shr` 6) .&. 0x3F) + 0x80
            x3 = (x .&. 0x3F) + 0x80
        in f3 x1 x2 x3
    | otherwise ->
        let x1 = (x `shr` 18) + 0xF0
            x2 = ((x `shr` 12) .&. 0x3F) + 0x80
            x3 = ((x `shr` 6) .&. 0x3F) + 0x80
            x4 = (x .&. 0x3F) + 0x80
        in f4 x1 x2 x3 x4
