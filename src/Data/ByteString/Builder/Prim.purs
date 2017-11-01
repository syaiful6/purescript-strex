module Data.ByteString.Builder.Prim
  ( primFixed
  , primBounded
  , primMapArrayBounded
  , primMapStringBounded
  , primUnfoldrBounded
  , primMapListBounded
  , char8
  , charUtf8
  , module Data.ByteString.Builder.Prim.Types
  , module Data.ByteString.Builder.Prim.Binary
  ) where

import Prelude

import Control.Monad.Eff (whileE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.ST (newSTRef, readSTRef, writeSTRef, runST)
import Control.Monad.Rec.Class (tailRecM2, Step(..))

import Data.Array as A
import Data.Int.Bits ((.&.), shr)
import Data.Char (toCharCode)
import Data.ByteString.Internal (plusPtr, pokeByteOff, memcpyArr)
import Data.ByteString.Builder.Internal
  (Builder, BuildStep, BufferRange(..), ensureFree, builder, bufferFull, refinedEff)
import Data.ByteString.Builder.Prim.Types
  ( FixedPrim, BoundedPrim, size, runFP, emptyFP, liftToBounded, boundedPrim, sizeBound
  , runBP, emptyBP, condB )
import Data.ByteString.Builder.Prim.Types hiding (FixedPrim, BoundedPrim) as I
import Data.ByteString.Builder.Prim.Binary
  ( int8BE, int16BE, int32BE, int8LE, int16LE , int32LE, uint8BE, uint16BE, uint32BE
  , uint8LE, uint16LE, uint32LE)
import Data.Function.Uncurried as Fn
import Data.Functor.Contravariant ((>$<))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.String (length) as S
import Data.String.Unsafe (charAt) as S
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

primFixed :: forall a. FixedPrim a -> a -> Builder
primFixed = primBounded <<< I.liftToBounded

primBounded :: forall a. BoundedPrim a -> a -> Builder
primBounded w x = ensureFree (I.sizeBound w) <> builder step
  where
  step :: forall r. BuildStep r -> BuildStep r
  step k (BufferRange op ope) = do
    op' <- Fn.runFn2 (I.runBP w) x op
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
      | op `plusPtr` bound <= ope0 = Loop <<< { a: ys, b:_ } <$> Fn.runFn2 (I.runBP w) y op
      | otherwise                  = pure $ Done (bufferFull bound op (step xs k))

    bound = I.sizeBound w

primMapArrayBounded :: forall a. BoundedPrim a -> Array a -> Builder
primMapArrayBounded w xs0 = builder (step 0)
  where
  lenxs = A.length xs0
  step :: forall r. Int -> BuildStep r -> BuildStep r
  step i k (BufferRange op0 ope0)=
    if lenxs <= i
      then k (BufferRange op0 ope0)
      else
      liftEff $ runST do
        ix <- newSTRef i
        op <- newSTRef op0
        loop <- newSTRef true
        whileE (readSTRef loop) do
          i' <- readSTRef ix
          op1 <- readSTRef op
          op2 <- refinedEff (Fn.runFn2 (I.runBP w) (unsafePartial (A.unsafeIndex xs0 i')) op1)
          let ix' = i' + 1
          unless (lenxs > ix' && op2 `plusPtr` bound <= ope0) (void $ writeSTRef loop false)
          _ <- writeSTRef op op2
          void $ writeSTRef ix ix'
        ic <- readSTRef ix
        op2 <- readSTRef op
        if lenxs > ic
          then pure (bufferFull bound op2 (step ic k))
          else refinedEff (k (BufferRange op2 ope0))
  bound = I.sizeBound w

primMapStringBounded :: BoundedPrim Char -> String -> Builder
primMapStringBounded w str = builder (step 0)
  where
  lenxs = S.length str
  bound = I.sizeBound w
  step :: forall r. Int -> BuildStep r -> BuildStep r
  step i k (BufferRange op0 ope0)=
    if lenxs <= i
      then k (BufferRange op0 ope0)
      else
      liftEff $ runST do
        ix <- newSTRef i
        op <- newSTRef op0
        loop <- newSTRef true
        whileE (readSTRef loop) do
          i' <- readSTRef ix
          op1 <- readSTRef op
          op2 <- refinedEff (Fn.runFn2 (I.runBP w) (unsafePartial (S.charAt i' str)) op1)
          let ix' = i' + 1
          unless (lenxs > ix' && op2 `plusPtr` bound <= ope0) (void $ writeSTRef loop false)
          _ <- writeSTRef op op2
          void $ writeSTRef ix ix'
        ic <- readSTRef ix
        op2 <- readSTRef op
        if lenxs > ic
          then pure (bufferFull bound op2 (step ic k))
          else refinedEff (k (BufferRange op2 ope0))

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
      | op `plusPtr` bound <= ope0 = Loop <<< { a: f x', b:_ } <$> Fn.runFn2 (I.runBP w) y op
      | otherwise = pure $ Done $ bufferFull bound op $
                      \(BufferRange opNew opeNew) -> do
                        opNew' <- Fn.runFn2 (I.runBP w) y opNew
                        fillWith x' k (BufferRange opNew' opeNew)
  bound = I.sizeBound w

char8 :: FixedPrim Char
char8 = toCharCode >$< uint8BE

charUtf8 :: BoundedPrim Char
charUtf8 = I.boundedPrim 4 step
  where
  step = Fn.mkFn2 \chr op -> liftEff $ case toCharCode chr of
    x | x <= 0x7F -> do
          pokeByteOff op 0 x
          pure (op `plusPtr` 1)
      | x <= 0x07FF -> do
          let x1 = (x `shr` 6)  + 0xC0
              x2 = (x .&. 0x3F) + 0x80
          memcpyArr op [x1, x2]
          pure (op `plusPtr` 2)
      | x <= 0xFFFF -> do
          let x1 = (x `shr` 12) + 0xE0
              x2 = ((x `shr` 6) .&. 0x3F) + 0x80
              x3 = (x .&. 0x3F) + 0x80
          memcpyArr op [x1, x2, x2]
          pure (op `plusPtr` 3)
      | otherwise -> do
          let x1 = (x `shr` 18) + 0xF0
              x2 = ((x `shr` 12) .&. 0x3F) + 0x80
              x3 = ((x `shr` 6) .&. 0x3F) + 0x80
              x4 = (x .&. 0x3F) + 0x80
          memcpyArr op [x1, x2, x3, x4]
          pure (op `plusPtr` 4)
