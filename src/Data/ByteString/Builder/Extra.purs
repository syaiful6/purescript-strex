module Data.ByteString.Builder.Extra
  ( BufferWriter
  , Next(..)
  , runBuilder
  , BuilderPopper
  , BuilderRecv
  , BuilderFinish
  , BufferAllocStrategy
  , newByteStringBuilderRecv
  , allNewBuffersStrategy
  , reuseBufferStrategy
  , module Data.ByteString.Builder.Internal) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)

import Data.ArrayBuffer.TypedArray (Ptr, Uint8, plusPtr, minusPtr)
import Data.ByteString.Builder.Internal
  ( Builder, toLazyByteStringWith, byteStringCopy, byteStringInsert, byteStringThreshold
  , AllocationStrategy, safeStrategy, untrimmedStrategy, flush
  , UNREFINED, RefinedEff, refinedEff, unsafePerformRefined)
import Data.ByteString as S
import Data.ByteString.Builder.Internal as I
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

type BufferWriter = Ptr Uint8 -> Int -> RefinedEff (Tuple Int Next)

-- | After running a 'BufferWriter' action there are three possibilities for
-- | what comes next:
data Next
  -- | This means we're all done. All the builder data has now been written.
  =  Done

  -- | This indicates that there may be more data to write. It
  -- gives you the next 'BufferWriter' action. You should call that action
  -- with an appropriate buffer. The int indicates the /minimum/ buffer size
  -- required by the next 'BufferWriter' action. That is, if you call the next
  -- action you /must/ supply it with a buffer length of at least this size.
  | More   Int          BufferWriter

  -- | In addition to the data that has just been written into your buffer
  -- by the 'BufferWriter' action, it gives you a pre-existing chunk
  -- of data as a 'S.ByteString'. It also gives you the following 'BufferWriter'
  -- action. It is safe to run this following action using a buffer with as
  -- much free space as was left by the previous run action.
  | Chunk  S.ByteString BufferWriter

-- | Turn a 'Builder' into its initial 'BufferWriter' action.
--
runBuilder :: Builder -> BufferWriter
runBuilder = run <<< I.runBuilder
  where
    bytesWritten startPtr endPtr = endPtr `minusPtr` startPtr

    run :: I.BuildStep Unit -> BufferWriter
    run step = \buf len ->
      let doneH endPtr _ =
            let wc  = bytesWritten buf endPtr
                next = Done
             in pure (Tuple wc next)

          bufferFullH endPtr minReq step' =
            let wc   = bytesWritten buf endPtr
                next = More minReq (run step')
             in pure (Tuple wc next)

          insertChunkH endPtr bs step' =
            let wc  = bytesWritten buf endPtr
                next = Chunk bs (run step')
             in pure (Tuple wc next)

          br = I.BufferRange buf (buf `plusPtr` len)

      in I.fillWithBuildStep step doneH bufferFullH insertChunkH br

type BuilderPopper = RefinedEff S.ByteString

type BuilderRecv = Builder -> RefinedEff BuilderPopper

type BuilderFinish = RefinedEff (Maybe S.ByteString)

type BufferAllocStrategy = Tuple (RefinedEff I.Buffer) (Int -> I.Buffer -> RefinedEff (RefinedEff I.Buffer))

newByteStringBuilderRecv :: BufferAllocStrategy -> RefinedEff (Tuple BuilderRecv BuilderFinish)
newByteStringBuilderRecv (Tuple ioBufInit nextBuf) = do
  refBuf <- liftEff (newRef ioBufInit)
  pure (Tuple (push refBuf) (finish refBuf))

  where
  finish refBuf = do
    ioBuf <- liftEff $ readRef refBuf
    buf <- ioBuf
    pure $ I.nonEmptyByteStringFromBuffer buf

  push refBuf bldr = do
    refWri <- liftEff $ newRef (Left (runBuilder bldr))
    pure $ popper refBuf refWri

  popper refBuf refWri = do
    ioBuf <- liftEff $ readRef refBuf
    ebWri <- liftEff $ readRef refWri
    case ebWri of
      Left bWri -> do
        buf@I.Buffer _ (I.BufferRange op ope) <- ioBuf
        Tuple bytes next <- bWri op (ope `minusPtr` op)
        let op' = op `plusPtr` bytes
        case next of
          Done -> do
            liftEff $ writeRef refBuf (pure (I.updateFilledPart buf op'))
            pure $ S.empty
          More minSize bWri' -> do
            let buf' = I.updateFilledPart buf op'
                cont mbs = do
                  ioBuf' <- nextBuf minSize buf'
                  _ <- liftEff do
                    writeRef refBuf ioBuf'
                    writeRef refWri $ Left bWri'
                  case mbs of
                    Just bs | not (S.null bs) -> pure bs
                    _ -> popper refBuf refWri
            cont $ I.nonEmptyByteStringFromBuffer buf'
          Chunk bs bWri' -> do
            let buf'    = I.updateFilledPart buf op'
                yieldBS = do
                  n <- nextBuf 1 buf'
                  _ <- liftEff $ writeRef refBuf n
                  _ <- liftEff $ writeRef refWri (Left bWri')
                  if S.null bs
                    then popper refBuf refWri
                    else pure bs
            case I.nonEmptyByteStringFromBuffer buf' of
              Nothing -> yieldBS
              Just bs' -> do
                _ <- liftEff $ writeRef refWri $ Right yieldBS
                pure bs'
      Right action -> action

allNewBuffersStrategy :: Int -> BufferAllocStrategy
allNewBuffersStrategy size =
  Tuple
    (liftEff $ I.newBuffer size)
    (\reqSize _ -> pure (liftEff $ I.newBuffer (max reqSize size)))

reuseBufferStrategy :: forall eff. Eff eff I.Buffer -> BufferAllocStrategy
reuseBufferStrategy buf0 = Tuple (liftEff buf0) tryReuseBuffer
  where
  tryReuseBuffer reqSize buf
    | I.bufferSize buf >= reqSize = pure $ pure (I.reuseBuffer buf)
    | otherwise = pure $ liftEff $ I.newBuffer reqSize
