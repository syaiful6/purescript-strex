module Data.ByteString.Builder.Internal
  ( UNREFINED
  , RefinedEff
  , refinedEff
  , unsafePerformRefined

  -- Buffer
  , Buffer(..)
  , BufferRange(..)
  , bufferSize
  , newBuffer
  , byteStringFromBuffer
  , trimmedChunkFromBuffer
  , nonEmptyByteStringFromBuffer
  , reuseBuffer
  , updateFilledPart

  -- Chunk Stream
  , ChunkStream(..)
  , yield
  , chunkStreamToLBS
  , chunkStreamToLBS'
  , buildStepToCIOS
  , BuildSignal
  , BuildStep
  , finalBuildStep
  , done
  , bufferFull
  , insertChunk
  , fillWithBuildStep
  , flush

  , Builder
  , builder
  , runBuilder
  , runBuilderWith

  , ensureFree
  , byteStringCopy
  , byteStringInsert
  , byteStringThreshold

  , lazyByteStringCopy
  , lazyByteStringInsert
  , lazyByteStringThreshold

  , maximalCopySize
  , byteString
  , lazyByteString

  , toLazyByteStringWith
  , AllocationStrategy
  , customStrategy
  , safeStrategy
  , untrimmedStrategy

  , Put
  , put
  , runPut
  , putToLazyBytString
  , putToLazyByteStringWith
  , putBuilder
  , fromPut
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Lazy as Z

import Data.Lazy (defer)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))

import Data.ArrayBuffer.TypedArray (Ptr, Uint8, plusPtr, minusPtr)
import Data.ByteString as S
import Data.ByteString.Lazy.Internal
  (chunk, smallChunkSize, defaultChunkSize, empty, foldrChunks') as L
import Data.ByteString.Lazy (ByteString(..), Step(..)) as L
import Data.ByteString.Internal (create, mallocByteString, memcpy)

foreign import data UNREFINED :: Effect

newtype RefinedEff a = RefinedEff (Eff (unrefined :: UNREFINED) a)

derive newtype instance functorRefinedEff :: Functor RefinedEff
derive newtype instance applyRefinedEff :: Apply RefinedEff
derive newtype instance applicativeRefinedEff :: Applicative RefinedEff
derive newtype instance bindRefinedEff :: Bind RefinedEff
derive newtype instance monadRefinedEff :: Monad RefinedEff
derive newtype instance monadRecRefinedEff :: MonadRec RefinedEff

instance monadEffEff :: MonadEff eff RefinedEff where
  liftEff = RefinedEff <<< unsafeCoerceEff

refinedEff :: forall eff a. RefinedEff a -> Eff eff a
refinedEff (RefinedEff eff) = unsafeCoerceEff eff

unsafePerformRefined :: forall a. RefinedEff a -> a
unsafePerformRefined = unsafePerformEff <<< refinedEff

-- | A range of bytes in a buffer represented by the pointer to the first byte
-- | of the range and the pointer to the first byte /after/ the range.
data BufferRange = BufferRange (Ptr Uint8) (Ptr Uint8)

-- | A 'Buffer' together with the 'BufferRange' of free bytes. The filled
-- | space starts at offset 0 and ends at the first free byte.
data Buffer = Buffer (Ptr Uint8) BufferRange

-- | Combined size of the filled and free space in the buffer.
bufferSize :: Buffer -> Int
bufferSize (Buffer fpbuf (BufferRange _ ope)) =
  ope `minusPtr` fpbuf

-- | Resets the beginning of the next slice and the next free byte such that
-- | the whole buffer can be filled again.
reuseBuffer :: Buffer -> Buffer
reuseBuffer (Buffer fpbuf (BufferRange _ ope)) = Buffer fpbuf (BufferRange fpbuf ope)

-- | Allocate a new buffer of the given size.
newBuffer :: forall e. Int -> Eff e Buffer
newBuffer len = do
  pbuf <- mallocByteString len
  pure $ Buffer pbuf (BufferRange pbuf (pbuf `plusPtr` len))

-- | Convert the filled part of a 'Buffer' to a strict 'S.ByteString'.
byteStringFromBuffer :: Buffer -> S.ByteString
byteStringFromBuffer (Buffer fpbuf (BufferRange op _)) =
  S.ByteString fpbuf 0 (op `minusPtr` fpbuf)

nonEmptyByteStringFromBuffer :: Buffer -> Maybe S.ByteString
nonEmptyByteStringFromBuffer buf@(Buffer fpbuf (BufferRange op _))
  | (op `minusPtr` fpbuf) <= 0 = Nothing
  | otherwise                  = Just $ byteStringFromBuffer buf

updateFilledPart :: Buffer -> Ptr Uint8 -> Buffer
updateFilledPart (Buffer fpbuf (BufferRange _ pe)) op = Buffer fpbuf (BufferRange op pe)

-- | Prepend the filled part of a 'Buffer' to a lazy 'L.ByteString'
-- | trimming it if necessary.
trimmedChunkFromBuffer :: AllocationStrategy -> Buffer -> L.ByteString -> L.ByteString
trimmedChunkFromBuffer alloc buf k = go alloc
  where
  go (AllocationStrategy _ _ trim)
    | S.null bs                           = k
    | trim (S.length bs) (bufferSize buf) = L.chunk (S.copy bs) k
    | otherwise                           = L.chunk bs k
  bs = byteStringFromBuffer buf

--------------------------------------------------------------------------------
-- Chunked Stream --------------------------------------------------------------
--------------------------------------------------------------------------------

data ChunkStream a
  = Finished Buffer a
  | Yield S.ByteString (RefinedEff (ChunkStream a))

-- | A smart constructor for yielding one chunk that ignores the chunk if
-- it is empty.
yield :: forall a. S.ByteString -> RefinedEff (ChunkStream a) -> RefinedEff (ChunkStream a)
yield bs cs
  | S.null bs = cs
  | otherwise = pure $ Yield bs cs

chunkStreamToLBS :: AllocationStrategy -> L.ByteString -> ChunkStream Unit -> L.ByteString
chunkStreamToLBS strategy k = go
  where
  go (Finished buf _) = trimmedChunkFromBuffer strategy buf k
  go (Yield bs eff)    =
    L.ByteString $ defer \_ -> L.Chunk bs (unsafePerformRefined (go <$> eff))

chunkStreamToLBS'
  :: forall a b
   . AllocationStrategy
  -> (a -> Tuple b L.ByteString)
  -> ChunkStream a
  -> Tuple b L.ByteString
chunkStreamToLBS' strategy k = go
  where
    go (Finished buf x) = map (trimmedChunkFromBuffer strategy buf) $ k x
    go (Yield bs eff)   = map (L.chunk bs) $ (unsafePerformRefined (go <$> eff))

------------------------------------------------------------------------------
-- Build signals
------------------------------------------------------------------------------
type BuildStep a = BufferRange -> RefinedEff (BuildSignal a)

data BuildSignal a
  = Done (Ptr Uint8) a
  | BufferFull Int (Ptr Uint8) (BuildStep a)
  | InsertChunk (Ptr Uint8) S.ByteString (BuildStep a)

-- | Signal that the current 'BuildStep' is done and has computed a value. The first
-- | arguments is next free byte in current 'BufferRange'
done :: forall a. Ptr Uint8 -> a -> BuildSignal a
done = Done

-- | Signal that the current buffer is full. The first and seconds arguments is Minimal size
-- | of next 'BufferRange' and Next free byte in current 'BufferRange'.
bufferFull :: forall a. Int -> Ptr Uint8 -> BuildStep a -> BuildSignal a
bufferFull = BufferFull

-- | Signal that a 'S.ByteString' chunk should be inserted directly.
insertChunk :: forall a. Ptr Uint8 -> S.ByteString -> BuildStep a -> BuildSignal a
insertChunk = InsertChunk

-- | Fill a 'BufferRange' using a 'BuildStep'.
fillWithBuildStep
  :: forall a b
   . BuildStep a
  -- ^ Build step to use for filling the 'BufferRange'
  -> (Ptr Uint8 -> a -> RefinedEff b)
  -- ^ Handling the 'done' signal
  -> (Ptr Uint8 -> Int -> BuildStep a -> RefinedEff b)
  -- ^ Handling the 'bufferFull' signal
  -> (Ptr Uint8 -> S.ByteString -> BuildStep a -> RefinedEff b)
  -- ^ Handling the 'insertChunk' signal
  -> BufferRange
  -> RefinedEff b
fillWithBuildStep step fDone fFull fChunk br = do
  sig <- step br
  case sig of
    Done op x                      -> fDone op x
    BufferFull minSize op nextStep -> fFull op minSize nextStep
    InsertChunk op bs nextStep     -> fChunk op bs nextStep

-- | The builder. It's a function that take BuildStep and return another BuildStep.
-- | In other word, it's a function that fills a 'BufferRange', calls the continuation
-- | with the updated 'BufferRange' once its done, and signals its caller how to
-- | proceed using 'done', 'bufferFull', or 'insertChunk'.
newtype Builder = Builder (forall r. BuildStep r -> BuildStep r)

-- | The smart constructor of Builder
builder :: (forall r. BuildStep r -> BuildStep r) -> Builder
builder = Builder

-- | The final build step that returns the 'done' signal.
finalBuildStep ::BuildStep Unit
finalBuildStep (BufferRange op _) = pure $ Done op unit

-- | Run a 'Builder' with the 'finalBuildStep'.
runBuilder :: Builder -> BuildStep Unit
runBuilder b = runBuilderWith b finalBuildStep

-- | Run a 'Builder'.
runBuilderWith :: forall a. Builder -> BuildStep a -> BuildStep a
runBuilderWith (Builder b) = b

emptyBuilder :: Builder
emptyBuilder = Builder (\k -> (\r -> k r))

appendBuilder :: Builder -> Builder -> Builder
appendBuilder (Builder a) (Builder b) = Builder (a <<< b)

instance semigroupBuilder :: Semigroup Builder where
  append = appendBuilder

instance monoidBuilder :: Monoid Builder where
  mempty = emptyBuilder

instance lazyBuilder :: Z.Lazy Builder where
  defer f = Builder (\k r -> runBuilderWith (f unit) k r)

-- | Flush the current buffer. This introduces a chunk boundary.
flush :: Builder
flush = builder (\k (BufferRange op _) -> pure (insertChunk op S.empty k))

-- | Put monad.
newtype Put a = Put (forall r. (a -> BuildStep r) -> BuildStep r)

put :: forall a. (forall r. (a -> BuildStep r) -> BuildStep r) -> Put a
put = Put

unPut :: forall a. Put a -> (forall r. (a -> BuildStep r) -> BuildStep r)
unPut (Put a) = a

runPut :: forall a. Put a -> BuildStep a
runPut (Put k) = k (\x (BufferRange op _) -> pure (Done op x))

putBuilder :: Builder -> Put Unit
putBuilder (Builder b) = Put \k -> b (k unit)

fromPut :: Put Unit -> Builder
fromPut (Put p) = Builder \k -> p (\_ -> k)

instance functorPut :: Functor Put where
  map f p = Put \k -> unPut p (\x -> k (f x))

instance applyPut :: Apply Put where
  apply (Put f) (Put a) = Put \k -> f (\f' -> a (\a' -> k (f' a')))

instance applicativePut :: Applicative Put where
  pure a = Put \k -> k a

instance bindPut :: Bind Put where
  bind (Put m) f = Put \k -> m (\m' -> unPut (f m') k)

instance monadPut :: Monad Put

-- | Execute a 'Put' and return the computed result and the bytes
-- | written during the computation as a lazy 'L.ByteString'.
putToLazyBytString :: forall a. Put a -> Tuple a L.ByteString
putToLazyBytString = putToLazyByteStringWith
  (safeStrategy L.smallChunkSize L.defaultChunkSize) (\x -> Tuple x L.empty)

-- | Execute a 'Put' with a buffer-allocation strategy and a continuation.
putToLazyByteStringWith
  :: forall a b
   . AllocationStrategy
  -> (a -> Tuple b L.ByteString)
  -> Put a
  -> Tuple b L.ByteString
putToLazyByteStringWith strategy k p =
  chunkStreamToLBS' strategy k $ unsafePerformRefined $ buildStepToCIOS strategy (runPut p)

-- | Ensure that there are at least 'n' free bytes for the following 'Builder'
ensureFree :: Int -> Builder
ensureFree minFree = builder step
  where
    step :: (forall r. BuildStep r -> BuildStep r)
    step k br@(BufferRange op ope)
      | ope `minusPtr` op < minFree = pure (bufferFull minFree op k)
      | otherwise                   = k br

-- | Copy the bytes from a 'BufferRange' into the output stream.
wrappedBytesCopyStep :: forall a. BufferRange -> BuildStep a -> BuildStep a
wrappedBytesCopyStep (BufferRange ip0 ipe) k = go ip0
  where
    go ip (BufferRange op ope) =
      let outRemaining = ope `minusPtr` op
          inpRemaining = ipe `minusPtr` ip
      in if inpRemaining <= outRemaining
        then do
          _ <- liftEff $ memcpy op ip inpRemaining
          let br' = BufferRange (op `plusPtr` inpRemaining) ope
          k br'
        else do
          _ <- liftEff $ memcpy op ip outRemaining
          let ip' = ip `plusPtr` outRemaining
          pure $ bufferFull 1 ope (go ip')

byteStringThreshold :: Int -> S.ByteString -> Builder
byteStringThreshold maxCopySize bs' = builder (step bs')
  where
    step :: S.ByteString -> (forall r. BuildStep r -> BuildStep r)
    step bs@(S.ByteString _ _ len) k br@(BufferRange op _)
      | len <= maxCopySize = byteStringCopyStep bs k br
      | otherwise          = pure $ insertChunk op bs k

-- | Construct a 'Builder' that copies the strict 'S.ByteString'.
--
-- Use this function to create 'Builder's from smallish (@<= 4kb@)
-- 'S.ByteString's or if you need to guarantee that the 'S.ByteString' is not
-- shared with the chunks generated by the 'Builder'.
byteStringCopy :: S.ByteString -> Builder
byteStringCopy = builder <<< byteStringCopyStep

byteStringCopyStep :: forall a. S.ByteString -> BuildStep a -> BuildStep a
byteStringCopyStep (S.ByteString ifp ioff isize) k0 br0@(BufferRange op ope) =
  let op'  = op `plusPtr` isize
      ip   = ifp `plusPtr` ioff
      ipe  = ip `plusPtr` isize
      k br = k0 br
  in if op' <= ope
    then do
      _ <- liftEff $ memcpy op ip isize
      k0 (BufferRange op' ope)
    else wrappedBytesCopyStep (BufferRange ip ipe) k br0

-- | Construct a 'Builder' that always inserts the strict 'S.ByteString'
-- | directly as a chunk.
byteStringInsert :: S.ByteString -> Builder
byteStringInsert bs = builder (\k (BufferRange op _) -> pure $ insertChunk op bs k)

lazyByteStringThreshold :: Int -> L.ByteString -> Builder
lazyByteStringThreshold maxCopySize =
  L.foldrChunks' (\bs b -> byteStringThreshold maxCopySize bs <> b) mempty

lazyByteStringCopy :: L.ByteString -> Builder
lazyByteStringCopy =
  L.foldrChunks' (\bs b -> byteStringCopy bs <> b) mempty

lazyByteStringInsert :: L.ByteString -> Builder
lazyByteStringInsert =
  L.foldrChunks' (\bs b -> byteStringInsert bs <> b) mempty

byteString :: S.ByteString -> Builder
byteString = byteStringThreshold maximalCopySize

lazyByteString :: L.ByteString -> Builder
lazyByteString = lazyByteStringThreshold maximalCopySize

maximalCopySize :: Int
maximalCopySize = 2 * L.smallChunkSize

------------------------------------------------------------------------------
-- Builder execution
------------------------------------------------------------------------------

-- | A buffer allocation strategy for executing 'Builder's. The fields are
-- |
-- | > 'AllocationStrategy' firstBufSize bufSize trim
-- |
-- | states that the first buffer is of size @firstBufSize@, all following buffers
-- | are of size @bufSize@, and a buffer of size @n@ filled with @k@ bytes should
-- | be trimmed if @trim k n@ is 'True'.
data AllocationStrategy = AllocationStrategy
    (Maybe (Tuple Buffer Int) -> RefinedEff Buffer)
    Int
    (Int -> Int -> Boolean)

customStrategy
  :: (Maybe (Tuple Buffer Int) -> RefinedEff Buffer)
  -- ^ Buffer allocation function. If 'Nothing' is given, then a new first
  -- buffer should be allocated. If @'Just' (oldBuf, minSize)@ is given,
  -- then a buffer with minimal size 'minSize' must be returned. The
  -- strategy may reuse the 'oldBuffer', if it can guarantee that this
  -- referentially transparent and 'oldBuffer' is large enough.
  -> Int
  -- ^ Default Buffer Size
  -> (Int -> Int -> Boolean)
  -- ^ A predicate @trim used allocated@ returning 'True', if the buffer
  -- should be trimmed before it is returned.
  -> AllocationStrategy
customStrategy = AllocationStrategy

sanitizeSize :: Int -> Int
sanitizeSize = max 8

untrimmedStrategy
  :: Int -- ^ Size of the first buffer
  -> Int -- ^ Size of successive buffers
  -> AllocationStrategy
untrimmedStrategy firstSize bufSize = AllocationStrategy nextBuffer (sanitizeSize bufSize) (\_ _ -> false)
  where
    nextBuffer Nothing = liftEff $ newBuffer (sanitizeSize firstSize)
    nextBuffer (Just (Tuple _ minSize)) = liftEff $ newBuffer minSize

safeStrategy
  :: Int -- ^ Size of first buffer
  -> Int -- ^ Size of successive buffers
  -> AllocationStrategy
safeStrategy firstSize bufSize =
  AllocationStrategy nextBuffer (sanitizeSize bufSize) trim
  where
  trim used size                      = 2 * used < size
  nextBuffer Nothing                  = liftEff $ newBuffer (sanitizeSize firstSize)
  nextBuffer (Just (Tuple _ minSize)) = liftEff $ newBuffer minSize

toLazyByteStringWith
  :: AllocationStrategy
  -> L.ByteString
  -> Builder
  -> L.ByteString
toLazyByteStringWith strategy k b =
  chunkStreamToLBS strategy k $ unsafePerformRefined $ buildStepToCIOS strategy (runBuilder b)

buildStepToCIOS :: forall a. AllocationStrategy -> BuildStep a -> RefinedEff (ChunkStream a)
buildStepToCIOS (AllocationStrategy nextBuffer bufSize trim) stepper =
  nextBuffer Nothing >>= fill stepper
  where
    fill step buf@(Buffer fpbuf br@(BufferRange _ pe)) =
      fillWithBuildStep step doneH fullH insertChunkH br
      where
        pbuf = fpbuf

        doneH op' x = pure $
          Finished (Buffer fpbuf (BufferRange op' pe)) x

        fullH op' minSize nextStep =
          wrapChunk op' $ const $
            nextBuffer (Just (Tuple buf (max minSize bufSize))) >>= fill nextStep

        insertChunkH op' bs nextStep =
          wrapChunk op' $ \isEmpty -> yield bs $
            if isEmpty
              then fill nextStep buf
              else do
                buf' <- nextBuffer (Just (Tuple buf bufSize))
                fill nextStep buf'

        wrapChunk op' mkCIOS
          | op' `minusPtr` pbuf == 0 = mkCIOS true
          | trim (op' `minusPtr` pbuf) (pe  `minusPtr` pbuf) = do
              let chunkSize = op' `minusPtr` pbuf
              bs <- liftEff $ create chunkSize $ \pbuf' -> do
                      _ <- memcpy pbuf' pbuf chunkSize
                      pure unit
              pure $ Yield bs (mkCIOS false)
          | otherwise            =
              pure $ Yield (S.ByteString fpbuf 0 (op' `minusPtr` pbuf)) (mkCIOS false)
