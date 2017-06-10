module Data.ByteString.Lazy
  ( empty
  , singleton
  , fromChunks
  , toChunks
  , fromStrict
  , null
  , length
  , cons
  , snoc
  , head
  , tail
  , uncons
  , foldl
  , foldr
  , foldr'
  , unfoldr
  , iterate
  , repeat
  , module Data.ByteString.Lazy.Internal
  ) where

import Prelude

import Control.Lazy as Z

import Data.ByteString as B
import Data.ByteString.Internal (Octet) as I
import Data.ByteString.Lazy.Internal
  (ByteString(..), Step(..), step, foldrChunks, foldrChunks', foldlChunks)
import Data.ByteString.Lazy.Internal as BL
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Lazy (defer)
import Data.List.Lazy (List, Step(..), step, cons, nil) as L
import Data.Tuple (Tuple(..))

-- | The empty 'ByteString'
empty :: ByteString
empty = BL.empty

-- | A byte string with a single byte.
-- |
-- | Running time: `O(1)`
singleton :: I.Octet -> ByteString
singleton s = BL.chunk (B.singleton s) empty

-- | /O(c)/ Convert a list of strict 'ByteString' into a lazy 'ByteString'
fromChunks :: L.List B.ByteString -> ByteString
fromChunks cs = foldrLazy BL.chunk empty cs

-- | /O(c)/ Convert a lazy 'ByteString' into a list of strict 'ByteString'
toChunks :: ByteString -> L.List B.ByteString
toChunks xs = BL.foldrChunks L.cons L.nil xs

-- | /O(1)/ Convert a strict 'ByteString' into a lazy 'ByteString'.
fromStrict :: B.ByteString -> ByteString
fromStrict bs
  | B.null bs = BL.empty
  | otherwise = BL.chunk bs BL.empty

--------------------------------------------------------------------------------
-- ByteString size -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Test whether a bytestring is empty.
null :: ByteString -> Boolean
null = isNothing <<< uncons

-- | Get the length of a ByteString
length :: ByteString -> Int
length = BL.foldlChunks (\n + c -> n + (B.length c)) 0

--------------------------------------------------------------------------------
-- Extending byteString --------------------------------------------------------
--------------------------------------------------------------------------------

cons :: I.Octet -> ByteString -> ByteString
cons c cs = BL.chunk (B.singleton c) cs

snoc :: ByteString -> I.Octet -> ByteString
snoc xs x = BL.foldrChunks' BL.chunk (BL.chunk (B.singleton x) empty) xs

--------------------------------------------------------------------------------
-- Non-indexed reads -----------------------------------------------------------
--------------------------------------------------------------------------------

head :: ByteString -> Maybe I.Octet
head bs = _.head <$> uncons bs

tail :: ByteString -> Maybe ByteString
tail bs = _.tail <$> uncons bs

uncons :: ByteString -> Maybe { head :: I.Octet, tail :: ByteString }
uncons bs = case step bs of
  Empty      -> Nothing
  Chunk c cs -> B.uncons c >>= \b -> Just { head: b.head, tail: BL.chunk b.tail cs }

last :: ByteString -> Maybe I.Octet
last bs = case step bs of
  Empty        -> Nothing
  Chunk c0 cs0 -> go c0 cs0
  where
  go c xs = case step xs of
    Empty        -> B.last c
    Chunk c' cs' -> go c' cs'

init :: ByteString -> Maybe ByteString
init bs = case step bs of
  Empty        -> Nothing
  Chunk c0 cs0 -> go c0 cs0
  where
  go c xs = case step xs of
    Empty
      | B.length c == 1 -> Just empty
      | otherwise       -> flip BL.chunk empty <$> B.init c
    Chunk c' cs'        -> Just $ BL.ByteString $ defer \_ -> BL.Chunk c (maybe empty id $ go c' cs')

foldl :: forall a. (a -> I.Octet -> a) -> a -> ByteString -> a
foldl f z = go z
  where
  go a bs = case step bs of
    Empty      -> a
    Chunk c cs -> go (B.foldl f a c) cs

foldr :: forall a. (I.Octet -> a -> a) -> a -> ByteString -> a
foldr k z = BL.foldrChunks (flip (B.foldr k)) z

foldr' :: forall a. Z.Lazy a => (I.Octet -> a -> a) -> a -> ByteString -> a
foldr' k z = BL.foldrChunks' (flip (B.foldr k)) z

unfoldr :: forall a. (a -> Maybe (Tuple I.Octet a)) -> a -> ByteString
unfoldr f z = unfoldChunk 32 z
  where
  unfoldChunk n x = case B.unfoldrN n f x of
    Tuple c Nothing
      | B.null c  -> empty
      | otherwise -> BL.chunk c empty
    Tuple c (Just x') -> BL.ByteString $ defer \_ -> BL.Chunk c (unfoldChunk (n*2) x')

-- | 'iterate' f x returns an infinite ByteString of repeated applications
-- | of f to x:
iterate :: (I.Octet -> I.Octet) -> I.Octet -> ByteString
iterate f = unfoldr (\x -> case f x of x' -> Just (Tuple x x'))

-- | 'repeat' x is an infinite ByteString, with @x@ the value of every
-- | element.
repeat :: I.Octet -> ByteString
repeat w = Z.fix \cs -> BL.chunk (B.replicate BL.smallChunkSize w) cs

--------------------------------------------------------------------------------
-- Internal helper -------------------------------------------------------------
--------------------------------------------------------------------------------

foldrLazy :: forall a b. Z.Lazy b => (a -> b -> b) -> b -> L.List a -> b
foldrLazy op z = go
  where
  go xs = case L.step xs of
    L.Nil -> z
    L.Cons x xs' -> Z.defer \_ -> x `op` go xs'
