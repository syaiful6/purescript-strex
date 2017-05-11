module Data.ByteString.Lazy.Internal
  ( ByteString(..)
  , Step(..)
  , step
  , invariant
  , append
  , concat
  , empty
  , chunk
  , foldrChunks
  , foldlChunks
  , foldrChunks'
  , defaultChunkSize
  , smallChunkSize
  ) where

import Prelude

import Control.Lazy as Z

import Data.Lazy (Lazy, defer, force)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.List (List(Nil), (:))

import Data.ByteString.Internal (ByteString(..)) as S
import Data.ByteString (length, take, drop) as S


-- | A lazy ByteString, encoded using lazy linked list
newtype ByteString = ByteString (Lazy Step)

-- | The step of ByteString, it's either Empty or consists of non-null `S.ByteString`.
-- | To preserve this, use smart constructor defined here to construct chunk
data Step = Empty | Chunk S.ByteString ByteString

-- | Unwrap a ByteString
step :: ByteString -> Step
step = unwrap >>> force

-- | Every ByteString is either 'Empty' or consists of non-null 'S.ByteString's.
invariant :: ByteString -> Boolean
invariant bs = go (step bs) true
  where
    go Empty acc                              = if acc then true else false
    go (Chunk (S.ByteString _ _ len) cs)  acc =
      if len > 0 && acc then go (step cs) true else false

-- | Smart constructor for Empty
empty :: ByteString
empty = ByteString $ defer \_ -> Empty

-- | Smart constructor for 'Chunk'.
chunk :: S.ByteString -> ByteString -> ByteString
chunk c@(S.ByteString _ _ len) cs
  | len == 0   = cs
  | otherwise  = ByteString $ defer \_ -> Chunk c cs

foldrChunks' :: forall a. Z.Lazy a => (S.ByteString -> a -> a) -> a -> ByteString -> a
foldrChunks' op z = go
  where
  go xs = case step xs of
    Empty     -> z
    Chunk h t -> Z.defer \_ -> h `op` go t

foldrChunks :: forall a. (S.ByteString -> a -> a) -> a -> ByteString -> a
foldrChunks op z xs = foldlChunks (flip op) z (rev xs)
  where
    rev = foldlChunks (flip chunk) empty

foldlChunks :: forall a. (a -> S.ByteString -> a) -> a -> ByteString -> a
foldlChunks op = go
  where
    go b xs = case step xs of
      Empty       -> b
      Chunk hd tl -> go (b `op` hd) tl

defaultChunkSize :: Int
defaultChunkSize = 32 * 1024 - 16

smallChunkSize :: Int
smallChunkSize = 4 * 1024 - 16

eqBS :: ByteString -> ByteString -> Boolean
eqBS xs ys = go (step xs) (step ys)
  where
    go Empty Empty = true
    go Empty _     = false
    go _     Empty = false
    go (Chunk a as) (Chunk b bs) =
      case compare (S.length a) (S.length b) of
        LT ->
          if a == S.take (S.length a) b
            then go (step as) (Chunk (S.drop (S.length a) b) bs)
            else false
        EQ ->
          a == b
        GT ->
          if S.take (S.length b) a == b
            then go (Chunk (S.drop (S.length b) a) as) (step bs)
            else false

compareBS :: ByteString -> ByteString -> Ordering
compareBS xs ys = go (step xs) (step ys)
  where
    go Empty Empty = EQ
    go Empty _     = LT
    go _     Empty = GT
    go (Chunk a as) (Chunk b bs) =
        case compare (S.length a) (S.length b) of
          LT -> case compare a (S.take (S.length a) b) of
                  EQ     -> go (step as) (Chunk (S.drop (S.length a) b) bs)
                  result -> result
          EQ -> case compare a b of
                  EQ     -> go (step as) (step bs)
                  result -> result
          GT -> case compare (S.take (S.length b) a) b of
                  EQ     -> go (Chunk (S.drop (S.length b) a) as) (step bs)
                  result -> result

append :: ByteString -> ByteString -> ByteString
append xs ys = foldrChunks' chunk ys xs

concat :: List ByteString -> ByteString
concat css0 = to css0
  where
  go x css = case step x of
    Empty        -> to css
    (Chunk c cs) -> ByteString $ defer \_ -> Chunk c (go cs css)
  to Nil    = empty
  to (x:xs) = go x xs

derive instance newtypeByteString :: Newtype ByteString _

instance eqByteString :: Eq ByteString where
  eq = eqBS

instance ordByteString :: Ord ByteString where
  compare = compareBS

instance semigroupByString :: Semigroup ByteString where
  append xs ys = ByteString (go <$> unwrap xs)
    where
    go Empty         = step ys
    go (Chunk x xs') = Chunk x (xs' <> ys)

instance monoidByteString :: Monoid ByteString where
  mempty = empty

instance showByteString :: Show ByteString where
  show bs0 = "(Lazy ByteString " <> (foldlChunks (\acc bs -> acc <> show bs) "" bs0) <> " )"

instance lazyByteString :: Z.Lazy ByteString where
  defer f = ByteString $ defer (step <<< f)
