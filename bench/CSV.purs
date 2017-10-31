module Benc.CSV where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Functor.Contravariant ((>$<))

import Data.ArrayBuffer.Types (Uint8Array, ArrayBuffer)
import Data.ByteString (ByteString(..))
import Data.ByteString.Builder as B
import Data.ByteString.Builder.Prim as P
import Data.ByteString.Internal (Ptr(..))
import Data.Function.Uncurried as Fn
import Data.Divide (divided)
import Data.String as S
import Data.Tuple (Tuple(..))
import Data.List.Lazy as Z
import Data.List (List(Nil), (:), (..), fromFoldable)
import Data.Foldable (fold, foldMap)
import Data.Monoid (mempty)

import Unsafe.Coerce (unsafeCoerce)

import Performance.Minibench (bench)

data Cell
  = CellStr String
  | CellInt Int

type Row = List Cell
type Table = List Row

exdata :: List String
exdata = fromFoldable ["hello", "\"1\"", "λ-wörld"]

table :: Z.List (List Cell)
table = Z.fromFoldable [map CellStr exdata, map CellInt ((-3)..3)]

-- | A bigger table for benchmarking our encoding functions.
maxiTable :: Table
maxiTable = Z.toUnfoldable (Z.take 1000 $ Z.cycle table)

------------------------------------------------------------------------------
-- String based rendering
------------------------------------------------------------------------------
renderString :: String -> String
renderString cs = "\"" <> S.fromCharArray escaped <> "\""
  where
  escaped      = bind (S.toCharArray cs) escape
  escape '\\'  = ['\\', '\\']
  escape '\"'  = ['\\', '\"']
  escape s     = pure s

renderCell :: Cell -> String
renderCell (CellStr cs)    = renderString cs
renderCell (CellInt i)     = show i

renderRow :: Row -> String
renderRow = go
  where
  go Nil    = ""
  go (x:xs) = renderCell x <> foldMap (\s -> "," <> renderCell s) xs

renderTable :: Table -> String
renderTable rs = fold (map render rs)
  where
  render r = renderRow r <> "\n"

benchString :: forall eff. Eff (console :: CONSOLE | eff) Unit
benchString = bench \_ -> renderTable maxiTable

benchStringUtf8 :: forall eff. Eff (console :: CONSOLE | eff) Unit
benchStringUtf8 = bench \_ -> (B.toLazyByteString <<< B.stringUtf8 <<< renderTable) maxiTable

renderStringB :: String -> B.Builder
renderStringB cs = B.charUtf8 '"' <> foldMap escape (S.toCharArray cs) <> B.charUtf8 '"'
  where
    escape '\\' = B.charUtf8 '\\' <> B.charUtf8 '\\'
    escape '\"' = B.charUtf8 '\\' <> B.charUtf8 '"'
    escape c    = B.charUtf8 c

renderCellB :: Cell -> B.Builder
renderCellB (CellStr cs) = renderStringB cs
renderCellB (CellInt i)  = B.string7 $ show i

renderRowB :: Row -> B.Builder
renderRowB = go
  where
  go Nil      = mempty
  go (x : xs) = renderCellB x <> foldMap (\s -> B.charUtf8 ',' <> renderCellB s) xs

renderTableB :: Table -> B.Builder
renderTableB rs = fold (map render rs)
  where
  render r = renderRowB r <> B.charUtf8 '\n'

benchBuilderUtf8 :: forall eff. Eff (console :: CONSOLE | eff) Unit
benchBuilderUtf8 = bench \_ -> (B.toLazyByteString <<< renderTableB) maxiTable

--------------------------------------------------------------------------------
-- using Primitive -------------------------------------------------------------
--------------------------------------------------------------------------------
renderStringBP :: String -> B.Builder
renderStringBP cs = B.charUtf8 '"' <> P.primMapStringBounded escape cs <> B.charUtf8 '"'
  where
  escape = P.condB ('\\' == _) (const (Tuple '\\' '\\') >$< P.charUtf8 `divided` P.charUtf8) $
           P.condB ('\"' == _) (const (Tuple '\\' '\"') >$< P.charUtf8 `divided` P.charUtf8) $
           P.charUtf8

renderCellBP :: Cell -> B.Builder
renderCellBP (CellStr cs) = renderStringBP cs
renderCellBP (CellInt i)  = B.string7 $ show i

renderRowBP :: Row -> B.Builder
renderRowBP = go
  where
  go Nil      = mempty
  go (x : xs) = renderCellBP x <> foldMap (\s -> B.charUtf8 ',' <> renderCellBP s) xs

renderTableBP :: Table -> B.Builder
renderTableBP rs = fold (map render rs)
  where
  render r = renderRowBP r <> B.charUtf8 '\n'

benchBuilderPrimUtf8 :: forall eff. Eff (console :: CONSOLE | eff) Unit
benchBuilderPrimUtf8 = bench \_ -> (B.toLazyByteString <<< renderTableBP) maxiTable

sliceBuf :: forall r. ByteString -> Eff r Uint8Array
sliceBuf (ByteString (Ptr a av) n l) = Fn.runFn3 _sliceArrayBuffer (a + n) l (arrayBuffer av)

foreign import _sliceArrayBuffer :: forall r. Fn.Fn3 Int Int ArrayBuffer (Eff r Uint8Array)

foreign import writeFile :: forall e. Fn.Fn2 String Uint8Array (Eff e Unit)

arrayBuffer :: Uint8Array -> ArrayBuffer
arrayBuffer = _.buffer <<< typedArraytoRecord

typedArraytoRecord :: Uint8Array -> { buffer :: ArrayBuffer, length :: Int }
typedArraytoRecord = unsafeCoerce
