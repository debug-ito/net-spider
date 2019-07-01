{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module: NetSpider.Pangraph.Atom
-- Description: Atom type for Pangraph attributes
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Pangraph.Atom
  ( -- * Types
    Atom,
    ToAtom(..),
    -- * Utility
    showAtom
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import Data.Word (Word, Word8, Word16, Word32)
import qualified NetSpider.GraphML.Writer as GraphML

-- | 'Atom' is the type for Node ID and attributes in Pangraph data
-- model.
type Atom = ByteString

-- | Conversion to 'Atom'.
--
-- String-like types are just converted without quoting. Number types
-- are converted using 'show'.
class ToAtom a where
  toAtom :: a -> Atom

instance ToAtom ByteString where
  toAtom = id

instance ToAtom BSL.ByteString where
  toAtom = BSL.toStrict

instance ToAtom Text where
  toAtom = encodeUtf8

instance ToAtom TL.Text where
  toAtom = toAtom . TL.toStrict

instance ToAtom String where
  toAtom = toAtom . TL.pack

showAtom :: Show a => a -> Atom
showAtom = toAtom . show

instance ToAtom Int where
  toAtom = showAtom

instance ToAtom Int8 where
  toAtom = showAtom

instance ToAtom Int16 where
  toAtom = showAtom

instance ToAtom Int32 where
  toAtom = showAtom

instance ToAtom Int64 where
  toAtom = showAtom

instance ToAtom Word where
  toAtom = showAtom

instance ToAtom Word8 where
  toAtom = showAtom

instance ToAtom Word16 where
  toAtom = showAtom

instance ToAtom Word32 where
  toAtom = showAtom

instance ToAtom Integer where
  toAtom = showAtom

instance ToAtom Float where
  toAtom = showAtom

instance ToAtom Double where
  toAtom = showAtom

instance ToAtom Bool where
  toAtom = showAtom

instance ToAtom GraphML.AttributeValue where
  toAtom v =
    case v of
      GraphML.AttrBoolean b -> showAtom b
      GraphML.AttrInt i -> showAtom i
      GraphML.AttrLong l -> showAtom l
      GraphML.AttrFloat f -> showAtom f
      GraphML.AttrDouble d -> showAtom d
      GraphML.AttrString t -> showAtom t
