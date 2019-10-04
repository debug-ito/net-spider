-- |
-- Module: NetSpider.RPL.IPv6
-- Description: (INTERNAL) Basic routines for IPv6 type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This module is for internal use.__
--
-- @since 0.3.1.0
module NetSpider.RPL.IPv6
  ( Prefix,
    InterfaceID,
    isLinkLocal,
    getPrefix,
    setPrefix,
    getInterfaceID
  ) where

import Data.Bits ((.&.), shift, shiftL, (.|.), shiftR)
import Data.Word (Word64, Word32)
import Net.IPv6 (IPv6, toWord16s, toWord32s, fromWord32s)

type Prefix = Word64

type InterfaceID = Word64

isLinkLocal :: IPv6 -> Bool
isLinkLocal addr = (== 0xfe80) $ (.&. bit_mask) $ top_word
  where
    (top_word, _, _, _, _, _, _, _) = toWord16s addr
    bit_mask = (2 ^ (10 :: Int) - 1) `shift` 6

getPrefix :: IPv6 -> Prefix
getPrefix addr = to64 u l
  where
    (u, l, _, _) = toWord32s addr

setPrefix :: Prefix -> IPv6 -> IPv6
setPrefix p orig = fromWord32s u1 u2 l1 l2
  where
    (_, _, l1, l2) = toWord32s orig
    u1 = fromIntegral ((p `shiftR` 32) .&. 0xFFFFFFFF)
    u2 = fromIntegral (p .&. 0xFFFFFFFF)

getInterfaceID :: IPv6 -> InterfaceID
getInterfaceID addr = to64 u l
  where
    (_, _, u, l) = toWord32s addr

to64 :: Word32 -> Word32 -> Word64
to64 u l = (fromIntegral u `shiftL` 32) .|. fromIntegral l

