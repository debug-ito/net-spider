-- |
-- Module: NetSpider.RPL.ContikiNG
-- Description: Parser for Contiki-NG logs about RPL
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.RPL.ContikiNG
  ( pLocalNode,
    pNeighbor
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Bits (shift)
import Data.Char (isDigit, isHexDigit)
import Data.Monoid ((<>))
import Data.Text (pack)
import Data.Word (Word16)
import Net.IPv6 (IPv6)
import qualified Net.IPv6 as IPv6
import qualified Text.ParserCombinators.ReadP as P
import Text.Read (readEither)

import qualified NetSpider.RPL.Local as Local

type Parser = P.ReadP

isAddressChar :: Char -> Bool
isAddressChar c = isHexDigit c || c == ':'

pAddress :: Parser IPv6
pAddress = fromS =<< P.munch1 isAddressChar
  where
    fromS str =
      case IPv6.decode $ pack str of
        Nothing -> fail ("Invalid IPv6 address: " <> str)
        Just addr -> return addr

data CompactID = CNodeID !Int
               | CNodeAddress !Word16
               deriving (Show,Eq,Ord)

makeCompactAddress :: CompactID -> IPv6
makeCompactAddress cid =
  case cid of
    CNodeID nid -> IPv6.fromWord32s 0 0 0 (fromIntegral nid)
    CNodeAddress addr -> IPv6.fromWord16s 0 0 0 0 0 0 0 addr

pHexWord16 :: String -> Parser Word16
pHexWord16 input = go 0 input
  where
    go acc [] = return acc
    go acc (c:rest) = do
      c_num <- parseC
      go ((acc `shift` 8) + c_num) rest
      where
        diffWord a b = fromIntegral (fromEnum a - fromEnum b)
        parseC = if c >= '0' && c <= '9'
                 then return $ diffWord c '0'
                 else if c >= 'a' && c <= 'f'
                      then return $ diffWord c 'a'
                      else if c >= 'A' && c <= 'F'
                           then return $ diffWord c 'A'
                           else fail ("Invalid hex number: " <> input)

pCompactID :: Parser CompactID
pCompactID = (fmap CNodeID $ pRead =<< P.count 3 (P.satisfy isDigit))
             <|> (fmap CNodeAddress $ pHexWord16 =<< P.count 4 (P.satisfy isHexDigit))

pCompactAddress :: Parser IPv6
pCompactAddress = do
  void $ P.string "6G-"  -- expecting unicast global address
  fmap makeCompactAddress $ pCompactID

-- neighbor address can be logged in a "compact" form.
-- https://github.com/contiki-ng/contiki-ng/blob/develop/os/sys/log.c

pRead :: Read a => String -> Parser a
pRead = either fail return . readEither

pLocalNode :: Parser (IPv6, Local.LocalNode)
pLocalNode = do
  void $ P.string "nbr: own state, addr "
  addr <- pAddress
  void $ P.string ", DAG state: "
  void $ P.munch (\c -> c /= ',')
  void $ P.string ", MOP "
  void $ P.munch isDigit
  void $ P.string " OCP "
  void $ P.munch isDigit
  void $ P.string " rank "
  rank <- pRead =<< P.munch1 isDigit
  void $ P.string " max-rank "
  void $ P.munch isDigit
  void $ P.string ", dioint "
  void $ P.munch isDigit -- TODO: store DIO interval
  return (addr, Local.LocalNode { Local.rank = rank })

pExpectChar :: Char -> Parser Bool
pExpectChar exp_c = fmap (== exp_c) $ P.get

pNeighbor :: Parser (IPv6, Local.LocalLink)
pNeighbor = do
  void $ P.string "nbr: "
  neighbor_addr <- pCompactAddress <|> pAddress
  P.skipSpaces
  neighbor_rank <- pRead =<< P.munch1 isDigit
  void $ P.string ", "
  P.skipSpaces
  metric <- pRead =<< P.munch1 isDigit
  void $ P.string " => "
  P.skipSpaces
  void $ P.munch isDigit -- rank_via_neighbor
  void $ P.string " -- "
  P.skipSpaces
  void $ P.munch isDigit -- freshness
  void $ P.string " "
  void $ pExpectChar 'r'
  void $ pExpectChar 'b'
  acceptable <- pExpectChar 'a'
  void $ pExpectChar 'f'
  preferred <- pExpectChar 'p'
  return ( neighbor_addr,
           Local.LocalLink
           { Local.neighborType = if preferred
                                  then Local.PreferredParent
                                  else if acceptable
                                       then Local.ParentCandidate
                                       else Local.OtherNeighbor,
             Local.neighborRank = neighbor_rank,
             Local.metric = metric,
             Local.rssi = Nothing -- TODO: How should we get this?
           }
         )
