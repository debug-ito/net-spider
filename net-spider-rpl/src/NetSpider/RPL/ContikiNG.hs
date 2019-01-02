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
import Data.Char (isDigit)
import Data.Monoid ((<>))
import Data.Text (pack)
import Net.IPv6 (IPv6)
import qualified Net.IPv6 as IPv6
import qualified Text.ParserCombinators.ReadP as P
import Text.Read (readEither)

import qualified NetSpider.RPL.Local as Local

type Parser = P.ReadP

isAddressChar :: Char -> Bool
isAddressChar c = c `elem` charset
  where
    charset = ":" ++ ['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']

pAddress :: Parser IPv6
pAddress = fromS =<< P.munch1 isAddressChar
  where
    fromS str =
      case IPv6.decode $ pack str of
        Nothing -> fail ("Invalid IPv6 address: " <> str)
        Just addr -> return addr

pCompactAddress :: Parser IPv6
pCompactAddress = undefined -- TODO

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
