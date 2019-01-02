-- |
-- Module: NetSpider.RPL.ContikiNG
-- Description: Parser for Contiki-NG logs about RPL
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.RPL.ContikiNG
  ( pLocalNode
  ) where

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

pHeading :: Parser ()
pHeading = void $ P.string "nbr: own state, addr "

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

pRead :: Read a => String -> Parser a
pRead = either fail return . readEither

pLocalNode :: Parser (IPv6, Local.LocalNode)
pLocalNode = do
  pHeading
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

