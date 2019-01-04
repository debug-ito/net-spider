-- |
-- Module: NetSpider.RPL.ContikiNG
-- Description: Parser for Contiki-NG logs about RPL
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.RPL.ContikiNG
  ( parseFile,
    pLocalNode,
    pLocalNeighbor
  ) where

import Control.Applicative ((<|>), (<$>), (<*>), (*>), (<*), many)
import Control.Monad (void)
import Data.Bits (shift)
import Data.Char (isDigit, isHexDigit)
import Data.List (sortOn, reverse)
import Data.Monoid ((<>))
import Data.Text (pack)
import Data.Word (Word16)
import Net.IPv6 (IPv6)
import qualified Net.IPv6 as IPv6
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(LinkToTarget))
import NetSpider.Timestamp (Timestamp)
import System.IO (withFile, IOMode(ReadMode), hGetLine, hIsEOF)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read (readEither)

import NetSpider.RPL.FindingID (FindingID(FindingID), FindingType(FindingLocal))
import qualified NetSpider.RPL.Local as Local

type Parser = P.ReadP

runParser :: Parser a -> String -> Maybe a
runParser p input = extract $ sortPairs $ P.readP_to_S p input
  where
    sortPairs = sortOn $ \(_, rest) -> length rest
    extract [] = Nothing
    extract ((a,_) : _) = Just a

parseFile :: Parser Timestamp -> FilePath -> IO [FoundNode FindingID Local.LocalNode Local.LocalLink]
parseFile pTimestamp input_file = withFile input_file ReadMode $ onHandle
  where
    onHandle h = go []
      where
        go acc = do
          mfn <- parseFoundNode pTimestamp $ tryGetLine h
          case mfn of
            Nothing -> return $ reverse acc
            Just fn -> go (fn : acc)
    tryGetLine h = do
      eof <- hIsEOF h
      if eof
        then return Nothing
        else Just <$> hGetLine h

parseFoundNode :: Parser Timestamp -> IO (Maybe String) -> IO (Maybe (FoundNode FindingID Local.LocalNode Local.LocalLink))
parseFoundNode pTimestamp getL = impl
  where
    impl = do
      mline <- getL
      case mline of
        Nothing -> return Nothing
        Just line -> 
          case runParser ((,) <$> pTimestamp <*> pLocalNode) line of
            Nothing -> impl
            Just (ts, (addr, ln)) -> do
              mns <- parseNeighbors pTimestamp getL
              case mns of
                Nothing -> return Nothing
                Just ns -> 
                  return $ Just $ FoundNode { subjectNode = FindingID FindingLocal addr,
                                              foundAt = ts,
                                              neighborLinks = map toFoundLink ns,
                                              nodeAttributes = ln
                                            }
    toFoundLink (neighbor_addr, ll) =
      FoundLink { targetNode = FindingID FindingLocal neighbor_addr,
                  linkState = LinkToTarget,
                  linkAttributes = ll
                }
--- TODO: this functions should be deprecated by pNeighbors
parseNeighbors :: Parser Timestamp -> IO (Maybe String) -> IO (Maybe [(IPv6, Local.LocalLink)])
parseNeighbors pTimestamp getL = go []
  where
    go acc = do
      mline <- getL
      case mline of
        Nothing -> return Nothing
        Just line -> 
          case runParser (pTimestamp *> pLocalNeighbor) line of
            Nothing -> return $ Just $ reverse acc
            Just n -> go (n : acc)

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

pLocalNeighbors :: Parser Timestamp -> Parser [(IPv6, Local.LocalLink)]
pLocalNeighbors pTimestamp = P.endBy neighbor_line end_line
  where
    neighbor_line = pTimestamp *> pLogHead *> pLocalNeighbor <* skipUntilNewline
    end_line = pTimestamp *> P.string "nbr: end of list" <* skipUntilNewline
    skipUntilNewline = void $ P.munch (not . isNewline) *> P.munch isNewline
    isNewline c = c == '\n' || c == '\r'

pLocalNeighbor :: Parser (IPv6, Local.LocalLink)
pLocalNeighbor = do
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

pLogHead :: Parser ()
pLogHead = do
  void $ P.char '['
  void $ P.munch (not . (== ']'))
  void $ P.string "] "

-- TODO: SR entry logs:
-- rpl_dag_root_print_links, uip_sr_link_snprint
