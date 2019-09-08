-- |
-- Module: NetSpider.RPL.ContikiNG
-- Description: Parser for Contiki-NG logs about RPL
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module exports utility functions to read and parse log files
-- from Contiki-NG applications.
--
-- [Contiki-NG](http://contiki-ng.org/) is a tiny operation system for
-- wireless network devices. It supports RPL.
module NetSpider.RPL.ContikiNG
  ( -- * Parser functions
    parseFile,
    parseFileHandle,
    -- * Parser components
    Parser,
    pCoojaLogHead,
    pCoojaLogHead',
    pSyslogHead
  ) where

import Control.Applicative ((<|>), (<$>), (<*>), (*>), (<*), many, optional)
import Control.Exception (Exception, throwIO)
import Control.Monad (void)
import Data.Bits (shift)
import Data.Char (isDigit, isHexDigit, isSpace)
import Data.Int (Int64)
import Data.List (sortOn, reverse)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Text (pack)
import qualified Data.Time as Time
import Data.Word (Word16)
import GHC.Exts (groupWith)
import Net.IPv6 (IPv6)
import qualified Net.IPv6 as IPv6
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(LinkToTarget))
import NetSpider.Timestamp (Timestamp, fromEpochMillisecond, fromLocalTime, fromZonedTime)
import System.IO (withFile, IOMode(ReadMode), hGetLine, hIsEOF, Handle)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read (readEither)

import NetSpider.RPL.FindingID (FindingID(FindingID), FindingType(..))
import NetSpider.RPL.IPv6 (isLinkLocal, setPrefix, getPrefix)
import qualified NetSpider.RPL.DIO as DIO
import NetSpider.RPL.DIO (FoundNodeDIO, dioLinkState, Rank)
import qualified NetSpider.RPL.DAO as DAO
import NetSpider.RPL.DAO (FoundNodeDAO)

type Parser = P.ReadP

data ParseError = ParseError String
                deriving (Show,Eq,Ord)

instance Exception ParseError

runParser :: Parser a -> String -> Maybe a
runParser p input = extract $ sortPairs $ P.readP_to_S p input
  where
    sortPairs = sortOn $ \(_, rest) -> length rest
    extract [] = Nothing
    extract ((a,_) : _) = Just a

runParser' :: Parser a
           -> String -- ^ error message
           -> String -- ^ input
           -> Parser a
runParser' p err input =
  case runParser p input of
    Nothing -> fail err
    Just a -> return a

-- | Read and parse a log file from a Contiki-NG application to make
-- 'FoundNodeDIO' and 'FoundNodeDAO'.
--
-- Currently this parser function supports logs from \"rpl-lite\"
-- module only.
--
-- It assumes that each line of log file has prefix, and that the
-- prefix contains timestamp information. You have to pass the parser
-- for the prefix to this function. For example, if you read a log
-- file generated by Cooja simulator, use 'pCoojaLogHead'' parser.
--
-- One 'FoundNodeDIO' object is parsed from one block of log lines
-- from the rpl module. On the other hand, one or more 'FoundNodeDAO'
-- objects are parsed from one block of log lines from the rpl
-- module. The 'FoundNodeDAO' objects generated from the same log
-- block share the same timestamp.
parseFile :: Parser Timestamp -- ^ Parser for log prefix
          -> FilePath -- ^ File to read
          -> IO ([FoundNodeDIO], [FoundNodeDAO])
parseFile pt file = withFile file ReadMode $ parseFileHandle pt

-- | Same as 'parseFile' but for a 'Handle'.
parseFileHandle :: Parser Timestamp -- ^ Parser for log prefix
                -> Handle -- ^ File handle to read
                -> IO ([FoundNodeDIO], [FoundNodeDAO])
parseFileHandle pTimestamp handle = go ([], [])
  where
    go (acc_dio, acc_dao) = do
      mentry <- parseOneEntry pTimestamp $ tryGetLine handle
      case mentry of
        Nothing -> return (reverse acc_dio, reverse acc_dao)
        Just (PEDIO fl) -> go (fl : acc_dio, acc_dao)
        Just (PEDAO srs) -> go (acc_dio, srs ++ acc_dao)
        Just PEMisc -> go (acc_dio, acc_dao)
    tryGetLine h = do
      eof <- hIsEOF h
      if eof
        then return Nothing
        else Just <$> hGetLine h

data ParseEntry = PEDIO FoundNodeDIO
                | PEDAO [FoundNodeDAO]
                | PEMisc
                deriving (Show,Eq)

-- | Return 'Nothing' when it finishes reading.
parseOneEntry :: Parser Timestamp -> IO (Maybe String) -> IO (Maybe ParseEntry)
parseOneEntry pTimestamp getL = impl
  where
    impl = do
      mline <- getL
      case mline of
        Nothing -> return Nothing
        Just line ->
          case runParser ((,) <$> pTimestamp <*> (pLogHead *> pHeading)) line of
            Nothing -> return $ Just PEMisc
            Just (ts, HDIO addr node) -> proceedDIO ts addr node
            Just (ts, HDAO route_num) -> proceedDAO ts route_num
    withPrefix p = pTimestamp *> pLogHead *> p
    proceedDIO ts addr node = do
      mlinks <- readUntil getL (withPrefix pDIONeighbor) (withPrefix pDIONeighborEnd)
      case mlinks of
        Nothing -> return Nothing
        Just links -> return $ Just $ PEDIO $ makeFoundNodeDIO ts addr node $ map (setAddressPrefix addr) links
    proceedDAO ts route_num = do
      mlinks <- readUntil getL (withPrefix pDAOLink) (withPrefix pDAOLinkEnd)
      case mlinks of
        Nothing -> return Nothing
        Just links -> do
          root_address <- maybe (throwIO $ ParseError "No root address found in SR log") return
                          $ getRootAddress links
          return $ Just $ PEDAO $ map (makeDAONodeFromTuple root_address route_num ts) $ groupDAOLinks links
    getRootAddress :: [(IPv6, Maybe (IPv6, Word))] -> Maybe IPv6
    getRootAddress links = fmap fst $ listToMaybe $ filter isRootEntry links
      where
        isRootEntry (_, Nothing) = True
        isRootEntry (_, _) = False
    groupDAOLinks :: [(IPv6, Maybe (IPv6, Word))] -> [(IPv6, [(IPv6, Word)])]
    groupDAOLinks links = map toTuple $ groupWith byParentAddr $ (filterOutRoot =<< links)
      where
        filterOutRoot (_, Nothing) = []
        filterOutRoot (c, Just (p, lt)) = [(c, p, lt)]
        byParentAddr (_, p, _) = p
        toTuple [] = error "groupDAOLinks: this should not happen"
        toTuple entries@((_, p, _) : _) = (p, map extractChildAndLifetime entries)
        extractChildAndLifetime (c, _, lt) = (c, lt)
    makeDAONodeFromTuple root_addr route_num ts (parent_addr, children) =
      makeFoundNodeDAO
      ts (if parent_addr == root_addr then Just route_num else Nothing)
      parent_addr children
    setAddressPrefix self_addr (neighbor_addr, ll) = (modified_addr, ll)
      where
        modified_addr = if isLinkLocal neighbor_addr
                        then setPrefix (getPrefix self_addr) neighbor_addr
                        else neighbor_addr

readUntil :: IO (Maybe String) -> Parser a -> Parser end -> IO (Maybe [a])
readUntil getL pBody pEnd = go []
  where
    go acc = do
      mline <- getL
      case mline of
        Nothing -> return Nothing
        Just line ->
          case runParser ((Left <$> pEnd) <|> (Right <$> pBody)) line of
            Nothing -> throwIO $ ParseError ("Parse error at line: " <> line)
            Just (Left _) -> return $ Just $ reverse acc
            Just (Right body) -> go (body : acc)

makeFoundNodeDIO :: Timestamp -> IPv6 -> DIO.DIONode -> [(IPv6, DIO.DIOLink)] -> FoundNodeDIO
makeFoundNodeDIO ts self_addr node_attr neighbors =
  FoundNode { subjectNode = FindingID FindingDIO self_addr,
              foundAt = ts,
              neighborLinks = map toFoundLink neighbors,
              nodeAttributes = node_attr
            }
  where
    toFoundLink (neighbor_addr, ll) =
      FoundLink { targetNode = FindingID FindingDIO neighbor_addr,
                  linkState = dioLinkState ll,
                  linkAttributes = ll
                }

makeFoundNodeDAO :: Timestamp -> Maybe Word -> IPv6 -> [(IPv6, Word)] -> FoundNodeDAO
makeFoundNodeDAO ts mroute_num parent_addr children =
  FoundNode { subjectNode = FindingID FindingDAO parent_addr,
              foundAt = ts,
              neighborLinks = map toFoundLink children,
              nodeAttributes = DAO.DAONode mroute_num
            }
  where
    toFoundLink (child_addr, lifetime) =
      FoundLink { targetNode = FindingID FindingDAO child_addr,
                  linkState = LinkToTarget,
                  linkAttributes = DAO.DAOLink lifetime
                }


data Heading = HDIO IPv6 DIO.DIONode
             | HDAO Word
             deriving (Show,Eq,Ord)

pHeading :: Parser Heading
pHeading = (fmap (uncurry HDIO) $ pDIONode)
           <|> (HDAO <$> pDAOLogHeader)

isAddressChar :: Char -> Bool
isAddressChar c = isHexDigit c || c == ':'

pAddress :: Parser IPv6
pAddress = fromS =<< P.munch1 isAddressChar
  where
    fromS str =
      case IPv6.decode $ pack str of
        Nothing -> fail ("Invalid IPv6 address: " <> str)
        Just addr -> return addr

data CompactID = CNodeID Int
               | CNodeAddress Word16
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

pMaybeCompactAddress :: Parser IPv6
pMaybeCompactAddress = pCompactAddress <|> pAddress

pRead :: Read a => String -> Parser a
pRead = either fail return . readEither

pNum :: Read a => Parser a
pNum = pRead =<< P.munch1 isDigit

pDIONode :: Parser (IPv6, DIO.DIONode)
pDIONode = do
  void $ P.string "nbr: own state, addr "
  addr <- pAddress
  void $ P.string ", DAG state: "
  void $ P.munch (\c -> c /= ',')
  void $ P.string ", MOP "
  void $ P.munch isDigit
  void $ P.string " OCP "
  void $ P.munch isDigit
  void $ P.string " rank "
  rank <- pNum
  void $ P.string " max-rank "
  void $ P.munch isDigit
  void $ P.string ", dioint "
  dio_int <- pNum
  let node = DIO.DIONode { DIO.rank = rank,
                           DIO.dioInterval = dio_int
                         }
  return (addr, node)

pExpectChar :: Char -> Parser Bool
pExpectChar exp_c = fmap (== Just exp_c) $ optional P.get

pNeighborAndRank :: Parser (IPv6, Rank)
pNeighborAndRank = spaced <|> non_spaced
  where
    spaced = do
      addr <- pMaybeCompactAddress
      P.skipSpaces
      rank <- pNum
      void $ P.string ", "
      return (addr, rank)
    non_spaced = do
      -- Rank is so large that there is no space between the address and rank.
      -- This case happens when the rank is 5 digits.
      addr_and_rank <- P.munch isAddressChar
      void $ P.string ", "
      let (addr_str, rank_str) = splitAt (length addr_and_rank - 5) addr_and_rank
      addr <- runParser' pMaybeCompactAddress ("Failed to parse address:" <> addr_str) addr_str
      rank <- runParser' pNum ("Failed to parser rank:" <> rank_str) rank_str
      return (addr, rank)

pDIONeighbor :: Parser (IPv6, DIO.DIOLink)
pDIONeighbor = do
  void $ P.string "nbr: "
  (neighbor_addr, neighbor_rank) <- pNeighborAndRank
  P.skipSpaces
  metric <- pNum
  void $ P.string " => "
  P.skipSpaces
  void $ P.munch isDigit -- rank_via_neighbor
  void $ P.string " -- "
  P.skipSpaces
  void $ P.munch isDigit -- freshness
  void $ pExpectChar ' '
  void $ pExpectChar 'r'
  void $ pExpectChar 'b'
  acceptable <- pExpectChar 'a'
  void $ pExpectChar 'f'
  preferred <- pExpectChar 'p'
  return ( neighbor_addr,
           DIO.DIOLink
           { DIO.neighborType = if preferred
                                  then DIO.PreferredParent
                                  else if acceptable
                                       then DIO.ParentCandidate
                                       else DIO.OtherNeighbor,
             DIO.neighborRank = neighbor_rank,
             DIO.metric = Just metric
           }
         )

pDIONeighborEnd :: Parser ()
pDIONeighborEnd = void $ P.string "nbr: end of list"

pLogHead :: Parser ()
pLogHead = do
  void $ P.char '['
  void $ P.munch (not . (== ']'))
  void $ P.string "] "

pDAOLogHeader :: Parser Word
pDAOLogHeader = do
  void $ P.string "links: "
  route_num <- pNum
  void $ P.string " routing links in total "
  return route_num

pDAOLink :: Parser (IPv6, Maybe (IPv6, Word))
pDAOLink = do
  void $ P.string "links: "
  child <- pMaybeCompactAddress
  mparent <- optional pParentAndLifetime
  return (child, mparent)
  where
    pParentAndLifetime = (,)
                         <$> (P.string "  to " *> pMaybeCompactAddress)
                         <*> (P.string " (lifetime: " *> pNum <* P.string " seconds)")
                         

pDAOLinkEnd :: Parser ()
pDAOLinkEnd = void $ P.string "links: end of list"

-- | Parse the head of Cooja log line, and return the timestamp and
-- node ID.
pCoojaLogHead :: Parser (Timestamp, Int)
pCoojaLogHead = do
  ts_min <- pNum
  void $ P.string ":"
  ts_sec <- pNum
  void $ P.string "."
  ts_msec <- pNum
  P.skipSpaces
  void $ P.string "ID:"
  node_id <- pNum
  P.skipSpaces
  return (makeTs ts_min ts_sec ts_msec, node_id)
  where
    makeTs :: Int64 -> Int64 -> Int64 -> Timestamp
    makeTs ts_min ts_sec ts_msec = fromEpochMillisecond ((ts_min * 60 + ts_sec) * 1000 + ts_msec)

-- | Same as 'pCoojaLogHead', but it returns the timestamp only.
pCoojaLogHead' :: Parser Timestamp
pCoojaLogHead' = fmap fst pCoojaLogHead

-- | Parser for head of syslog line with its default format.
-- @\"Mmm dd hh:mm:ss HOSTNAME TAG: \"@.
--
-- Because the format does not contain year, you have to pass it to
-- this function.
pSyslogHead :: Integer -- ^ year
            -> Maybe Time.TimeZone -- ^ optional time zone.
            -> Parser Timestamp
pSyslogHead year mtz = do
  ts <- pSyslogTimestamp year mtz
  P.skipSpaces
  void $ P.munch (not . isSpace) -- hostname
  P.skipSpaces
  void $ P.munch (not . isSpace) -- tag
  P.skipSpaces
  return ts

pSyslogTimestamp :: Integer -> Maybe Time.TimeZone -> Parser Timestamp
pSyslogTimestamp year mtz = do
  month <- pMonth
  P.skipSpaces
  day <- pNum
  P.skipSpaces
  hour <- pNum <* P.string ":"
  minute <- pNum <* P.string ":"
  sec <- pNum
  let lt = Time.LocalTime (Time.fromGregorian year month day) (Time.TimeOfDay hour minute sec)
  case mtz of
    Nothing -> return $ fromLocalTime lt
    Just tz -> return $ fromZonedTime $ Time.ZonedTime lt tz
  where
    pMonth = do
      mstr <- P.munch1 (not . isSpace)
      case mstr of
        "Jan" -> return 1
        "Feb" -> return 2
        "Mar" -> return 3
        "Apr" -> return 4
        "May" -> return 5
        "Jun" -> return 6
        "Jul" -> return 7
        "Aug" -> return 8
        "Sep" -> return 9
        "Oct" -> return 10
        "Nov" -> return 11
        "Dec" -> return 12
        _ -> fail ("Invalid for a month: " <> mstr)

