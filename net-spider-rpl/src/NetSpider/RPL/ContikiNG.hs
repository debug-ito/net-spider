-- |
-- Module: NetSpider.RPL.ContikiNG
-- Description: Parser for Contiki-NG logs about RPL
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.RPL.ContikiNG
  ( -- * Parser functions
    parseFile,
    FoundNodeLocal,
    FoundNodeSR,
    -- * Parser components
    Parser,
    pCoojaLogHead,
    pCoojaLogHead',
    pSyslogHead
  ) where

import Control.Applicative ((<|>), (<$>), (<*>), (*>), (<*), many, optional)
import Control.Monad (void)
import Data.Bits (shift)
import Data.Char (isDigit, isHexDigit, isSpace)
import Data.Int (Int64)
import Data.List (sortOn, reverse)
import Data.Monoid ((<>))
import Data.Text (pack)
import qualified Data.Time as Time
import Data.Word (Word16)
import GHC.Exts (groupWith)
import Net.IPv6 (IPv6)
import qualified Net.IPv6 as IPv6
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(LinkToTarget))
import NetSpider.Timestamp (Timestamp, fromEpochMillisecond, fromLocalTime, fromZonedTime)
import System.IO (withFile, IOMode(ReadMode), hGetLine, hIsEOF)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read (readEither)

import NetSpider.RPL.FindingID (FindingID(FindingID), FindingType(..))
import NetSpider.RPL.IPv6 (isLinkLocal, setPrefix, getPrefix)
import qualified NetSpider.RPL.Local as Local
import qualified NetSpider.RPL.SR as SR

type Parser = P.ReadP

type FoundNodeLocal = FoundNode FindingID Local.LocalNode Local.LocalLink

type FoundNodeSR = FoundNode FindingID SR.SRNode SR.SRLink

runParser :: Parser a -> String -> Maybe a
runParser p input = extract $ sortPairs $ P.readP_to_S p input
  where
    sortPairs = sortOn $ \(_, rest) -> length rest
    extract [] = Nothing
    extract ((a,_) : _) = Just a

parseFile :: Parser Timestamp -> FilePath -> IO ([FoundNodeLocal], [FoundNodeSR])
parseFile pTimestamp input_file = withFile input_file ReadMode $ onHandle
  where
    onHandle h = go ([], [])
      where
        go (acc_local, acc_sr) = do
          mentry <- parseOneEntry pTimestamp $ tryGetLine h
          case mentry of
            Nothing -> return (reverse acc_local, reverse acc_sr)
            Just (PELocal fl) -> go (fl : acc_local, acc_sr)
            Just (PESR srs) -> go (acc_local, srs ++ acc_sr)
            Just PEMisc -> go (acc_local, acc_sr)
    tryGetLine h = do
      eof <- hIsEOF h
      if eof
        then return Nothing
        else Just <$> hGetLine h

data ParseEntry = PELocal FoundNodeLocal
                | PESR [FoundNodeSR]
                | PEMisc
                deriving (Show,Eq)

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
            Just (ts, HLocal addr node) -> proceedLocal ts addr node
            Just (ts, HSR) -> proceedSR ts
    withPrefix p = pTimestamp *> pLogHead *> p
    proceedLocal ts addr node = do
      mlinks <- readUntil getL (withPrefix pLocalNeighbor) (withPrefix pLocalNeighborEnd)
      case mlinks of
        Nothing -> return Nothing
        Just links -> return $ Just $ PELocal $ makeFoundNodeLocal ts addr node $ map (setAddressPrefix addr) links
    proceedSR ts = do
      mlinks <- readUntil getL (withPrefix pSRLink) (withPrefix pSRLinkEnd)
      case mlinks of
        Nothing -> return Nothing
        Just links -> return $ Just $ PESR $ map (uncurry $ makeFoundNodeSR ts) $ groupSRLinks links
    groupSRLinks :: [(IPv6, Maybe IPv6)] -> [(IPv6, [IPv6])]
    groupSRLinks links = map toTuple $ groupWith byParentAddr $ (filterOutRoot =<< links)
      where
        filterOutRoot (_, Nothing) = []
        filterOutRoot (c, Just p) = [(c, p)]
        byParentAddr = snd
        toTuple [] = error "groupSRLinks: this should not happen"
        toTuple ((c1, p) : rest) = (p, c1 : map fst rest)
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
            Nothing -> return Nothing
            Just (Left _) -> return $ Just $ reverse acc
            Just (Right body) -> go (body : acc)

makeFoundNodeLocal :: Timestamp -> IPv6 -> Local.LocalNode -> [(IPv6, Local.LocalLink)] -> FoundNodeLocal
makeFoundNodeLocal ts self_addr node_attr neighbors =
  FoundNode { subjectNode = FindingID FindingLocal self_addr,
              foundAt = ts,
              neighborLinks = map toFoundLink neighbors,
              nodeAttributes = node_attr
            }
  where
    toFoundLink (neighbor_addr, ll) =
      FoundLink { targetNode = FindingID FindingLocal neighbor_addr,
                  linkState = LinkToTarget,
                  linkAttributes = ll
                }

makeFoundNodeSR :: Timestamp -> IPv6 -> [IPv6] -> FoundNodeSR
makeFoundNodeSR ts parent_addr children =
  FoundNode { subjectNode = FindingID FindingSR parent_addr,
              foundAt = ts,
              neighborLinks = map toFoundLink children,
              nodeAttributes = SR.SRNode
            }
  where
    toFoundLink child_addr =
      FoundLink { targetNode = FindingID FindingSR child_addr,
                  linkState = LinkToTarget,
                  linkAttributes = SR.SRLink
                }


data Heading = HLocal IPv6 Local.LocalNode
             | HSR
             deriving (Show,Eq,Ord)

pHeading :: Parser Heading
pHeading = (fmap (uncurry HLocal) $ pLocalNode)
           <|> (pSRLogHeader *> pure HSR)

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

pMaybeCompactAddress :: Parser IPv6
pMaybeCompactAddress = pCompactAddress <|> pAddress

pRead :: Read a => String -> Parser a
pRead = either fail return . readEither

pNum :: Read a => Parser a
pNum = pRead =<< P.munch1 isDigit

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
  rank <- pNum
  void $ P.string " max-rank "
  void $ P.munch isDigit
  void $ P.string ", dioint "
  void $ P.munch isDigit -- TODO: store DIO interval
  return (addr, Local.LocalNode { Local.rank = rank })

pExpectChar :: Char -> Parser Bool
pExpectChar exp_c = fmap (== Just exp_c) $ optional P.get

pLocalNeighbor :: Parser (IPv6, Local.LocalLink)
pLocalNeighbor = do
  void $ P.string "nbr: "
  neighbor_addr <- pMaybeCompactAddress
  P.skipSpaces
  neighbor_rank <- pNum
  void $ P.string ", "
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

pLocalNeighborEnd :: Parser ()
pLocalNeighborEnd = void $ P.string "nbr: end of list"

pLogHead :: Parser ()
pLogHead = do
  void $ P.char '['
  void $ P.munch (not . (== ']'))
  void $ P.string "] "

pSRLogHeader :: Parser ()
pSRLogHeader = do
  void $ P.string "links: "
  void $ P.munch1 isDigit
  void $ P.string " routing links in total "

pSRLink :: Parser (IPv6, Maybe IPv6)
pSRLink = do
  void $ P.string "links: "
  child <- pMaybeCompactAddress
  mparent <- optional (P.string "  to " *> pMaybeCompactAddress)
  return (child, mparent)

pSRLinkEnd :: Parser ()
pSRLinkEnd = void $ P.string "links: end of list"

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

