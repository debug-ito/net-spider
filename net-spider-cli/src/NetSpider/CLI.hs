-- |
-- Module: NetSpider.CLI
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module NetSpider.CLI
       (parserSnapshotQuery) where

import Control.Applicative ((<$>), (<*>), many)
import Data.Monoid (mconcat)
import qualified NetSpider.Query as Q
import NetSpider.Timestamp (Timestamp)
import qualified Options.Applicative as Opt

-- | Configuration for parsers.
data Config n na sla =
  Config
  { nodeIDParser :: Opt.ReadM n
  }

-- | Command-line parser for 'Q.Query'.
parserSnapshotQuery :: Config n na sla
                    -> Q.Query n na fla sla -- ^ Basis for the query.
                    -> Opt.Parser (Q.Query n na fla sla)
parserSnapshotQuery conf basis = fmap fromParsedElement the_parser
  where
    fromParsedElement (sf, m_ti) =
      let q = basis { Q.startsFrom = sf }
      in maybe q (\ti -> q { Q.timeInterval = ti } ) m_ti
    the_parser = (,) <$> pStartsFrom <*> pTimeInterval
    rNodeID = nodeIDParser conf
    pStartsFrom = many $ Opt.option rNodeID $ mconcat
                  [ Opt.short 's',
                    Opt.long "starts-from",
                    Opt.help "ID of a node from which the Spider starts traversing the history graph. You can specify this option multiple times.",
                    Opt.metavar "NODE-ID"
                  ]
    pTimeInterval = undefined -- TODO

type ErrorMsg = String

-- TODO: time intervalじゃなくて、上限と下限を別オプションにするほうが
-- 使いやすいか？その場合、inclusiveかexclusiveかはどう表現するか？ =とかつけるか？

-- | Parse a 'String' into a time interval.
--
-- >>> parseTimeInterval "[2019-10-09T12:03:22, 2019-10-09T13:05:33.231]"
-- TODO
-- >>> parseTimeInterval "(2019-10-09T12:03:22, 2019-10-09T13:05:33.231]"
parseTimeInterval :: String -> Either ErrorMsg (Q.Interval Timestamp)
parseTimeInterval = undefined
