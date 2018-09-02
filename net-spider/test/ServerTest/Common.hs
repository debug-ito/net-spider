{-# LANGUAGE OverloadedStrings #-}
module ServerTest.Common
       ( withServer,
         withSpider,
         withSpider',
         sortSnapshotElements,
         AText(..),
         AInt(..),
         APorts(..),
         subIdWithAPorts,
         alignAPortsToLinkDirection
       ) where

import Control.Applicative ((<$>))
import Control.Category ((<<<))
import Control.Exception.Safe (bracket, withException)
import Data.Greskell (parseOneValue, gProperty, newBind)
import Data.List (sort)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.NeedEnv (needEnvHostPort, EnvMode(Need))

import NetSpider.Found (LinkState(..))
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..), VNode)
import NetSpider.Pair (Pair(..))
import NetSpider.Spider.Unify
  ( LinkSample(..)
  )
import NetSpider.Spider.Config
  ( Host, Port, Spider, Config(..), defConfig
  )
import NetSpider.Spider
  ( connectWith, close, clearAll
  )
import NetSpider.Snapshot
  ( SnapshotNode, SnapshotLink
  )

withServer :: SpecWith (Host,Port) -> Spec
withServer = before $ needEnvHostPort Need "NET_SPIDER_TEST"

withSpider :: Eq n => (Spider n na fla fla -> IO ()) -> (Host, Port) -> IO ()
withSpider = withSpider' defConfig

withSpider' :: Config n na fla sla -> (Spider n na fla sla -> IO ()) -> (Host, Port) -> IO ()
withSpider' orig_conf action (host, port) = bracket (connectWith conf) close $ \spider -> do
  clearAll spider
  action spider
    where
      conf = orig_conf { wsHost = host,
                         wsPort = port
                       }

sortSnapshotElements :: (Ord n, Eq na, Eq la)
                     => ([SnapshotNode n na], [SnapshotLink n la])
                     -> (Vector (SnapshotNode n na), Vector (SnapshotLink n la))
sortSnapshotElements (ns, ls) = (sortV ns, sortV ls)
  where
    sortV :: Ord a => [a] -> Vector a
    sortV = V.fromList . sort


newtype AText = AText Text
              deriving (Show,Eq,Ord)

instance NodeAttributes AText where
  writeNodeAttributes (AText t) = gProperty "text" <$> newBind t
  parseNodeAttributes ps = AText <$> parseOneValue "text" ps

instance LinkAttributes AText where
  writeLinkAttributes (AText t) = gProperty "text" <$> newBind t
  parseLinkAttributes ps = AText <$> parseOneValue "text" ps

newtype AInt = AInt Int
             deriving (Show,Eq,Ord)

instance NodeAttributes AInt where
  writeNodeAttributes (AInt n) = gProperty "integer" <$> newBind n
  parseNodeAttributes ps = AInt <$> parseOneValue "integer" ps

instance LinkAttributes AInt where
  writeLinkAttributes (AInt n) = gProperty "integer" <$> newBind n
  parseLinkAttributes ps = AInt <$> parseOneValue "integer" ps


-- | Pair of ports.
--
-- This type is used in two ways. (subject port, target port) and
-- (source port, destination port).
data APorts =
  APorts
  { apFst :: Text,
    apSnd :: Text
  }
  deriving (Show,Eq,Ord)

instance LinkAttributes APorts where
  writeLinkAttributes ap = do
    writeSource <- gProperty "subject_port" <$> newBind (apFst ap)
    writeDest <- gProperty "target_port" <$> newBind (apSnd ap)
    return (writeDest <<< writeSource)
  parseLinkAttributes ps = APorts
                           <$> parseOneValue "subject_port" ps
                           <*> parseOneValue "target_port" ps

swapAPorts :: APorts -> APorts
swapAPorts ap = ap { apFst = apSnd ap,
                     apSnd = apFst ap
                   }

-- | Link sub-ID with 'APorts'.
subIdWithAPorts :: LinkSample n APorts -> Pair (n, Text)
subIdWithAPorts ls = Pair ( (lsSubjectNode ls, apFst $ lsLinkAttributes ls),
                            (lsTargetNode ls, apSnd $ lsLinkAttributes ls)
                          )

-- | Sort 'APorts' according to 'LinkState'. This converts the
-- 'APorts' as (subject port, target port) into (source port,
-- destination port).
alignAPortsToLinkDirection :: LinkSample n APorts -> LinkSample n APorts
alignAPortsToLinkDirection ls = ls { lsLinkAttributes = updated }
  where
    updated = case lsLinkState ls of
               LinkToSubject -> swapAPorts $ lsLinkAttributes ls
               _ -> lsLinkAttributes ls
