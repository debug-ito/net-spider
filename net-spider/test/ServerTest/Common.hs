{-# LANGUAGE OverloadedStrings #-}
module ServerTest.Common
       ( withServer,
         withSpider,
         withSpider',
         sortSnapshotElements,
         AText(..),
         AInt(..),
         APorts(..)
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

import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..), VNode)
import NetSpider.Spider.Config
  ( Host, Port, Spider, Config(..), defConfig
  )
import NetSpider.Spider
  ( connectWith, close, clearAll
  )
import NetSpider.Snapshot
  ( SnapshotElement, SnapshotNode, SnapshotLink
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
                     => [SnapshotElement n na la]
                     -> (Vector (SnapshotNode n na), Vector (SnapshotLink n la))
sortSnapshotElements es = toVs $ foldr f mempty $ sort es
  where
    f (Left n)  (ns, ls) = (n : ns, ls)
    f (Right l) (ns, ls) = (ns,     l : ls)
    toVs (ll, rl) = (V.fromList ll, V.fromList rl)


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
data APorts =
  APorts
  { apSource :: Text,
    apDestination :: Text
  }
  deriving (Show,Eq,Ord)

instance LinkAttributes APorts where
  writeLinkAttributes ap = do
    writeSource <- gProperty "source_port" <$> newBind (apSource ap)
    writeDest <- gProperty "destination_port" <$> newBind (apDestination ap)
    return (writeDest <<< writeSource)
  parseLinkAttributes ps = APorts
                           <$> parseOneValue "source_port" ps
                           <*> parseOneValue "destination_port" ps
