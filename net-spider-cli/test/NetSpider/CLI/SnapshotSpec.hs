module NetSpider.CLI.SnapshotSpec (main,spec) where

import NetSpider.Interval (interval, Extended(..))
import NetSpider.Query (startsFrom, defQuery, timeInterval)
import qualified Options.Applicative as Opt
import Test.Hspec

import NetSpider.CLI.TestCommon (runP)
import NetSpider.CLI.Snapshot (Config(..), parserSnapshotQuery)

defConfig :: Config Int () () ()
defConfig = Config { nodeIDParser = Opt.auto,
                     basisSnapshotQuery = defQuery []
                   }

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parserSnapshotQuery" $ do
  specify "default" $ do
    let (Right got) = runP (parserSnapshotQuery defConfig) []
        expected = defQuery []
    startsFrom got `shouldMatchList` startsFrom expected
    timeInterval got `shouldBe` interval (NegInf, False) (PosInf, True) -- TODO: does it fail??

-- TODO: more test cases
  
