module NetSpider.CLI.SnapshotSpec (main,spec) where

import Data.List (isInfixOf)
import NetSpider.Interval (interval, Extended(..))
import NetSpider.Query
  ( startsFrom, defQuery, timeInterval,
    foundNodePolicy, policyAppend,
    Query
  )
import NetSpider.Timestamp (fromEpochMillisecond)
import qualified Options.Applicative as Opt
import Test.Hspec

import NetSpider.CLI.TestCommon (runP)
import NetSpider.CLI.Snapshot (SnapshotConfig(..), parserSnapshotQuery, makeSnapshotQuery)

defConfig :: SnapshotConfig Int
defConfig =
  SnapshotConfig
  { nodeIDReader = Opt.auto,
    startsFromAsArguments = False
  }

main :: IO ()
main = hspec spec

parseSQ :: SnapshotConfig Int -> [String] -> Either String (Query Int () () ())
parseSQ sconf args = makeSnapshotQuery base_query =<< runP (parserSnapshotQuery sconf) args
  where
    base_query = (defQuery []) { foundNodePolicy = policyAppend }

spec :: Spec
spec = describe "parserSnapshotQuery" $ do
  specify "default" $ do
    let (Right got) = parseSQ defConfig []
    startsFrom got `shouldBe` []
    timeInterval got `shouldBe` interval (NegInf, False) (PosInf, False)
    foundNodePolicy got `shouldBe` policyAppend
  specify "time-from" $ do
    let (Right got) = parseSQ defConfig
                      ["--time-from", "2019-02-19T11:12:00"]
    timeInterval got `shouldBe`
      interval (Finite $ fromEpochMillisecond 1550574720000, True)
               (PosInf, False)
  specify "time-to with exclusive" $ do
    let (Right got) = parseSQ defConfig
                      ["--time-to", "x2017-12-20T19:22:02"]
    timeInterval got `shouldBe`
      interval (NegInf, False)
               (Finite $ fromEpochMillisecond 1513797722000, False)
  specify "both time-from and time-to with inclusive" $ do
    let (Right got) = parseSQ defConfig
                      ["-f", "i2018-10-11T14:13:33", "-t", "i2018-10-11T14:13:50.332"]
    timeInterval got `shouldBe`
      interval (Finite $ fromEpochMillisecond 1539267213000, True)
               (Finite $ fromEpochMillisecond 1539267230332, True)
  specify "explicit infinity" $ do
    let (Right got) = parseSQ defConfig
                      ["--time-from", "-inf", "--time-to", "+inf"]
    timeInterval got `shouldBe` interval (NegInf, True) (PosInf, True)
  specify "multiple starts-from" $ do
    let (Right got) = parseSQ defConfig
                      ["-s", "10", "-s", "12", "-s", "15"]
    startsFrom got `shouldBe` [10,12,15]
  let argsConfig = defConfig { startsFromAsArguments = True }
  specify "startsFromAsArguments" $ do
    let (Right got) = parseSQ argsConfig
                      ["143", "200", "473","21"]
    startsFrom got `shouldBe` [143, 200, 473, 21]
  specify "startsFromAsArguments - -s still enabled" $ do
    let (Right got) = parseSQ argsConfig
                      ["90", "-s", "181"]
    startsFrom got `shouldBe` [181, 90]
  specify "duration + time-from" $ do
    let (Right got) = parseSQ defConfig
                      ["--duration", "3600", "--time-from", "i2019-04-30T19:03:33"]
    timeInterval got `shouldBe`
      interval (Finite $ fromEpochMillisecond 1556651013000, True)
               (Finite $ fromEpochMillisecond (1556651013000 + 3600000), False)
  specify "duration + time-to" $ do
    let (Right got) = parseSQ defConfig
                      ["-d", "600", "--time-to", "x2019-04-30T19:03:33"]
    timeInterval got `shouldBe`
      interval (Finite $ fromEpochMillisecond (1556651013000 - 600000), True)
               (Finite $ fromEpochMillisecond 1556651013000, False)
  specify "duration + time-to + time-from expects error" $ do
    let (Left err) = parseSQ defConfig
                     ["-d", "600", "--time-to", "x2019-04-30T19:03:33", "--time-from", "x2019-04-30T17:00:52"]
    err `shouldSatisfy` (isInfixOf "all --time-to, --time-from and --duration is not allowed")
  specify "duration without time-to or time-from" $ do
    let (Left err) = parseSQ defConfig
                     ["-d", "600"]
    err `shouldSatisfy` (isInfixOf "--duration only is not allowed")

