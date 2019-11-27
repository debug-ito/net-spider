module ServerTest (main,spec) where

import Test.Hspec
import Test.Hspec.NeedEnv (needEnvHostPort, EnvMode(Need))

main :: IO ()
main = hspec spec

spec :: Spec
spec = before (needEnvHostPort Need "NET_SPIDER") $ describe "server test" $ do
  specify "hoge" $ \(_, _) -> do
    True `shouldBe` False -- TODO.
  
