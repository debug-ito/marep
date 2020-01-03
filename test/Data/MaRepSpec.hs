{-# LANGUAGE ScopedTypeVariables #-}
module Data.MaRepSpec (main, spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BSL
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Test.Hspec

import Data.MaRep (replaceAll, Decomposable)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "replaceAll" $ do
    specString "String" (Proxy :: Proxy String)
    -- specString "strict Text" (Proxy :: Proxy Text)
    -- specString "lazy Text" (Proxy :: Proxy TL.Text)
    -- specString "strict ByteString" (Proxy :: Proxy ByteString)
    -- specString "lazy ByteString" (Proxy :: Proxy BSL.ByteString)

specString :: forall str . (IsString str, Decomposable str, Show str, Eq str, Semigroup str)
           => String -- ^ Type name
           -> Proxy str -- ^ specifies the string type
           -> Spec
specString type_name _ = do
  describe type_name $ do
    specify "empty input" $ do
      let input = ("" :: str)
      replaceAll (\_ -> error "this should not happen") input `shouldBe` ""
    specify "matcher for empty string should be ignored" $ do
      let input = ("aaa" :: str)
      replaceAll (\x -> if x == "" then Just "bbb" else Nothing) input `shouldBe` input
    specify "match one letter, at the middle" $ do
      let input = ("uffu" :: str)
      replaceAll (\x -> if x == "f" then Just "fff" else Nothing) input `shouldBe` "uffffffu"
    specify "match one letter, at the start and the end" $ do
      let input = ("fuuf" :: str)
      replaceAll (\x -> if x == "f" then Just "fff" else Nothing) input `shouldBe` "fffuufff"
    specify "match different lengths of strings" $ do
      let input = ("gfgoihgfhiig" :: str)
          matcher x =
            case x of
              "" -> Just "this should not happen"
              "f" -> Just "fff"
              "go" -> Just "gg"
              "hii" -> Just "H"
              _ -> Nothing
      replaceAll matcher input `shouldBe` "gfffggihgfffHg"
