{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
module Data.MessagePack.ArbitrarySpec where

import           Data.MessagePack.Arbitrary ()
import           Data.MessagePack.Types     (Object (..))
import           Test.Hspec                 (Spec, describe, it, shouldBe)
import           Test.QuickCheck            (Arbitrary (..), property)

spec :: Spec
spec =
    describe "arbitrary" $
        it "produces reasonably-sized objects" $
            property $ \(x :: Object) -> read (show x) `shouldBe` x
