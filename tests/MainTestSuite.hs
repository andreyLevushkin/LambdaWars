module Main (
    main
 ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import UITest


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    testGroup "UI"
    [
       testGroup    "Migrated from HUnit" $ hUnitTestToTests testRenderBullet
     , testProperty "SVG for bullet"      propRenderedSvgContainsCircle
    ]
  ]
