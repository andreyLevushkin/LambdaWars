module UITest where

import UI hiding (renderSvg)
import Core

import Control.Applicative
import Data.List (isInfixOf)
import Data.Vector.Class
import Data.Vector.V2

import Test.HUnit
import Test.QuickCheck

import Text.Blaze.Svg.Renderer.String (renderSvg)

testRenderBullet = TestCase (do
    let bullet = Bullet {
        _bulletPosition = Vector2 1 2,
        _bulletVelocity = undefined -- unimportant for now
    }

    let expected = "<circle class=\"bullet\" cx=\"1.0\" cy=\"2.0\" r=\"5\" />"
    assertEqual "" expected (renderSvg $ renderBullet bullet))

instance Arbitrary Bullet where
    arbitrary = Bullet <$> arbitrary <*> arbitrary

instance Arbitrary Vector2  where
    arbitrary = Vector2 <$> arbitrary <*> arbitrary

-- Useless test, but useful for checking QuickCheck is set up correctly :)
propRenderedSvgContainsCircle bullet = "circle" `isInfixOf` (renderSvg $ renderBullet bullet)
