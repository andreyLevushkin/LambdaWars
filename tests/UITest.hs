module UITest where

import UI hiding (renderSvg)
import Core

import Data.List (isInfixOf)
import Test.HUnit

import Text.Blaze.Svg.Renderer.String (renderSvg)

testRenderBullet = TestCase (do
    let bullet = Bullet {
        bulletPosition = (1,2),
        bulletVelocity = undefined -- unimportant for now
    }

    let expected = "<circle class=\"bullet\" cx=\"1.0\" cy=\"2.0\" r=\"5\" />"
    assertEqual "" expected (renderSvg $ renderBullet bullet))

