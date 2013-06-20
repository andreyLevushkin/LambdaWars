module Main where

import System.Random

import Core
import Engine
import GLUI

import SimpleBots

main = glMain

glMain = do
  gen <- getStdGen
  let bots  = [runInCircle, sittingDuck, fireBot]
  let world = (newWorld gen bots)
  showBattle world stepWorld


