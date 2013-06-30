module Main where

import System.Random

import Core
import Engine
import GLUI

import SimpleBots

main = glMain

glMain = do
  gen <- getStdGen
  let bots  = [runInCircle, fireBot, fireBot, fireBot, fireBot, runInCircle]
  let world = (newWorld gen bots)
  showBattle world stepWorld


