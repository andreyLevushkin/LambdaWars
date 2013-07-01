module Main where

import Engine
import GLUI

import SimpleBots

main :: IO ()
main = runBattle openGLUI [
        ("bot1", runInCircle), 
        ("bot2", fireBot)
    ]


