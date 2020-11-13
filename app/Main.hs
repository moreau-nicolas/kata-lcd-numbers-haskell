module Main where

import Lcd (lcdNumber, width, height)
import Control.Category ((>>>))
import System.Environment (getArgs)

main :: IO ()
main = getArgs
    >>= mapM_ (read >>> lcdNumber >>> width 3 >>> height 2 >>> show >>> putStrLn)
