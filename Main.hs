module Main where

import Parse
import DrawMats

import Control.Monad.State
import System.IO
import System.Environment

main = do
    args <- getArgs
    script <- readFile (head args)
    let cmds = parse $ lines script :: [StateT DrawMats IO ()]
    runStateT (sequence_ cmds) emptyDM

