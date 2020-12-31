module Main where

import Parser ( parse )
import Interpreter ( Env, exec )



main :: IO ()
main =
    do
        input <- getLine;
        let p = fst(parse input)
        let m = exec [] p
        print m 
