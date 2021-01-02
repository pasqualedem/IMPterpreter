module Main where

import Parser ( parse, parseFailed )
import Evaluator ( Env, exec, Env, showEnv )
import Tree ( Program (..) ) 


menu :: Env -> String -> IO ()
menu _ "" = 
    do
        input <- getLine
        menu [] input

menu env (':':('l':file)) =
    do   
        prog <- readFile file
        execute env prog

menu env (':':('q':other)) =
    do   
        putStrLn "Goodbye!"

menu env prog = execute env prog

execute :: Env -> [Char] -> IO ()
execute env prog = 
    do
        let parsedProg = parse prog
        if parseFailed parsedProg
            then do
                putStrLn "Parsing failed, string not consumed:"
                putStrLn (snd parsedProg)
                input <- getLine 
                menu env input
            else do
                let newEnv = exec env (fst parsedProg);
                putStrLn ("Enviroment:\n" ++ showEnv newEnv)
                input <- getLine 
                menu newEnv input
        
main :: IO ()
main =
    do
        putStrLn "-------------------------------------------------------------------------------"
        putStrLn "                 ____                                                         "
        putStrLn "    |  |\\    /| |    |  |    ____       ____        ____   |    ____          "
        putStrLn "    |  | \\  / | |    | ---- |    | | / |    | |  / |    | ---- |    | |  /    "
        putStrLn "    |  |  \\/  | |____|  |   |____| |/  |____| |/   |____|  |   |____| |/      "
        putStrLn "    |  |      | |       |   |      |   |      |    |       |   |      |       "
        putStrLn "    |  |      | |       |   |____  |   |      |    |____   |   |____  |       "
        putStrLn "                                                                               "
        putStrLn "----------------------------- Pasquale De Marinis -----------------------------"
        putStrLn "Type the instructions to be executed"
        putStrLn "Type :l file to load and execute instructions in a file"
        putStrLn "Type :q to quit"
        menu [] ""

