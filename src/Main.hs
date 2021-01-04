module Main where

import Parser ( parse, isParseFailed )
import Evaluator ( Env, exec, Env, showEnv )
import Tree ( Program (..) ) 

lineString :: [Char]
lineString = "IMPterpreter> "

-- A dummy function used to force eager evaluation in Haskell
-- in order to catch errors before printing the environment
force :: p -> [Char]
force env = ""

menu :: Env -> String -> IO ()
menu env "" = 
    do
        putStr lineString
        input <- getLine
        menu env input

menu env (':':('l':(' ':file))) =
    do   
        prog <- readFile file
        execute env prog

menu env (':':('e':('n':('v':other)))) =
    do   
        putStrLn ("\nEnviroment:\n" ++ showEnv env)   
        execute env ""

menu env (':':('c':('l':other))) =
    do   
        execute [] ""

menu env (':':('q':other)) =
    do
        putStrLn "Goodbye!"

menu env prog = execute env prog

execute :: Env -> [Char] -> IO ()
execute env prog = 
    do
        let parsedProg = parse prog
        if isParseFailed parsedProg
            then do
                putStrLn "Parsing failed, string not consumed:"
                putStrLn (snd parsedProg)
                putStr lineString
                input <- getLine 
                menu env input
            else do
                let newEnv = exec env (fst parsedProg)
                putStr (force $! newEnv) -- Force the evaluation of the environment
                putStr lineString
                input <- getLine 
                menu newEnv input
        
main :: IO ()
main =
    do
        putStrLn "-------------------------------------------------------------------------------------------------"
        putStrLn " ██ ███    ███ ██████  ████████ ███████ ██████  ██████  ██████  ███████ ████████ ███████ ██████  "
        putStrLn " ██ ████  ████ ██   ██    ██    ██      ██   ██ ██   ██ ██   ██ ██         ██    ██      ██   ██ "
        putStrLn " ██ ██ ████ ██ ██████     ██    █████   ██████  ██████  ██████  █████      ██    █████   ██████  "
        putStrLn " ██ ██  ██  ██ ██         ██    ██      ██   ██ ██      ██   ██ ██         ██    ██      ██   ██ "
        putStrLn " ██ ██      ██ ██         ██    ███████ ██   ██ ██      ██   ██ ███████    ██    ███████ ██   ██ "
        putStrLn "-------------------------------------- Pasquale De Marinis --------------------------------------\n"    
        putStrLn "Type the instructions to be executed"
        putStrLn "Type ':l filename' to load and execute instructions from file"
        putStrLn "Type ':env' to show the enviroment"
        putStrLn "Type ':cl' to clear the enviroment"
        putStrLn "Type ':q' to quit\n"
        menu [] ""
        