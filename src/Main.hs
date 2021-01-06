module Main where

import Parser ( parse, isParseFailed )
import Evaluator
import Tree ( Program (..) ) 

lineString :: [Char]
lineString = "IMPterpreter> "

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

menu env (':':('p':(' ':var)))=
    do
        print (getVarValue env var)
        execute env ""

menu env (':':('h':other))=
    do
        help
        execute env ""

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
                putStr lineString
                input <- getLine 
                menu newEnv input
        
help = 
    do
        putStrLn "':l filename' loads and executes instructions from file"
        putStrLn "':env' shows the enviroment"
        putStrLn "':p v' shows the value of variable with name 'v'"
        putStrLn "':cl' clears the enviroment"
        putStrLn "':q' quits\n"

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
        putStrLn "Type the instructions to be executedor use ':l filename' to load from file."
        putStrLn "Use ':h' to show all commands."
        menu [] ""
