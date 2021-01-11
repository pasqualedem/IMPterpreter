module Main where

import IMPterpreter.Parser ( isParseFailed, parse )
import IMPterpreter.Evaluator ( Env, getVarValue, showEnv, exec )

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
        putStrLn ("\nEnvironment:\n" ++ showEnv env)   
        execute env ""

menu env (':':('c':('l':other))) =
    do   
        execute [] ""

menu env (':':('p':(' ':var)))=
    do  
        case getVarValue env var of
            Left v -> print v
            Right err -> print err
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
                case newEnv of
                    Left okEnv -> 
                        do
                            putStr lineString
                            input <- getLine 
                            menu okEnv input
                    Right err ->
                        do
                            putStr (show err ++ "\n") 
                            putStr lineString
                            input <- getLine 
                            menu env input

        
help :: IO ()
help = 
    do
        putStrLn "':l filename' loads and executes instructions from file"
        putStrLn "':env' shows the enviroment"
        putStrLn "':p v' shows the value of variable with name 'v'"
        putStrLn "':cl' clears the enviroment"
        putStrLn "':h' shows this message"
        putStrLn "':q' quits\n"

shell :: IO ()
shell =
    do
        putStrLn "-------------------------------------------------------------------------------------------------"
        putStrLn " ██ ███    ███ ██████  ████████ ███████ ██████  ██████  ██████  ███████ ████████ ███████ ██████  "
        putStrLn " ██ ████  ████ ██   ██    ██    ██      ██   ██ ██   ██ ██   ██ ██         ██    ██      ██   ██ "
        putStrLn " ██ ██ ████ ██ ██████     ██    █████   ██████  ██████  ██████  █████      ██    █████   ██████  "
        putStrLn " ██ ██  ██  ██ ██         ██    ██      ██   ██ ██      ██   ██ ██         ██    ██      ██   ██ "
        putStrLn " ██ ██      ██ ██         ██    ███████ ██   ██ ██      ██   ██ ███████    ██    ███████ ██   ██ "
        putStrLn "-------------------------------------- Pasquale De Marinis --------------------------------------\n"    
        putStrLn "Type the instructions to be executed or use ':l filename' to load from file."
        putStrLn "Use ':h' to show all commands."
        menu [] ""

main :: IO ()
main = shell