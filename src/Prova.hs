module Prova where


data VType = TDouble Double | TBool Bool | TArray [VType]

det (l:ls) = 
    case l of
        TDouble d -> "double " ++ det ls
        TBool b -> "bool " ++ det ls
        TArray a -> "array " ++ det ls


