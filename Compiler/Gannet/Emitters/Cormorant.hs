-- | Emitter for Cormorant, the C back-end. Work in progress, requires "Gannet.Symbolizer.InferTypes" to be completed.
-- Likely to be postponed until the Gannet-C front-end is ready. 
module Gannet.Emitters.Cormorant (
emitCCode
) where
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration    
import Gannet.Emitters.ConfigReader

import Control.Monad.State hiding (join)              
import qualified Data.Map as Hash 
import Data.Char (toUpper)

translation_table_list :: [(String,(String->[String]->String))]
translation_table_list=[
    ("let",\op args -> "do {\n " ++ (join ";\n " args) ++ "}\n"),
    ("assign",\op args -> assignstr args),
    ("read",\op args -> readstr args),
    ("update",\op args ->  updatestr args),
    ("if",\op args -> "do {"++op++"( " ++ (head args) ++ "){\n" ++ (args !! 1) ++ "\n} else {\n" ++ (args !! 2) ++ "\n}}"),
    ("return",\op args -> returnstr args),
    ("lambda",\op args -> lambdastr args),
    ("apply",\op args -> ['&']++(head args)++"("++(join "," (tail args))++")"),
    ("rnd", \op args -> "rand()" ),
    ("+", \op args -> "("++(join op args)++")"),
    ("-", \op args -> "("++(join op args)++")"),
    ("*", \op args -> "("++(join op args)++")"),
    ("/", \op args -> "("++(join op args)++")"),
    ("<", \op args -> join op args),
    (">", \op args -> join op args),
    ("<=", \op args -> join op args),
    (">=", \op args -> join op args),    
    ("==", \op args -> join op args),    
    ("list",\op args -> "["++(join "," args)++"]"),
    ("head",\op args -> (head args)++"->[0]"),
    ("tail",\op args ->"[@{" ++ (head args) ++"}[1..scalar("++['@']++"{" ++ (head args) ++ "})-1]]"),
    ("length",\op args -> "scalar(@{"++(head args)++"})"),
    ("append",\op args -> "[@{"++ (join "},@{" args) ++ "}]"),
    ("cons",\op args -> "[push(@{" ++ (args !! 0) ++ "}," ++ (args !! 1) ++ ")]"),
    ("label",\op args -> (args !! 0) ++ ": " ++ (args !! 1)),
    ("display",\op args -> "print(" ++  (join ", " args) ++ ",\"\\n\")"),
    ("fopen",\op args -> let 
                                rw | (args !! 2) == "\"r\"" = "<"
                                   | (args !! 2) == "\"w\"" = ">"
                                   | otherwise = "<"
                          in
                                "open FD"++(args !! 0)++", \""++rw++ (tail (args !! 1))),
    ("fclose",\op args -> "close FD"++(args !! 0) ),       
    ("iowrite",\op args -> "do {my $_lw="++(args !! 1)++"; if ($_lw) {print FD"++(args !! 0)++" $_lw;1;} else {0}}" ),                         
    ("ioread",\op args -> "do {my $_lr=<FD"++(args !! 0)++">; if ($_lr) {$_l} else {0} }" ),
    ("buffer", \op args -> "do {my $_f=sub {"++(args !! 1)++"};push @{$buffers["++(args !! 0)++"][0]}, &{$_f}() ;${$buffers["++(args !! 0)++"][1]}=$_f}" ),
    ("stream", \op args -> "do {push @{$buffers["++(args !! 0)++"][0]}, &{${$buffers["++(args !! 0)++"][1]}}();shift @{$buffers["++(args !! 0)++"][0]}}" ),
    ("iter", \op args -> "do  { for (1.."++(args !!0)++"){"++(args !! 1)++"}}" ),
    ("default", \op args -> (op ++ "(" ++ (join ", " args) ++ ")"))
    ]

{-
WV: we could incorporate these in the table
-}    
returnstr args 
    | length args > 1 = "do {" ++ (join ";\n" (tail args)) ++ ";\n" ++ (head args) ++ "}"
    | otherwise = (head args)
lambdastr args = 
    let 
        body:rlargs=reverse args
        largs=reverse rlargs
    in 
        "sub {(" ++ (join "," (map (\x->("my "++x)) largs)) ++ ")=@_;" ++ body ++"}"
readstr =returnstr
updatestr args
    | length args > 2 = "do {" ++ (join ";\n" (tail (tail args))) ++ ";\n" ++ (args !! 0) ++  " = " ++ (args !! 1)  ++ "\n}"     
    | otherwise = (args !! 0) ++  " = " ++ (args !! 1) 
assignstr args
    | length args > 2 = "do {" ++ (join ";\n" (tail (tail args))) ++ ";\n" ++  "my " ++ (args !! 0) ++ " = " ++ (args !! 1) ++ "\n}"   
    | otherwise =  "my " ++ (args !! 0) ++ " = " ++ (args !! 1)     

join :: String -> [String] -> String
join sep ls = foldl1 (\x y->x++sep++y) ls
     
type CodeTable = Hash.Map String (String->[String] -> String)
code_table :: CodeTable
code_table= Hash.fromList translation_table_list


---------------------------------------------------------------------------------------------------

headerStr = ""

emitCCode :: PacketList  ->  String
emitCCode pl  = emitRuntimeCode pl code_table

emitRuntimeCode :: PacketList -> CodeTable -> String 
emitRuntimeCode pl code_table = ""
---------------------------------------------------------------------------------------------------
{-
So, basically, we emit the header file with the function declarations and the 
source file with the definitions.
Currently, as I'm lazy and the whole thing is stand-alone, the header has a
static array of 32-bit ints as memory


-}