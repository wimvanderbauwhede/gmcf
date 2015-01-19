{-# LANGUAGE CPP #-}

module Main (
        main,
        ymlFileName
) where 
import System.Environment
import Gannet.Tokenizer
import Gannet.Symbolizer
import Gannet.Symbolizer.InferTypes
import Gannet.Packetizer
import Gannet.Bytecodizer
import Gannet.State.Context
--import Gannet.Emitters.Puffin
--import Gannet.Emitters.Petrel
--import Gannet.Emitters.Skua
--import Gannet.Emitters.Cormorant
import Gannet.SBA.Constants
import Gannet.SBA.SystemConfiguration

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import System.IO.Unsafe


{-
To get at ymlfile I should do the command line parsing separately and return
-}
(inFileName,outFileName,ymlFileName)= unsafePerformIO $ getFilenames

getFilenames :: IO (String,String,String)
getFilenames =do
    args <- getArgs
    (opts,inp) <- compilerOpts args
    let
        infile:_=inp                
        outfile
            | not $ null (filter isOutput opts) = 
                let 
                    Output outf = head (filter isOutput opts)
                in 
                    outf
            | otherwise = infile++"c"++(if c_WORDSZ==32 then "" else show c_WORDSZ) -- td -> tdc; tdc64 for 64-bit
        ymlfile
            | not $ null (filter isYaml opts) = 
                let 
                    Yaml ymlf = head (filter isYaml opts)
                in 
                    ymlf
            | otherwise ="SBA.yml" 
    return (infile,outfile,ymlfile)         


main :: IO ()
main =do
    args <- getArgs
    (opts,inp) <- compilerOpts args
    if  (elem Help opts)||(inp==[])   -- (opts !! 0) ==  
        then
            do showHelp
        else     do                
            let
                infile:_=inp                
                outfile
                    | not $ null (filter isOutput opts) = 
                        let 
                            Output outf = head (filter isOutput opts)
                        in 
                            outf
                    | otherwise = infile++"c"++(if c_WORDSZ==32 then "" else show c_WORDSZ) -- td -> tdc; tdc64 for 64-bit
                ymlfile
                    | not $ null (filter isYaml opts) = 
                        let 
                            Yaml ymlf = head (filter isYaml opts)
                        in 
                            ymlf
                    | otherwise ="SBA.yml"                    
                datafile= ((init . init . init) outfile)++"data" -- s/tdc/data/
            if elem Verbose opts 
                then putStrLn $ "\nCompiling "++infile++" with YAML file "++ ymlfile ++ " ...\n\n"
                else return () 
            input <- readFile infile -- slurp file into string. Ugly but easy
-- ----------------------------------------------------------------------------            
--
-- Tokenize
--
-- ----------------------------------------------------------------------------            
            let
                tokens = tokenize input                
            if elem Verbose opts 
                then putStrLn  $ "; Tokenized code\n" ++ show tokens ++ "\n"
                else return () 
            let
                numeric
                    | elem PPrint opts = False
                    | elem PPrintNum opts = True
{- ifdef OLD_BACKENDS
                    | elem Puffin opts = False
                    | elem Petrel opts = False
                    | elem Skua opts = False
--                    | elem Cormorant opts = True
 endif 
-}
                    | otherwise = True
-- ----------------------------------------------------------------------------            
--
-- Symbolize
--
-- ----------------------------------------------------------------------------            
                (symboltree,ctxt) = symbolize tokens
--                    | elem Cormorant opts = let (nst,nctxt) = symbolize tokens in inferTypes nctxt nst
--                    | otherwise = symbolize tokens                            
            if elem Verbose opts 
                then 
                    do 
                        putStrLn "; Pretty-print Service Instances\n" 
                        putStrLn $ show newservices
                        putStrLn "\n; Pretty-print Aliases\n"
                        putStrLn $ show newaliases
                        putStrLn "\n; Pretty-print Services\n"
                        putStrLn $ show newinterfaces
                        putStrLn "\n; Pretty-print AST\n" 
                        putStrLn $ show symboltree
                else return ()
{-
            if elem Puffin opts
                then
                    do
                        putStrLn $ emitPuffinCode symboltree
                else return ()
            if elem Petrel opts
                then
                    do                        
                        putStrLn $ emitPetrelCode symboltree
                else return ()    
            if elem Skua opts
                then
                    do                        
                        putStrLn $ emitSkuaCode symboltree
                else return ()                    
--            if elem Cormorant opts
--                then
--                    do                        
--                        putStrLn $ emitCCode $ packetize symboltree numeric
--                else return ()
-}
            if ((elem PPrint opts) || (elem PPrintNum opts))
                then
                    do
                    let
                        packets = packetize symboltree ctxt numeric
                    putStrLn $ "\n; Gannet packet list\n"
                    BSC8.putStrLn (gplToWords packets False)
                else return ()
-- ----------------------------------------------------------------------------            
--
-- Packetize
--
-- ----------------------------------------------------------------------------            
            if numeric
                then
                    do
                    let
                        packets = packetize symboltree ctxt numeric
                        padding = take (c_WORDSZ - ((length ymlfile) `mod` c_WORDSZ)) (repeat '\0')                   
                        path_padding :: String
                        path_padding = (ymlfile ++ padding)
                    BSC8.writeFile outfile (BSC8.append (gplToWords packets numeric) (BSC8.pack path_padding) )
                else return()
                    
                
isOutput :: Flag->Bool
isOutput x = 
    case x of 
        (Output _) -> True
        otherwise -> False                

isYaml :: Flag->Bool
isYaml x = 
    case x of 
        (Yaml _) -> True
        otherwise -> False   
    
data Flag 
 = Verbose  | Version | Help | PPrint | PPrintNum | Warnings | Puffin | Petrel | Skua | Cormorant
 | Input String | Output String | Yaml String
   deriving (Show,Eq,Ord)

options :: [OptDescr Flag]
options =
 [ Option ['v']     ["verbose"]         (NoArg Verbose)        "be verbose"
 , Option ['V']         ["version"]         (NoArg Version)        "show version number"
 , Option ['h','?'] ["help"]            (NoArg Help)            "show some help"
 , Option ['p']     ["print"]            (NoArg PPrint)        "pretty-print the compiled task"
 , Option ['s']     ["show"]            (NoArg PPrintNum)        "show the compiled task (like -p but numeric)"
 , Option ['w']     ["warnings"]        (NoArg Warnings)        "show extra warnings"
 , Option ['o']     ["outfile"]        (ReqArg Output "FILE")        "output FILE"
 , Option ['Y']                ["yaml"]                (ReqArg Yaml "FILE")                "YAML file"
 , Option ['5']     ["petrel"]        (NoArg Petrel)        "emit Perl5 code for Petrel"
 , Option ['6']     ["puffin"]        (NoArg Puffin)        "emit Perl6 code for Puffin"
 , Option ['S']     ["skua"]        (NoArg Skua)        "emit Scheme code for Skua" 
 , Option ['C']     ["cormorant"]        (NoArg Cormorant)        "emit C code"  
-- , Option ['H']     ["Haskell"]        (NoArg H)        "emit Haskell code for H"
 ]
 
outp :: Maybe String -> Flag
outp = Output . fromMaybe "stdout"

ymlSBA :: Maybe String -> Flag
ymlSBA = Yaml . fromMaybe "SBA.yml"

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts args = 
   case getOpt Permute options args of
      (opts,inp,[]) -> return (opts,inp)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: gannet [-hpsvwV56SC] task description file (.td)"    
  
showHelp = do
    putStrLn $ "Gannet bytecode compiler, "++(show c_WORDSZ)++"-bit version"
    putStrLn "Usage: gannet [-hpsvwV56SC] task description file (.td)"
    putStrLn "    -h,-? : this message"
    putStrLn "    -p : pretty-print the compiled task and exit"        
    putStrLn "    -s : show the compiled task and exit (like -p but numeric)"     
    putStrLn "    -o outfile : optional output file (.tdc)"
    putStrLn "    -Y ymlfile : optional YAML input file (.yml)"
    putStrLn "    -v : verbose (NOT IMPLEMENTED)"
    putStrLn "    -w : warnings (NOT IMPLEMENTED)"
    putStrLn "    -V : version (NOT IMPLEMENTED)"
    putStrLn "    -5 : emit Perl5 code for Petrel"
    putStrLn "    -6 : emit Perl6 code for Puffin"
    putStrLn "    -S : emit Scheme code for Skua"
    putStrLn "    -C : emit C code"    
