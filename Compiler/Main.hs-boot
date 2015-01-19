module Main (
	ymlFileName
) where 
import System.Environment

import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import System.IO.Unsafe

{-
Gannet main:
    -read td file
    -tokenize
    -symbolize
    -packetize
    -numerify    
    -bytecodize
    -write tdc file

Command-line arguments:
    -h: help
    -S: show the compiled code
    -v: be verbose
    -V: version
    -w: warnings
    -P5: Perl5
    -P6: Perl6
    -S: Scheme
    -H: Haskell
There might be others later, e.g. for "virtual" services        
-}

{-
To get at ymlfile I should do the command line parsing separately and return
-}
--(inFileName,outFileName,ymlFileName)= unsafePerformIO $ getFilenames
--(
inFileName :: String
outFileName :: String
ymlFileName :: String

getFilenames :: IO (String,String,String) 
{-
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
            | otherwise = infile++"c" -- td -> tdc
        ymlfile
            | not $ null (filter isYaml opts) = 
                let 
                    Yaml ymlf = head (filter isYaml opts)
                in 
                    ymlf
            | otherwise ="SBA.yml" 
    return (infile,outfile,ymlfile)         
-}

--main :: IO ()
{-
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
                    | otherwise = infile++"c" -- td -> tdc
                ymlfile
                    | not $ null (filter isYaml opts) = 
                        let 
                            Yaml ymlf = head (filter isYaml opts)
                        in 
                            ymlf
                    | otherwise ="SBA.yml"                    
                datafile= ((init . init . init) outfile)++"data" -- s/tdc/data/
            if elem Verbose opts 
                then putStrLn $ "\nCompiling "++infile++" ...\n\n"
                else return () 
            input <- readFile infile -- slurp file into string. Ugly but easy
            let
                tokens = tokenize input                
            if elem Verbose opts 
                then putStrLn  $ "; Tokenized code\n" ++ show tokens ++ "\n"
                else return () 
            let
                numeric
                    | elem PPrint opts = False
                    | elem PPrintNum opts = True
                    | elem Puffin opts = False
                    | elem Petrel opts = False
                    | elem Skua opts = False
--                    | elem Cormorant opts = True 
                    | otherwise = True
                (symboltree,ctxt) 
                    | elem Cormorant opts = let (nst,nctxt) = symbolize tokens in inferTypes nctxt nst
                    | otherwise = symbolize tokens                            
            if elem Verbose opts 
                then 
                    do 
                        putStrLn "; Pretty-print Services\n" 
                        --putStrLn $ show services
                        putStrLn "\n; Pretty-print Aliases\n"
                        --putStrLn $ show aliases
                        putStrLn "\n; Pretty-print AST\n" 
                        putStrLn $ show symboltree
                else return ()
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
            if ((elem PPrint opts) || (elem PPrintNum opts))
                then
                    do
                    let
                        packets = packetize symboltree ctxt numeric
                    putStrLn $ "\n; Gannet packet list\n" ++ (gplToWords packets False)
                else return ()
            if numeric
                then
                    do
                    let
                        packets = packetize symboltree ctxt numeric                    
                    writeFile outfile (gplToWords packets numeric)
                else return()
                    
-- We don't use DATA at the moment
--                    case numeric of 
--                        False -> putStrLn $ "\n; Data file content\n" ++ (writeData ctxt)
--                        True -> writeFile datafile (writeData ctxt)
-}                
isOutput :: Flag->Bool
{-
    isOutput x = 
    case x of 
        (Output _) -> True
        otherwise -> False                
-}
isYaml :: Flag->Bool
{-
isYaml x = 
    case x of 
        (Yaml _) -> True
        otherwise -> False   
-}    
data Flag 
 = Verbose  | Version | Help | PPrint | PPrintNum | Warnings | Puffin | Petrel | Skua | Cormorant
 | Input String | Output String | Yaml String
--   deriving (Show,Eq,Ord)

options :: [OptDescr Flag]
{-
options =
 [ Option ['v']     ["verbose"]         (NoArg Verbose)        "be verbose"
 , Option ['V']         ["version"]         (NoArg Version)        "show version number"
 , Option ['h','?'] ["help"]            (NoArg Help)            "show some help"
 , Option ['p']     ["print"]            (NoArg PPrint)        "pretty-print the compiled task"
 , Option ['s']     ["show"]            (NoArg PPrintNum)        "show the compiled task (like -p but numeric)"
 , Option ['w']     ["warnings"]        (NoArg Warnings)        "show extra warnings"
 , Option ['o']     ["outfile"]        (OptArg outp "FILE")        "output FILE"
 , Option ['y']		["yaml"]		(OptArg ymlSBA "FILE")		"YAML file"
 , Option ['5']     ["petrel"]        (NoArg Petrel)        "emit Perl5 code for Petrel"
 , Option ['6']     ["puffin"]        (NoArg Puffin)        "emit Perl6 code for Puffin"
 , Option ['S']     ["skua"]        (NoArg Skua)        "emit Scheme code for Skua" 
 , Option ['C']     ["cormorant"]        (NoArg Cormorant)        "emit C code"  
-- , Option ['H']     ["Haskell"]        (NoArg H)        "emit Haskell code for H"
 ]
-} 
outp :: Maybe String -> Flag
--outp = Output . fromMaybe "stdout"

ymlSBA :: Maybe String -> Flag
--ymlSBA = Yaml . fromMaybe "SBA.yml"

compilerOpts :: [String] -> IO ([Flag], [String])
{-
    compilerOpts args = 
   case getOpt Permute options args of
      (opts,inp,[]) -> return (opts,inp)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: gannet [-hpsvwV56SC] task description file (.td)"    
-}
--showHelp :: IO ()
{-
showHelp = do
    putStrLn "Usage: gannet [-hpsvwV56SC] task description file (.td)"
    putStrLn "    -h,-? : this message"
    putStrLn "    -p : pretty-print the compiled task and exit"        
    putStrLn "    -s : show the compiled task and exit (like -p but numeric)"     
    putStrLn "    -o outfile : optional output file (.tdc)"
    putStrLn "    -y ymlfile : optional YAML input file (.yml)"
    putStrLn "    -v : verbose (NOT IMPLEMENTED)"
    putStrLn "    -w : warnings (NOT IMPLEMENTED)"
    putStrLn "    -V : version (NOT IMPLEMENTED)"
    putStrLn "    -5 : emit Perl5 code for Petrel"
    putStrLn "    -6 : emit Perl6 code for Puffin"
    putStrLn "    -S : emit Scheme code for Skua"
    putStrLn "    -C : emit C code"   
-}    
