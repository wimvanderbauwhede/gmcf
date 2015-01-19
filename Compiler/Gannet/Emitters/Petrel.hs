-- | Emitter for Petrel, the Perl 5 backend. This is not a threaded OO back-end but a direct translation of the Gannet AST into Perl.
-- This is possible because Perl has @do { ... }@ and @sub { ... }@. The resulting code is of course purely sequential.
module Gannet.Emitters.Petrel (
emitPetrelCode
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
    ("assign",\op args -> assignstr args), --  "my " ++ (args !! 0) ++ " = " ++ (args !! 1)),
    ("read",\op args -> readstr args), -- (args !! 0)),
    ("update",\op args ->  updatestr args), -- (args !! 0) ++  " = " ++ (args !! 1) ),
    ("if",\op args -> "do {"++op++"( " ++ (head args) ++ "){\n" ++ (args !! 1) ++ "\n} else {\n" ++ (args !! 2) ++ "\n}}"),
    ("return",\op args -> returnstr args),
    ("lambda",\op args -> lambdastr args),
    ("apply",\op args -> ['&']++(head args)++"("++(join "," (tail args))++")"),
    ("applytc",\op args -> ['&']++(head args)++"("++(join "," (tail args))++")"),
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
So how do we handle buffer/stream?
(buffer bufno arg) => &buffer(bufno,arg)?

What we must do is define an array of buffers:
my @buffers=();
then we do: do {push @buffers->[$bufno][0], &$arg() ;$buffers->[$bufno][1]=$arg}

(stream bufno)  => do {
push @buffers->[$bufno][0], &{$buffers->[$bufno][1]}();
shift @buffers->[$bufno]
}
-}


{-
WV: we could incorporate these in the table
-}    
returnstr args 
    | length args > 1 = "do {" ++ (join ";\n" (tail args)) ++ ";\n" ++ (head args) ++ "}"
    | otherwise = (head args) -- "do {" ++ (head args) ++ "}"
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


{- then we define emit: (well, actually we define how emit changes)
emitC :: SymbolTree -> CodeTable -> String
emitC (Symbol s) ct = (show (unaliasService (name s)))
emitC (SymbolList (contents,slh)) ct =
    let
        (Symbol opsym):args = contents
        opname = (show (unaliasService (name opsym)))
        emit = case Hash.lookup opname ct of
            Just emit_custom -> emit_custom
            Nothing -> case Hash.lookup "default" ct of
                Just emit_default -> emit_default
                Nothing -> error "No default emitter provided"
    in      
        emit (map \a->(emitC a ct) args)
-}
{-                            
and we make sure that the args in the function is actually map emit args, some recursion
This must be recursive, so we need map emit args; but the emitters in the table are not-recursive
What it is: emit_custom expects a list of strings. We start with a SymbolTree. 
So we need to do something like SymbolTree -> ([String] -> String) -> String
What if we do simply (map \a->(emit a ct) args). SHould be OK: if a is a symbol, recursion stops.

Now we need to keep track of labels, so we need the state monad as previously

-}
---------------------------------------------------------------------------------------------------

headerStr = "use strict; no strict 'vars';\n" ++
            "use warnings;\n\n"


openGWStr="print(\n"
closeGWStr=",\"\\n\"\n);\n"

methodCallStr = "->{" 

instantiateObjStr str objstr = "my "++['$']++"S_"++str++"="++objstr++"();\n"

--noServiceStr str = "# No service for " ++ str ++ "\n"
noServiceStr str = 
    let 
        objstr = (toUpper $ str !! 0):((toUpper $ str !! 1): (tail (tail str) ))
    in 
        instantiateObjStr str objstr


openQuoteExprStr =  "sub {"
closeQuoteExprStr = "}"
openExprStr = "sub { &{"
closeExprStr="}"

openLambdaExprStr="sub { sub { &{"
closeLambdaExprStr n="}}"
openLambdaArgsExprStr="my (";
closeLambdaArgsExprStr=")=@_;"
openLambdaBodyExprStr=openExprStr++['$']++"S_"
closeLambdaBodyExprStr=" "

openQuoteVarStr =  "'"
closeQuoteVarStr =   "'"

noQuote = emptyStr

openParenStr = "}}("
closeParenStr = ")"
commaStr = ","
newlinetabStr = "\n\t"
newlineStr = "\n"
-- <maybe a comma> <maybe an opening quote> <the arg string> <maybe a closing quote>
-- sub { $S_inst->meth(...)}
argStr mc mqo str mqc =mc ++mqo++openExprStr++['$']++"S_" ++ str ++ closeParenStr ++closeExprStr++mqc++"\n"
readLetVarStr varnamestr = openExprStr++['$']++"S_let"++methodCallStr++"read}}("++openQuoteVarStr++ varnamestr ++ closeQuoteVarStr++")"++closeQuoteExprStr
substLambdaVarStr varnamestr = openExprStr++['$']++"S_apply"++methodCallStr++"subst}}("++openQuoteVarStr ++ varnamestr ++ closeQuoteVarStr++")"++closeExprStr
declLambdaVarStr varnamestr= ['$']++varnamestr

--substLambdaVarStr varnamestr = "&{"++['$']++"Function::subst}("++openQuoteVarStr ++ varnamestr ++ closeQuoteVarStr++")"

tagVarDeclStr tagvarname = " "++['$']++ tagvarname ++ " = "
tagVarInstStr tagvarname = " "++['$']++ tagvarname

codestrings :: CodeStrings
codestrings=(CodeStrings 
    headerStr 
    openGWStr
    closeGWStr
    methodCallStr
    instantiateObjStr
    noServiceStr
    openExprStr
    closeExprStr
    openQuoteExprStr
    closeQuoteExprStr
    openQuoteVarStr
    closeQuoteVarStr
    argStr
    readLetVarStr
    declLambdaVarStr
    substLambdaVarStr
    tagVarDeclStr
    ""
    tagVarInstStr
    openParenStr
    closeParenStr
    openLambdaExprStr
    closeLambdaExprStr
    openLambdaArgsExprStr
    closeLambdaArgsExprStr
    openLambdaBodyExprStr
    closeLambdaBodyExprStr
    commaStr
    ) --

emitPetrelCode :: SymbolTree -> String
emitPetrelCode st = emitRuntimeCode st codestrings code_table
---------------------------------------------------------------------------------------------------
{-
This is the common emitter code for Puffin (P6) and Petrel (P5).
It should work for similar languages as well.
-}

{-
Supporting LABEL is a little bit hairier than I thought.
The SymbolTree needs to remember that it was labeled.
Then, if we find a reference, this indicates a call to a
labeled sub. But how can we know which sub? From the label in the
SymbolListHeader.
So what we need as a 'labeled' bit in the SLH
If true, the current sub is labeled with a variable created from the name and the sub
label_varname=(show (name (label slh))) ++ "_" ++ (show (subtask (label slh)))
-}
{-
What we really need to do is create custom emitters per service.
We create a list of those and convert it to a map, and then look up
the emitter based on the name of the service

emitLambdaCode                                        
-}

-- The reference implementation does not follow exactly the same structure as the Gannet ServiceCoreLibrary:
-- e.g. lambda and apply are provided by the Function object, but in the VM they are separate services
-- What we list here is the actual service name (no aliases), and the class that provides it.
-- TODO: List services should not be in Block but on their own. So somehow the lists below must get precedence over aliases etc

(runtime_obj_list, runtime_alias_list)=readRuntimeConfig "RuntimeConfig.yml"
{-
runtime_obj_list = [ ("lambda","Function")
                                ,("if","Cond")
                                ,("alu","ALU")
                                ,("apply","Function")
                                ,("begin","Block")
                                ,("io","IO")
                                ,("let","Block")
                                ,("count","NONE")
                                ,("call","NONE")
                                ,("stream","Buffer")
                                ]
-}
runtime_obj_table= Hash.fromList runtime_obj_list

-- The services in this list are not in SystemConfiguration
-- The purpose of the reference implementation is exaclty to investigate new services

{-
runtime_alias_list = [ ("buffer","stream")
                                        ,("fifo","stream")
                                        ,("peek","stream")
                                        ,("loop","stream")
                                        ,("iter","stream")
                                        ,("get","stream")
                                        ,("rnd","alu")
                                        ,("display","io")
                                        ,("ioread","io")
                                        ,("iowrite","io")
                                        ,("fopen","io")
                                        ]
-}
runtime_aliases = Hash.fromList runtime_alias_list

startGS=emptyGS{kind=K_X}
startST = Symbol startGS

type  ServiceTable = Hash.Map String String
emptySH :: ServiceTable
emptySH = Hash.empty

emptyStr=""
newline="\n"
tab="\t"

data CodeStrings = CodeStrings {
header::String,
openGW::String,
closeGW::String,
methodCall::String, 
instantiateObj::String->String->String,
noService::String->String,
openExpr::String,
closeExpr::String,
openQuoteExpr::String,
closeQuoteExpr::String,
openQuoteVar::String,
closeQuoteVar::String,
arg::String->String->String->String->String,
readLetVar::String->String,
declLambdaVar::String->String,
substLambdaVar::String->String,
tagVarDeclOpen::String->String,
tagVarDeclClose::String,
tagVarInst::String->String,
openParen::String,
closeParen::String,
openLambdaExpr::String,
closeLambdaExpr::Int->String,
openLambdaArgsExpr::String,
closeLambdaArgsExpr::String,
openLambdaBodyExpr::String,
closeLambdaBodyExpr::String,
comma::String
}
-- TODO: merge this back into Emitters.Common
emitRuntimeCode :: SymbolTree -> CodeStrings -> CodeTable -> String
emitRuntimeCode st cs ct = str
        where
                (fstr,sht) = (unwrapSH (emitCM (quoteSymbolTree st emptyST) startST ct) emptySH)
                istr = "my @buffers=();\n" --serviceInstCode sht cs
                str=(header cs) ++  istr ++ (openGW cs) ++ fstr ++ (closeGW cs)                
-- ----------------------------------------------------------------------------
serviceInstCode :: ServiceTable -> CodeStrings -> String
serviceInstCode sht cs = " " ++ (unwords ( map (\str->(buildServiceInstCode str cs)) (Hash.keys sht))) ++ "\n"

buildServiceInstCode :: String -> CodeStrings -> String
buildServiceInstCode str cs =
        let
                objstr = case Hash.lookup str runtime_obj_table of
                        Just obj -> obj
                        Nothing -> "NOSERVICE"
        in
                case objstr of
                        "NOSERVICE" -> (noService cs) str
                        _ -> (instantiateObj cs) str objstr

-- ----------------------------------------------------------------------------                
{-
If I understand my own code correctly, emitPCM takes a SymbolTree (a Symbol or a SymbolList) and the previous
SymbolTree. So we can detect e.g. the last ASSIGN in a LET block:
(let (assign 'v1 (...)) ... (assign 'vn (...)) (S ...) ...)
"If the first symbol of the current list is not a K_S ASSIGN and the first symbol of the previous list is a K_S ASSIGN."
-}
emitCM :: SymbolTree -> SymbolTree ->  CodeTable -> State ServiceTable String
--emitCM (Symbol s) _ ct = do return (show (unaliasService (name s)))
emitCM (Symbol s) _ ct 
    | kind s == K_A =  do return $ ['$']++(show (name s))
    | kind s == K_R = do return $ "goto " ++ (show (aluName (name s)) ++ "_" ++ (show (subtask s)))
    | kind s ==K_L = do return $ ['$']++ show (name s)
    | otherwise = do return (show (name s))
emitCM (SymbolList (contents,slh)) prev ct =
    let
        hlabel= label slh
        Symbol tagname=head contents
        tag 
            | tagged slh = (show (aluName (name tagname))) ++ "_" ++ (show (subtask hlabel)) ++ ": "
            | otherwise = emptyStr    
        (Symbol opsym):args = noQuotes contents
--        opname = (show (unaliasService (name opsym)))
        opname = (show (name opsym))
        emit :: String -> [String] -> String
        emit = case Hash.lookup opname ct of
            Just emit_custom -> emit_custom
            Nothing -> case Hash.lookup "default" ct of
                Just emit_default -> emit_default
                Nothing -> error "No default emitter provided"
    in      
        do
            let
                argstrs = mapM (\a->(emitCM a prev ct)) args
            str <- argstrs >>= return . (\args -> emit opname args)
            return $ tag ++ str

emitPCM :: SymbolTree -> SymbolTree -> CodeStrings -> State ServiceTable String
emitPCM (Symbol s) (Symbol p) cs = do 
        str <- emitRuntimeSymbolM s cs
        return $ (maybeComma s p cs) ++ str
emitPCM (Symbol s) _ cs = do
        str <- emitRuntimeSymbolM s cs
        return $ (maybeComma s emptyGS cs) ++ str
emitPCM (SymbolList (contents,slh)) mq cs =                         
        let 
                p= case mq of
                        (Symbol mp) -> mp
                        _ -> emptyGS{kind=K_R}
                x=emptyGS{kind=K_R}        
                hlabel= label slh
                Symbol tagname=head contents
--                tag 
--                        | tagged slh = tagVarDecl cs
--                        | otherwise = emptyStr
                tag 
                        | tagged slh = (tagVarDeclOpen cs) $ (show (aluName (name tagname))) ++ "_" ++ (show (subtask hlabel))
                        | otherwise = emptyStr
                q=quoted hlabel
                (mqo,mqc) 
                        | q==1 && (kind hlabel/=K_B) = (tag ++ (openQuoteExpr cs),closeQuoteExpr cs) 
                        | otherwise = (tag ,emptyStr)
--                        | otherwise = (emptyStr,emptyStr)
                (Symbol ssym):_ = contents 
                sname = (show (unaliasService (name ssym)))
                maybeCloseLExprStr
                        | sname == "lambda" = (closeLambdaExpr cs) (quot ((length contents)-3) 2)
                        | otherwise = ""
                maybeCloseLabelStr 
                        | tagged slh = (tagVarDeclClose cs)
                        | otherwise = ""
                maybeOpenLBExprStr
                        | (kind p)==K_A && (quoted p)==1 = (closeLambdaArgsExpr cs) ++ (openLambdaBodyExpr cs)
                        | otherwise = ""
        in
                do 
                        str <- unwordsPSListM (noQuotes contents) cs
                        return $ (arg cs) (maybeComma x p cs) mqo (maybeOpenLBExprStr ++ str ++ maybeCloseLExprStr ) (mqc++maybeCloseLabelStr)

--         
emitRuntimeSymbolM :: GannetSymbol -> CodeStrings -> State ServiceTable String
emitRuntimeSymbolM s cs = do
        let
                (mqo,mqc) 
                        | (quoted s)==1 = (openQuoteVar cs,closeQuoteVar cs)
                        | otherwise = (emptyStr,emptyStr)
        emitRuntimeSymbolQM s (mqo,mqc)        cs
        
{- 
Here is where we actually find the object instances.
-}
emitRuntimeSymbolQM :: GannetSymbol -> (String,String) -> CodeStrings -> State ServiceTable String
--and we use "do" to "make it happen"
emitRuntimeSymbolQM s (mqo,mqc) cs
        | (kind s)==K_L  && mqo/=(openQuoteVar cs) = do return $ (readLetVar cs) $ show (name s)
        | (kind s)==K_A  && mqo/=(openQuoteVar cs) = do return $ (substLambdaVar cs) $ show (name s) 
        | (kind s)==K_A  && mqo==(openQuoteVar cs) = do return $ (declLambdaVar cs)  $ show (name s)
        | (kind s)==K_S = do emitRuntimeSSymbolQM s cs
        | (kind s)==K_B = do return $ show (name s) 
        | (kind s)==K_R = do return $ (tagVarInst cs) (show (aluName (name s)) ++ "_" ++ (show (subtask s))) -- temporary REF detection
        | otherwise = do return $ mqo ++ show (name s) ++ mqc

-- | Custom emitter for K_S symbols        
emitRuntimeSSymbolQM :: GannetSymbol -> CodeStrings -> State ServiceTable String
emitRuntimeSSymbolQM s cs = do
        let sname=(show (unaliasService (name s))) 
        let maybeOpenLExprStr 
                | sname=="lambda" = (openLambdaExpr cs)++(openLambdaArgsExpr cs)
                | otherwise = ""
        appendSH sname
        return $ sname ++ (methodCall cs) ++ (show (aluName (name s))) ++ (openParen cs) ++ maybeOpenLExprStr ++ (maybeNL s)        

--unwordsPSListM :: [SymbolTree] -> State ServiceTable String
--unwordsPSListM st = do
--        tmpl <- map (\(x,prev)->emitPCM x prev) (zip st ((Symbol emptyGS):st)) 
--        return $ unwords tmpl         

unwordsPSListM :: [SymbolTree] -> CodeStrings -> State ServiceTable String
unwordsPSListM st cs = mapPCM st (Symbol emptyGS) emptyStr cs
        
mapPCM :: [SymbolTree] -> SymbolTree -> String -> CodeStrings -> State ServiceTable String        
mapPCM st prev str cs
        | length st == 0 =
                do return str
        | otherwise =
                do
                        let x:xs=st
                        rstr <- emitPCM x prev cs
                        mapPCM xs x (str++rstr) cs
                                                
unwrapSH :: State ServiceTable String -> ServiceTable -> (String,ServiceTable)
unwrapSH lst st0 =
    let
        (State lf)=lst
        (str,st) = lf st0
    in 
        (str,st)
        
appendSH :: String -> State ServiceTable String
appendSH str = State (\st->(str,(updateSH st str)))        

updateSH :: ServiceTable -> String -> ServiceTable
updateSH st k =
        case Hash.lookup k st of
                Just k -> st
                Nothing -> Hash.insert k k st

------------------------------------------------------------------------------

-- | remove Quote symbols
noQuotes :: [SymbolTree] -> [SymbolTree]
noQuotes stl = 
        filter isNotQuote stl
        where
                isNotQuote (SymbolList sl) = True
                isNotQuote (Symbol s)        
                        | (kind s)==K_Q = False
                        | otherwise = True
-- ----------------------------------------------------------------------------        
maybeComma :: GannetSymbol -> GannetSymbol ->  CodeStrings -> String                
maybeComma s p cs
        | (kind p)==K_X = emptyStr
        | (kind s)==K_S = emptyStr
        | (kind p)==K_S = emptyStr
        | (kind s)==K_R && (p==emptyGS) = emptyStr
        | otherwise = comma cs
-- ----------------------------------------------------------------------------
maybeNL :: GannetSymbol -> String
maybeNL s
        | show (name s)=="let" = newline ++ tab 
        | otherwise = emptyStr

-- ----------------------------------------------------------------------------
-- | This function sets the Quoted field of a symbol or symbollist that was preceded by a Quote Symbol
-- We filter the Quote symbols out in the next step (emitPCM) with noQuotes
quoteSymbolTree        :: SymbolTree -> SymbolTree -> SymbolTree
quoteSymbolTree (Symbol s) mqs =
        let
                q = case mqs of
                        (Symbol mq) -> case (kind mq) of
                                K_Q -> 1
                                _ -> 0
                        _ -> 0
        in
                Symbol s{quoted=q}

quoteSymbolTree (SymbolList (sl,slh)) mqs =
        let
                q = case mqs of
                        (Symbol mq) -> case (kind mq) of
                                K_Q -> 1
                                _ -> 0
                        _ -> 0
                hlabel=label slh
                hlabel_mq=hlabel{quoted=q}
                slh_mq=slh{label=hlabel_mq}
        in
                SymbolList (map (\(sl_elt,prev_elt)->quoteSymbolTree sl_elt prev_elt) (zip sl ((Symbol emptyGS):sl)),slh_mq)
-- ----------------------------------------------------------------------------
-- | Helper for unaliasing of Services      
unaliasService :: GannetToken -> GannetToken
unaliasService (GannetTokenS gl) =
                let
                        GannetLabelS aname = gl
                        sname = case Hash.lookup aname services of
                                Just _ -> aname
                                Nothing -> case Hash.lookup aname alunames of
                                        Just alias -> case Hash.lookup alias aliases of
                                                Just (aaname,_,_) -> aaname
                                                Nothing -> aname
                                        Nothing -> case Hash.lookup aname aliases of
                                                Just (asname,_,_) -> asname
                                                Nothing -> aname
                        psname = case Hash.lookup sname runtime_aliases of
                                                Just paname -> paname
                                                Nothing -> sname
                in
                        (GannetTokenS (GannetLabelS psname))        
unaliasService (GannetTokenQ st) = (GannetTokenQ $ "UNRESOLVED:"++st)
unaliasService (GannetTokenB gbi) = (GannetTokenQ (show gbi))
unaliasService gtl = gtl
                        
aluName :: GannetToken -> GannetToken
aluName (GannetTokenS gl) = 
                let
                        GannetLabelS aname = gl
                        sname = case Hash.lookup aname alunames of
                                        Just sname -> sname
                                        Nothing -> aname
                in
                        (GannetTokenS (GannetLabelS sname))
