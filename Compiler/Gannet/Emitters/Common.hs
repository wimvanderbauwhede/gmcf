-- | Common functions used by the emitters for back-ends like Puffin and Skua, which are based on classes representing the Gannet services  
module Gannet.Emitters.Common (
emitRuntimeCode,
CodeStrings(..),
emptyStr
) where
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration    
import Gannet.Emitters.ConfigReader

import Control.Monad.State               
import qualified Data.Map as Hash 

{-
This should work quite differently:
e.g for a fast no-object implementation with use of native constructs in any language,
this is no good. We need a translation table for every service with a default for
services not in the table.
translation_table_list=[
("let",\args -> "do { " ++ (join "; " emit args) ++ "}"),
("assign",\args -> ['$']++ (head args) ++ " = "),
("read",\args -> ['$'] ++ (head args)),
("update",\args -> let v:a=args in ['$']++ v ++  " = " ++ a),
("if",\args -> let p:t:f=args in "do {if( " ++ p ++ "){\n" ++ t ++ "\n} else {\n" ++ f ++ "\n}}"),
("lambda",\args -> let {body:rlargs=reverse args; largs=reverse rlargs} in "sub {("++ join ";" (map \x->("my "++x) largs) ++ ")=@_;" ++ body ++"}")
]
type CodeTable = Hash.Map String ([String] -> String)
code_table= Hash.fromList translation_table_list

and we define
 
join sep ls = foldl1 (\x y->x++sep+y) ls

then we define emit: (well, actually we define how emit changes)

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
                            
and we make sure that the args in the function is actually map emit args, some recursion
This must be recursive, so we need map emit args; but the emitters in the table are not-recursive
What it is: emit_custom expects a list of strings. We start with a SymbolTree. 
So we need to do something like SymbolTree -> ([String] -> String) -> String
What if we do simply (map \a->(emit a ct) args). SHould be OK: if a is a symbol, recursion stops.

Now we need to keep track of labels, so we need the state monad as previously

-}

{-
This is the common emitter code for Puffin (P6) and Skua (Scheme)
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

-- | The implementation of the various back-ends does not follow exactly the same structure as the Gannet ServiceCoreLibrary:
-- e.g. lambda and apply are provided by the Function object, but in the VM they are separate services
-- RuntimeConfig.yml contains a table of the actual service names and aliases, and the class that provides it.
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

-- | emitRuntimeCode walks the symbol tree and emits code using CodeStrings
emitRuntimeCode :: SymbolTree -> CodeStrings -> String
emitRuntimeCode st cs = str
        where
                (fstr,sht) = (unwrapSH (emitPCM (quoteSymbolTree st emptyST) startST cs) emptySH)
                istr = serviceInstCode sht cs
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

-- custom emitter for K_S symbols        
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

-- remove Quote symbols
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
-- This function sets the Quoted field of a symbol or symbollist that was preceded by a Quote Symbol
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

                        
aluName :: GannetToken -> GannetToken
aluName (GannetTokenS gl) = 
                let
                        GannetLabelS aname = gl
                        sname = case Hash.lookup aname alunames of
                                        Just sname -> sname
                                        Nothing -> aname
                in
                        (GannetTokenS (GannetLabelS sname))
