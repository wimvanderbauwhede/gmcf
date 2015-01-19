{-# LANGUAGE CPP #-}
-- |The symbolizer transforms a TokenTree into a SymbolTree. 
-- This is the core of the compiler. The SymbolTree is the Abstract Syntax Tree
-- Gannet symbol Kinds are identified based on the token type, position in the expression or from the context.
-- Token types are Builtin, Quote, Service or Label. A service is the first elt in the S-expression list
-- A label is any element in the S-expression that is not a Builtin, Quote or Service

-- WV20110609 for old config handling, revert to before r4987 and undef NEW

module Gannet.Symbolizer (
    symbolize
) where
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration
--import Gannet.Symbolizer.InferTypes
import Gannet.State.SymbolTree
import Gannet.State.Context
import Gannet.State.Scope
import Gannet.Numerifier
import Gannet.Symbolizer.Internals
import Gannet.Warning (warning)

import Control.Monad.State
import System.IO
import qualified Data.Map as Hash
import Text.ParserCombinators.Parsec hiding (State)

{-
How to do 'taint checking'. i.e. detect which blocks should be stored at APPLY?
T1-if a block contains K_A's
T2-or it contains blocks containing K_A's
T3-or it contains K_L's binding blocks containing K_A's

(lambda 'x 'y (let 
                (assign 'v (+ x y :T1) :T2)
                (+ v 1 :T3)
                :T2)
)

the T3 rule requires the v to be tagged as tainted. As the whole taint check 
is only used inside the compiler, it would be better to add a 'taint' field 
to the GannetSymbol and to the SymbolListHeader. Call it 'lambda' :-)
So if we have an assign of which the value is tainted, 
the variable should be tainted as well 
-}

{-
Apart from that, with the view of supporting closures, K_L lexicals 
also come in 2 groups:
-Those that are called inside a nested lambda
(lambda 'x '(let (assign 'v 4) (lambda 'y '(+ v x y))))
-those that bind an expression containing lambda args

-If we encounter a v, and it's in a lambda (check the let_or_lambda context)
-if that lambda is the last expression in the let
OR it is bound to an f and that f is the last expression in the let
OR it is inside and IF

Clearly we need a rigorous type checker!
-}

-- | The Bool argument indicates if the symbols are numerified or not.
-- The returned Context contains the DataStore.  
-- We use the State monad to pass around the SymbolTree        
symbolize :: TokenTree -> (SymbolTree,Context)
symbolize tt = (st,ctxt)
    where
        (st,ctxt) = (unwrapST (t2sm (tt,emptyC)) emptySL)

-- | A monadic tree walker to convert the TokenTree into a SymbolTree
t2sm :: (TokenTree,Context) -> State SymbolTree (TokenTree,Context)
t2sm (tt,octxt)
    | ((length tl)==0) = do -- recursion is finished
        let
            ncurrent:ncallerstack= callerstack ctxt
            nletc:nouterc=
                if (cmpFQN (current ctxt) "let") || (cmpFQN (current ctxt) "lambda")
                    then (outerc ctxt)        
                    else (letc ctxt):(outerc ctxt)
            -- OBSOLETE                                                                          
            ncurrentlambda:nlambdastack
                | currentlambda ctxt == subtaskc ctxt = lambdastack ctxt
                | otherwise = (currentlambda ctxt):(lambdastack ctxt)
            nctxt
                | cmpFQN (head (callerstack ctxt)) "assign" || cmpFQN (head (callerstack ctxt)) "buf" = popAssignStack ctxt 
                | otherwise = ctxt
            nnctxt=nctxt{ letc=nletc,
                        outerc=nouterc,
                        current=ncurrent,
                        callerstack=ncallerstack,
                        currentlambda=ncurrentlambda,
                        lambdastack=nlambdastack
                        }
        return (tt,nnctxt)    
    | isTokenList x = do -- it's a list
{-
To have proper scoping for var/cache/acc/set or whatever I'm going to call them,
we need to scan the S-expression first for binding declarations
(cache 'c1 (S ...) ), (var 'v1 ...)
To distinguish the updates from the assignments, I'll use 'set' for updates (with 'acc' as an alias)

During this scan we update the context but that is all, no other changes are made.

bindRegs is the actual scanner
-}    
        let
            nsubtaskc= (subtaskc ctxt)+1
            TokenList (fx:xs)=x
--        tct <- case (stringInToken fx) of
--            "stream" -> parseStream (TokenList xs) (tt2,ctxt)
--            "get" -> parseGet (TokenList xs) (tt2,ctxt)
--            "eos" -> parseEOS (TokenList xs) (tt2,ctxt)     
            -- WV23122009: const should go here    
--            otherwise -> 
        tct <-  let                
                    (st2,nctxt) = unwrapST (t2sm (x,ctxt{subtaskc=nsubtaskc})) emptySL -- this creates a new ST based on the TT x
                in                        
                    appendST st2 (tt2,nctxt) 
        t2sm tct     
    | otherwise =         -- It's a token
        let
            (xsym2,nctxt) = (createSymCtxt x tl ctxt) -- ("createSymCtxt: "++(show x)++"\n")
        in    
            do            
                case (stringInToken x) of
                    "label" -> do
                        tct3 <- parseLabel (tt2,ctxt)
                        t2sm tct3
--                    "const" -> do -- WV15022011: obsolete?
--                        tct3 <- parseConst (tt2,ctxt)
--                        t2sm tct3                        
-- WV 20130108 buf is now implemented as part of LET, no special symbols etc anymore
--                    "buf" -> do
--                        tct3 <- parseBuf (tt2,ctxt)
--                        t2sm tct3
-- OBSOLETE:
                    "cache" -> do
                        tct3 <- parseCache (tt2,ctxt)
                        t2sm tct3                                                                                
-- OBSOLETE:
                    "var" -> do
                        tct3 <- parseVar (tt2,ctxt)
                        t2sm tct3                                                                                
                    otherwise -> do
                        tct1 <- (appendST xsym2 (tt2,nctxt)) -- ("\tappendST: "++(show xsym2)++"\n")
-- the lookup will fail for anything that is not a service, so I should check the Token type first
-- if it's not a Service token, no point in doing a lookup
                        case x of
                            Token (GannetTokenS _) -> do                    
                                case  lookupPQN (stringInToken x) True of 
                                    ("LET","let") -> do 
                                        let
                                            (tt,ctxt2)=tct1
                                            nouterc=(letc ctxt2):(outerc ctxt2)
                                            nletc=(subtaskc ctxt2)
                                            nscope=initScope nletc (letc ctxt2) (inLambda ctxt2) (scope ctxt2)
                                            tct3 = (tt,ctxt2{letc=nletc,outerc=nouterc,scope=nscope})
                                        t2sm tct3
                                    ("LET","lettc") -> do 
                                        let
                                            (tt,ctxt2)=tct1
                                            nouterc=(letc ctxt2):(outerc ctxt2)
                                            nletc=(subtaskc ctxt2)
                                            nscope=initScope nletc (letc ctxt2) (inLambda ctxt2) (scope ctxt2)
                                            tct3 = (tt,ctxt2{letc=nletc,outerc=nouterc,scope=nscope})
                                        t2sm tct3
                                    ("LET","assign") -> do
                                        tct3 <- parseAssignArg tct1
                                        t2sm tct3
                                    ("LET","buf") -> do
                                        tct3 <- parseAssignArg tct1
                                        t2sm tct3
                                    ("APPLY","lambda") -> do
                                        let
                                            (tt,ctxt2)=tct1 -- "LAMBDA\n"
                                            nouterc=(letc ctxt2):(outerc ctxt2)
                                            nletc=(subtaskc ctxt2)
                                                                                        -- WV 04/02/2013: added a scope for the lambdas. Why was it not here before?
                                                                                        -- but is letc OK? Means the lambda is treated as if it is a let ...
                                            nscope=initScope nletc (letc ctxt2) (inLambda ctxt2) (scope ctxt2)
                                            tct2 = (tt,ctxt2{letc=nletc,outerc=nouterc,scope=nscope})
                                        tct3 <- (parseLambdaArgs tct2) -- "parseLambdaArgs"
                                        t2sm tct3               
        --                            ("","data")        -> do
        --                                tct3 <- parseData tct1
        --                                t2sm tct3                                                                
                                    otherwise    -> do 
                                        t2sm tct1
                            Token _ -> do
                                t2sm tct1                                                                                  
    where
        --(TokenList tl)=tt -- it seems tt here can never be a Token
        tl=returnTokenList tt
        -- it seems tl can be [xtl] or not                
        ctxt = bindRegs octxt tl
        x:xs=tl
        tt2=(TokenList xs)

returnTokenList (TokenList tl)=tl
returnTokenList tt= [tt]

--------------------------------------------------------------------------------
-- Parsing of Language Services
--------------------------------------------------------------------------------
        
{- |
parsing Lambda arguments
-} 
parseLambdaArgs tct
    | (length xs)>2 = 
        do
            tct3 <- appendQSym tct K_A
            parseLambdaArgs tct3                
    | otherwise =
        do
            return tct
    where
        ((TokenList xs),ctxt)=tct -- `warning` "In parseLambdaArgs\n"
        ntct=((TokenList xs),ctxt)
        
-- | parsing Assign arguments
parseAssignArg tct =
    do            
        tct3 <- appendQSym tct K_L
        return tct3

-- OBSOLETE!        
-- | parse Data construct
-- values for Data are stored in the DataStore in the Context
-- labels for Data are stored on level 0 of the Scope     
parseData tct = appendQSym tct K_D

--------------------------------------------------------------------------------
-- Label handling
--------------------------------------------------------------------------------

{-
Now this is rather dense. What happens is:
-tct is transformed into tct1; then a Quote is
appended to the SymbolTree, then the actual Symbol
tct1 contains nnctxt, this is nctxt with updated 
var count, datastore and scope
nctxt is ctxt with var pushed onto the AssignStack if it's a K_L
In fact, a lot of the complexity is caused by the K_D

Now, for Label support, what we do is return 
tct1=((TokenList xs2),nnctxt) like below, but no appendST calls
Furthermore, we add an entry for the Label in some lookup table in the context
-}

{-
(label L_i (S_j ...))
What we want is 
- remove the (label ... ) call from the AST
- identify label symbols
- replace occurences of label symbols with the corresponding references.
The first step is to create the reference R_j from S_j. 
Then we put this in the context, via a lookup table labels L_i=>R_j
A label could be identified by the symbol kind or we could have a
corresponding (call ...) or (goto ...) or (do ...)
Alternatively of course we could have the label tagged onto the service:
(let:L_i ...)
Our philosophy has always been that Gannet should have almost no syntax
and certainly no sugar. So I guess (label L_i (... L_i)) is best.

So we must transform ["label", "L_i", ["S_j",...]] into
["S_j",...] and a Hash "L_i" => R_j 

How to make it work on quoted expressions?
(label L_i ' (S_j ...)) must be replaced by
 ' (S_j ...) 
-}
parseLabel tct = 
    do
        return tct1
    where
        ((TokenList ltl),ctxt)=tct                
        lbl:[tl]=ltl 
--        (lbl,tl) = case ltl of
--            plbl:[ptl] -> (plbl,ptl)
--            plbl:quote:[ptl] -> (plbl, TokenList ([quote]++[ptl])) --TokenList quoted_tl)
        Token glbl=lbl
        nctxt=ctxt{reflabel=glbl}
        tct1=(tl,nctxt)    
--        tct1=((TokenList tl),nctxt)    


parseConst tct = 
    do
        error $ show ctl -- return tct1
    where
        ((TokenList ltl),ctxt)=tct                
        lbl:quote:[ctl]=ltl 
        Token glbl=lbl
        nctxt=ctxt{reflabel=glbl}
        tct1=(ctl,nctxt)    

appendQSym tct skind = 
    do
        tct2 <- appendST qsym tct1
        tct3 <- appendST xsym tct2
        return tct3
    where
        -- create a quote symbol
        qsym=Symbol quoteGS
        -- unwrap current context
        ((TokenList xs),ctxt)=tct
        -- update running var count            
        nvarc=(varc ctxt)+1                 
        (var,xs2,nletc,nouterc,nlambda) -- ,ndatastore)        
            | skind == K_D = parseD xs ctxt -- OBSOLETE
            | otherwise = parseLA xs ctxt
        --  gtvar is redundant, I guess
        Token gtvar=var 
        -- update the context, step 1: push var on stack if it's a lexical -> not for K_A?
        nctxt 
            | skind == K_L = pushAssignStack gtvar ctxt 
            | otherwise = ctxt
        -- compile the Gannet symbol for the variable using the updated context
        -- This fails for K_A as it has not yet been appended to the scope! Need a shortcut here!!!
        xgsym = (compileSym var skind xs nctxt) -- `warning` ("Compiling "++(show var)++"::"++(show skind)++"\n")
        xsym=Symbol xgsym
        -- append the variable to the scope
        nscopet=appendScope xgsym nvarc skind nletc nouterc nlambda (scope nctxt)
        nscope = nscopet -- `warning` ("appendScope "++(show xgsym)++" "++(show nletc)++"\n")
        -- update the context, step 2: update the var count and the scope
        nnctxt=nctxt{varc=nvarc,scope=nscope} -- datastore=ndatastore,
        -- wrap tokenlist and context         
        tct1=((TokenList xs2),nnctxt)        
    
{-    
        qsym=Symbol quoteGS
        ((TokenList xs),ctxt)=tct            
        nvarc=(varc ctxt)+1         
        (var,xs2,nletc,nouterc,nlambda,ndatastore)        
            | skind == K_D = parseD xs ctxt            
            | otherwise = parseLA xs ctxt
        Token gtvar=var 
        nctxt 
            | skind == K_L = pushAssignStack gtvar ctxt
            | otherwise = ctxt
        xgsym = compileSym var skind xs nctxt
        nscope=appendScope xgsym nvarc skind nletc nouterc nlambda (scope nctxt)
        nnctxt=nctxt{varc=nvarc,datastore=ndatastore,scope=nscope}
        tct1=((TokenList xs2),nnctxt)        
        xsym=Symbol xgsym 
-}

-- | parse L or A Kind
-- This mainly extracts a list of items from the Context
parseLA :: [TokenTree] -> Context -> (TokenTree,[TokenTree],Integer,Integer,Integer) -- ,DataStore
parseLA xs ctxt =
    let
        q:var:xs2=xs    
--        ndatastore=datastore ctxt                
        nletc = letc ctxt
        nouterc:_= outerc ctxt
        nlambda=inLambda ctxt        
    in
        (var,xs2,nletc,nouterc,nlambda) -- ,ndatastore            

-- | parse D Kind -- OBSOLETE!
parseD xs ctxt =
    let            
        q:var:val:xs2=xs    
        (Token gt)=var            
        (Token (GannetTokenB gval))=val
--        ndatastore=appendData gt gval (datastore ctxt)    
        nletc =0
        nouterc=0
        nlambda=0
    in
        (var,xs2,nletc,nouterc,nlambda) --,ndatastore


--------------------------------------------------------------------------------
-- Gannet Symbol Manipulation        
--------------------------------------------------------------------------------

{-
This looks if the block bound to K_L is a LAMBDA.
If so, then what? Nothing realy, apart that the variable can be passed to APPLY
So it might be useful for error checking.
-}
bindsLambda :: [TokenTree] -> Context -> Bool
bindsLambda tl ctxt =
    let
        tt=(TokenList tl)
        (st2,_) = unwrapST (t2sm (tt,ctxt)) emptySL
    in
        case st2 of
            Symbol gs         -> (lambda gs)==1
            SymbolList (_,slh)     -> (rlambda slh)==1

--------------------------------------------------------------------------------
-- Buffer/Stream handling
--------------------------------------------------------------------------------
{-
(buf|var|cache|acc|stream V_i (S_j ...))
What we want is 
- remove the (buf|var|cache|acc ... ) call from the AST
- identify register/var symbols
- replace occurences of var symbols with the corresponding registers.
Presumably, we can defer that till numerification
At the very least bind the var symbol to a register value.
There are two cases: 
- (buf|var|cache|acc v (S ...)) result in the mode field 
of S being set to the corresponding type and the Register field set to the
(varname, regval) tuple. If the expression after the var token is not a
service expression or not present, that's a compile-time error.
- (stream v) results in a lookup of the register value and the service name
That means we must store (regval, service name) in some lookup table in the context
My idea is to have a lookup table like this:
Hash registers = {
servicename => { varname1 => regval1, varname2 => regval2 }
}
We immediately see a small problem: no scope! So ideally we should actually get unique varnames
-}


-- (stream ' b1)
parseStream tt tct = 
    do
        tct2 <- appendST vsym tct
        return tct2
    where
        (TokenList xs) = tt
        (tt2,ctxt)=tct                   
        q:var_token:xs2=xs -- so obviously xs2 is empty
        Token var=var_token         
        gvsym=compileStreamVarSym var ctxt     
        vsym = Symbol gvsym
--        tct1=(tt2,ctxt)

-- | parse Stream register var
parseStreamVar xs =
    let            
        q:var:xs2=xs    
        (Token gt)=var            
    in
        (var,xs2)


compileStreamVarSym :: GannetToken -> Context -> GannetSymbol
compileStreamVarSym var_token ctxt =
    let
        reg_info = case Hash.lookup var_token (regvartable ctxt)  of
                    Just reg_info -> reg_info
                    Nothing -> error $ "The variable "++(show var_token)++" is not bound to any service\n"
        sid = service_id reg_info
        task=taskc ctxt     
        greg = (var_token, (register reg_info)) 
        gvsym=MkGannetSymbol K_D T_x 0 0 task 0 (GannetTokenL $ GannetLabelI sid) 0 0 M_buf greg
    in        
        gvsym
        
-- (get ' b1)
parseGet tt tct = 
    do
        tct2 <- appendST vsym tct
        return tct2
    where
        (TokenList xs) = tt
        (tt2,ctxt)=tct                   
        q:var_token:xs2=xs -- so obviously xs2 is empty
        Token var=var_token         
        gvsym=compileGetVarSym var ctxt     
        vsym = Symbol gvsym
--        tct1=(tt2,ctxt)

-- | parse Get register var -- UNUSED?
--parseGetVar xs =
--    let            
--        q:var:xs2=xs    
--        (Token gt)=var            
--    in
--        (var,xs2)

-- Note : identical to Stream apart from M_var so refactor!
compileGetVarSym :: GannetToken -> Context -> GannetSymbol
compileGetVarSym var_token ctxt =
    let
        reg_info = case Hash.lookup var_token (regvartable ctxt)  of
                    Just reg_info -> reg_info
                    Nothing -> error $ "The variable "++(show var_token)++" is not bound to any service\n"
        sid = service_id reg_info
        task=taskc ctxt     
        greg = (var_token, (register reg_info)) 
        gvsym=MkGannetSymbol K_D T_x 0 0 task 0 (GannetTokenL $ GannetLabelI sid) 0 0 M_var greg
    in        
        gvsym        

-- --------------------------------

-- (eos ' b1)
parseEOS tt tct = 
    do
        tct2 <- appendST vsym tct
        return tct2
    where
        (TokenList xs) = tt
        (tt2,ctxt)=tct                   
        q:var_token:xs2=xs -- so obviously xs2 is empty
        Token var=var_token         
        gvsym=compileEOSVarSym var ctxt     
        vsym = Symbol gvsym

-- Note : identical to Get apart from M_eos so refactor!
compileEOSVarSym :: GannetToken -> Context -> GannetSymbol
compileEOSVarSym var_token ctxt =
    let
        reg_info = case Hash.lookup var_token (regvartable ctxt)  of
                    Just reg_info -> reg_info
                    Nothing -> error $ "The variable "++(show var_token)++" is not bound to any service\n"
        sid = service_id reg_info
        task=taskc ctxt     
        greg = (var_token, (register reg_info)) 
        gvsym=MkGannetSymbol K_D T_x 0 0 task 0 (GannetTokenL $ GannetLabelI sid) 0 0 M_eos greg
    in        
        gvsym   

-- ------------------------------------

parseBuf tct = 
    do
        tct2 <- appendST ssym tct1
        return tct2
    where
        ((TokenList xs),ctxt)=tct                   
        (var,xs2,nletc,nouterc,nlambda) = parseBufVar xs ctxt            
        nctxt=compileBufVarSym var M_buf xs2 ctxt     
--        xgsym=prevsym nctxt
--        nvarc=(varc ctxt)+1                 
--        nscope=appendScope xgsym nvarc K_D 0 0 0 (scope ctxt)
--        nnctxt=ctxt{varc=nvarc,scope=nscope}        
        TokenList xs2l = xs2
        sgt:xs3=xs2l
        (ssym, n3ctxt) = createSymCtxt sgt xs3 nctxt 
        tct1=((TokenList xs3),n3ctxt)        

-- | parse Buffer register var
parseBufVar xs ctxt =
    let            
        q:var:[xs2]=xs    
--        nletc =0
--        nouterc=0
--        nlambda=0
    in
        (var,xs2,0,0,0) -- nletc,nouterc,nlambda)
            
-- BufVarSymbol is K_D:*:*:*:task:Mode|Reg:name
compileBufVarSym :: TokenTree -> GMode -> TokenTree -> Context -> Context
compileBufVarSym var_token mode tt ctxt =
    let
        TokenList tl = tt
        (Token service_token):tl2=tl
        service_name = GannetTokenL (GannetLabelS (lookupFQN (getStrFromToken service_token)))
        task=taskc ctxt     
        Token var =var_token
        (reg,nctxt) = assignReg service_name var ctxt 
        gvsym=MkGannetSymbol K_D T_x 0 0 task 0 service_name 0 0 mode reg
    in        
        nctxt{prevsym=gvsym}

{-
Buffer: b is global
(buf 'b (S1 ...)) => (S1-buf<b> ...)

Local variable: v must be bound at S1
(let
    (cache 'c1 (S1 ...))
    (S0 ... c1 ...)
) => (S0-return '(S1 ... c1 ...) (cache 'c1 (S1 ...)))

With Gannet-C we can assume the first form will not occur, only the second form
will be translated into a reference symbol of a special Kind
K_C which encodes the register. 

Note that this means that for 32-bit 
symbols the number of registers is quite limited:

F_Reg=0x1C_00
FB_Reg=3
FW_Reg=0x07 
FS_Reg=10

FB_Mode=2
FW_Mode=0x03 # 0=normal,1=var/acc,2=buf/stream

 We have 8 bits for N_ARGS, so 8 remain. We use 3 for the registers, so we keep 5. Let's say we use 2 MSBs of Subtask
FS_Mode=14

K_C:(Datatype):(Ext):Quoted:Task:Mode|1Bit|Reg|xBits|CodeAddress:Name
F_CodeAddress = 0x00_00_1f_00 

 5 bits (HW needs 10?)
FN_CodeAddress = 0xff_ff_e0_ff 
FS_CodeAddress = 8 
FW_CodeAddress=31 # i.e. max. number of subtask code segments is 31!
FB_CodeAddress=5

The main complication is that we need to know S0, so actually parsing (cache 'c1 ...) should happen while
parsing (S0-return ...)

Any chance to parse (S0-return '(S0 ...) (cache 'c1 (S1 ...)))?
When we encounter (cache ...), the SymbolList being build is [(S0-return, ', (S0 ....); R0-return or whatever]
So if I could get at the current SLH, I could get the service name.
Now, parseCache only has tct, not the SymbolTree, as we've squirreled that into a monad with appendST

(st,ctxt) = (unwrapSymbolTree tct) 
-}

-- (S0-return (cache 'c1 (S1 ...)) ) => (S1 ...),C1<c1,S0> C is a cached R, cached in S0

parseCache tct = 
    do        
--        let
--            (st,ctxt) = unwrapSymbolTree (t2sm tct)                     
--            slh=getSLH(st)
--            service_name=lto slh -- GannetLabel   
--            service_token = GannetTokenL service_name         
--            ((TokenList xs),_)=tct -- xs = (' c1 (S1 ...))                  
--            (var,xs2,nletc,nouterc,nlambda) = parseCacheVar xs ctxt  -- var = c1, xs2 = (S1 ...)          
--            nctxt=compileCacheRefSym var M_buf service_token ctxt     
--            TokenList xs2l = xs2
--            sgt:xs3=xs2l
--            (ssym, nnctxt) = createSymCtxt sgt xs3 nctxt 
--            tct1=((TokenList xs3),nnctxt)  
        tct2 <- appendST ssym tct1                    
        return tct2
    where        
        (st,_) = unwrapSymbolTree (t2sm tct) -- to get at the enclosing service                    
        slh=getSLH(st)
        service_name=lto slh -- GannetLabel   
        service_token = GannetTokenL service_name         
        ((TokenList xs),ctxt)=tct -- xs = (' c1 (S1 ...))                  
        (var,xs2,nletc,nouterc,nlambda) = parseCacheVar xs ctxt  -- var = c1, xs2 = (S1 ...)          
        nctxt=compileCacheRefSym var M_buf service_token ctxt     
        TokenList xs2l = xs2
        sgt:xs3=xs2l
        (ssym, nnctxt) = createSymCtxt sgt xs3 nctxt 
        tct1=((TokenList xs3),nnctxt)  

--    where
--        ((TokenList xs),ctxt)=tct -- xs = (' c1 (S1 ...))                  
--        (var,xs2,nletc,nouterc,nlambda) = parseCacheVar xs ctxt  -- var = c1, xs2 = (S1 ...)          
--        nctxt=compileCacheRefSym var M_buf xs2 ctxt     
--        TokenList xs2l = xs2
--        sgt:xs3=xs2l
--        (ssym, nnctxt) = createSymCtxt sgt xs3 nctxt 
--        tct1=((TokenList xs3),nnctxt)  

-- | parse Buffer register var
parseCacheVar xs ctxt =
    let            
        q:var:[xs2]=xs    
    in
        (var,xs2,0,0,0) -- nletc,nouterc,nlambda)
            
-- CacheRefSymbol is K_C:*:*:*:task:Mode|Reg:name
compileCacheRefSym :: TokenTree -> GMode -> GannetToken -> Context -> Context
compileCacheRefSym var_token mode service_token ctxt =
    let
        service_name = GannetTokenL (GannetLabelS (lookupFQN (getStrFromToken service_token)))
        task=taskc ctxt     
        Token var =var_token -- b1
        (reg,nctxt) = assignReg service_name var ctxt -- assign a register to b1 in S1
        gvsym=MkGannetSymbol K_C T_x 0 0 task 0 service_name 0 0 mode reg -- create a K_D symbol to put in prevsym. Used by createSymCtxt via compileSym 
    in        
        nctxt{prevsym=gvsym}
--        error $ "lookup: "++(show service_name)++" reg:"++(show reg) -- should be IF

-- parseVar is like parseCache but it does not assign a register, instead it reads
-- the assigned register as in parseGet
-- now change parseCache to be an alias for parseVar; and find a way to 
-- deal with updating the var, i.e "set", so parseSet
parseVar tct = 
    do        
        tct2 <- appendST ssym tct1                    
        return tct2
    where        
--        (st,_) = unwrapSymbolTree (t2sm tct) -- to get at the enclosing service                    
--        slh=getSLH(st)
--        service_name=lto slh -- GannetLabel   
        ((TokenList xs),ctxt)=tct -- xs = (' c1 (S1 ...))
        service_name=head (callerstack ctxt) -- so this is a FQN
        service_token = GannetTokenS (GannetLabelS service_name) 
        sid = lookupNodeIdFromFQN service_name
        q:var_token:[xs2]=xs   
        Token var=var_token               
        reg_info = case Hash.lookup var (regvartable ctxt)  of
                    Just reg_info -> reg_info
                    Nothing -> error $ "The variable "++(show var_token)++" is not bound to any service\n"
        reg 
            | (service_id reg_info == sid) = register reg_info
            | otherwise = error $ "The variable "++(show var_token)++" is not bound to "++(show sid)++" ("++service_name++") but to "++(show (service_id reg_info))++"\n"
        greg=(var,reg)                        
        nctxt=compileVarSym var_token M_buf service_token greg ctxt             
        TokenList xs2l = xs2
        sgt:xs3=xs2l
        (ssym, nnctxt) = createSymCtxt sgt xs3 nctxt 
        tct1=((TokenList xs3),nnctxt)  

compileVarSym :: TokenTree -> GMode -> GannetToken -> GRegister -> Context -> Context
compileVarSym var_token mode service_token reg ctxt =
    let
        service_name = GannetTokenL (GannetLabelS (lookupFQN (getStrFromToken service_token)))
        task=taskc ctxt     
        Token var =var_token -- b1
        gvsym=MkGannetSymbol K_C T_x 0 0 task 0 service_name 0 0 mode reg -- create a K_D symbol to put in prevsym. Used by createSymCtxt via compileSym 
    in        
        ctxt{prevsym=gvsym}
--        error $ "lookup: "++(show service_name)++" reg:"++(show reg) -- should be IF


-- Binds var/cache variables to registers in a separate pass
bindRegs :: Context -> [TokenTree] -> Context            
--bindRegs ctxt (Token servicetoken) = ctxt
bindRegs ctxt (token:[]) = ctxt
--bindRegs ctxt (servicetoken:argtokens) =
bindRegs ctxt ((Token (GannetTokenS (GannetLabelS servicename))):argtokens) =
    let
--        servicename = stringInToken servicetoken
        nctxt = ctxt{current=servicename}
    in        
        updateContext nctxt argtokens regBinder
--        error $ "bindRegs:"++(show argtokens)
--bindRegs ctxt [] = ctxt        
bindRegs ctxt _ = ctxt        
    
--regBinder must check every argument against (var ' v e) or (cache ' v e)
regBinder :: Context -> TokenTree -> Context
regBinder ctxt (Token argt) = ctxt
regBinder ctxt (TokenList argtl) = 
    let
        x:xs=argtl
        nctxt
            | stringInToken x == "var" =
                let        
                    q:var:_=xs
                    service_token = GannetTokenS (GannetLabelS (current ctxt))
                in
--                    error $ "VAR:"++(show var)++" in "++(show service_token)
                    compileCacheRefSym var M_buf service_token ctxt                                                            
            | otherwise = ctxt
    in
        nctxt

updateContext :: Context -> [TokenTree] ->  (Context -> TokenTree -> Context) -> Context        
updateContext ctxt (i:nli) ctxt_updater = 
    let
        nctxt = ctxt_updater ctxt i
    in         
        updateContext nctxt nli ctxt_updater
--updateContext ctxt (i:[]) ctxt_updater = ctxt_updater ctxt i
updateContext ctxt [] _ = ctxt


                          
-- | register assignment for buf/cache/var
assignReg :: GannetToken -> GannetToken -> Context -> (GRegister,Context)
assignReg service_name vartoken ctxt =
    let
        register_table = registers ctxt
        cregvartable = regvartable ctxt        
        (greg,nctxt) = --if Hash.member cregvartable vartoken
                       -- then
                       --     error "The variable "++(show vartoken)++" is already in the register table for "++(show service_name)++"\n"
                       -- else
                            let
                               sid = lookupNodeIdFromFQN (getStrFromToken service_name)
                               (creg,nctxt) = case Hash.lookup sid register_table of
                                    Just creg -> 
                                        let
                                            nregister_table = Hash.insert sid (creg-1) register_table
                                        in
                                            (creg,ctxt{registers=nregister_table})
                                    Nothing -> error $ "No entry for service "++(show service_name)++" in register table\n"                               
                               reg = (MkRegInfo creg sid)    
                               greg=(vartoken, creg)
                               nregvartable=Hash.insert vartoken reg cregvartable
                            in
                                (greg,nctxt{regvartable=nregvartable})
    in
       (greg,nctxt)       

--------------------------------------------------------------------------------
-- Assign Stack manipulation
--------------------------------------------------------------------------------


-- use currentassign to store the GannetToken.
-- On entering an ASSIGN block: 
pushAssignStack :: GannetToken -> Context -> Context
pushAssignStack var ctxt =
    let
        currentassignvar=currentassign ctxt
        nassignstack
            |currentassignvar==emptyGT =assignstack ctxt -- this starts out as []. 
            |otherwise = (currentassign ctxt):(assignstack ctxt) -- this starts out as []
        ncurrentassign= var
        assignvarstack = case Hash.lookup var (assignvarstacks ctxt) of
                        Just st -> st
                        Nothing -> []
        nassignvarstacks = Hash.insert var ((letc ctxt):assignvarstack) (assignvarstacks ctxt)
    in
        ctxt{currentassign=ncurrentassign,assignstack=nassignstack,assignvarstacks=nassignvarstacks}

-- on leaving an ASSIGN block:
popAssignStack :: Context -> Context
popAssignStack ctxt =         
    let
        currentassignvar=currentassign ctxt
        ncurrentassign:nassignstack 
            | length (assignstack ctxt)>0 = assignstack ctxt
            | otherwise = (currentassign ctxt):[]
        nassignvarstack = 
            case Hash.lookup currentassignvar (assignvarstacks ctxt) of
                Just st -> st 
                Nothing -> []    
        nnassignvarstack
            | length nassignvarstack==0 = []
            | otherwise = tail nassignvarstack
        nassignvarstacks=Hash.insert currentassignvar nnassignvarstack (assignvarstacks ctxt)
        nmon=((head (callerstack ctxt))++":"++(current ctxt)):(mon ctxt)
        nctxt = ctxt{currentassign=ncurrentassign,assignstack=nassignstack,assignvarstacks=nassignvarstacks,mon=nmon}
    in        
        nctxt            



