{-# LANGUAGE CPP #-}

-- WV20110609 for old config handling, revert to before r4987 and undef NEW

module Gannet.State.SymbolTree (
    unwrapST,
    unwrapSymbolTree,    
    appendST
) where
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration
import Gannet.State.Scope
import Gannet.State.Context
import Gannet.Numerifier
import Gannet.Symbolizer.InferTypes

import Control.Monad.State 
import qualified Data.Map as Hash

-- | unwrap the SymbolTree, i.e. get it out of the State monad
unwrapST :: State SymbolTree (TokenTree,Context) -> SymbolTree -> (SymbolTree,Context)
unwrapST lst st0 =
    let
        --(State lf)=lst
        --((tt,ctxt),st) = lf st0
        ((tt,ctxt),st) = runState lst st0
    in 
        (st,ctxt)

unwrapSymbolTree :: State SymbolTree (TokenTree,Context) -> (SymbolTree,Context)       
unwrapSymbolTree lst =
    let
        --(State lf)=lst
        --((tt,ctxt),st) = lf emptySL
        ((tt,ctxt),st) = runState lst emptySL
    in 
        (st,ctxt)

{-
I'd like to modify the SLH here: 
If ms is a symbol, do this; if it's a symbollist, do that
-}

-- | Update the SymbolList header
updateSLH :: SymbolTree -> SymbolTree -> Context -> (SymbolTree,Context) -- looks like we could use a monad here
updateSLH st ms ctxt =
    let
        nsl= getSL st         
        slh = getSLH st
        slhl = label slh
        (nslh,ctxt1)= case ms of    
            Symbol gs -> 
                let 
                    nlambda
                        | kind gs == K_A = lambda gs
                        | otherwise = newLambda (lambda gs) (rlambda slh)                    
                in
                if (kind gs)==K_S 
                    then
                        let
                -- set to to current from the context in (name st2)
                            to=GannetLabelS $ lookupFQN $ current ctxt
                -- set return_to to the first elt in the callerstack:
                            _:caller:_=(callerstack ctxt)
                            from=GannetLabelS caller
                            ntagged
                                | (reflabel ctxt) /= emptyGT = True
                                | otherwise = False
-- type inferencing should be done in the HLL compiler. 
                            nretval = GAny
                            getDT (GSV (gk,gdt)) = gdt
                            getDT  _ = T_d
                            ndatatype = reconcileTypes (datatype slhl) (getDT nretval) -- NEW: reconcileTypes is OK for FQN
                            nlabel = gs{kind=K_R,datatype=ndatatype,name=(GannetTokenL to)}
                        in
                            (slh{lto=to,lfrom=from,label=nlabel,rlambda=nlambda,tagged=ntagged,retval=nretval},ctxt)
                    else
                        let
                            notlambda = lookupPQN ( ( \(GannetLabelS nstr)->nstr ) (lto slh))  True /= ("APPLY","lambda")
                            nlabel=label slh
                            ndatatype = reconcileTypes (datatype slhl) (inferGDTbyArgs st ms) -- NEW: infortunately, inferGDTbyArgs is not OK for FQN
                        in
                            if (kind gs)==K_A && notlambda -- this is a proper subtask with a lambda var, not a lambda expr
                                then
                                    (slh{rlambda=nlambda,label=nlabel{count=nlambda,datatype=ndatatype},to_apply=True},ctxt)
                                else
                                    (slh{rlambda=nlambda,label=nlabel{count=nlambda,datatype=ndatatype}},ctxt)       
            SymbolList (_,gslh) ->
                let
                    notlambda = lookupPQN ( ( \(GannetLabelS nstr)->nstr ) (lto slh)) True /= ("APPLY","lambda") 
                    nlambda
                     | notlambda = newLambda (rlambda gslh) (rlambda slh)
                     | otherwise = 0
                    maybe_to_apply
                      | notlambda = (to_apply gslh) || (to_apply slh)
                      | otherwise = False                                             
                in
                    let
                        nlabel=label slh
                        ndatatype = reconcileTypes (datatype slhl) (inferGDTbyArgs st ms)
                    in
                        (slh{rlambda=nlambda,label=nlabel{count=nlambda,datatype=ndatatype},to_apply=maybe_to_apply},ctxt)
        -- NEW: the next statements should all be modified to support multiple providers for LET, APPLY etc
        (nsl2,nslh2,ms2,ctxt2)
            |((newtestService nslh "assign") || (newtestService nslh "buf")) && ((length nsl==3)||(length nsl==4)) && (notQuote ms) = 
                let
                    assign:quote:var:mq = nsl
                    Symbol varsym =var
                    ndatatype=getGDT ms                    
                    nnsl = assign:quote:(Symbol varsym{datatype=ndatatype}):mq
                    nctxt = updateVar ctxt varsym ms                    
                in
                    (nnsl,nslh,ms,nctxt)
            |((newtestService nslh "read") || (newtestService nslh "stream")) && (length nsl==2) =
                let
                    Symbol varsym = ms                   
                    scopes=scope ctxt
                    ngs = lookupGSinScope scopes varsym
                    clabel = label nslh
                    nlabel = clabel{datatype=(datatype ngs)}                                   
                in
                    (nsl,slh{label=nlabel},Symbol ngs,ctxt)
--            |((newtestService nslh "apply")||(newtestService nslh "applytc")) && (length nsl>1) =
--                let    
--                    nctxt = ctxt -- bindLambdaArg ctxt (SymbolList (nsl,nslh)) ms
--                in
--                    (nsl,nslh,ms,nctxt)                 
            | otherwise = (nsl,nslh,ms,ctxt)
    in
        (SymbolList (nsl2++[ms2],nslh2),ctxt2)

        
{-

tct2 <- appendST qsym tct1

appendST appends a SymbolTree (i.e. either a Symbol or a list of Symbols)
to an (invisible ) SymbolTree. You pass it a SymbolTree and
a (TokenTree,Context) tuple. It will modify the Context as required;
appendST does not modify the TokenTree, that happens in t2sm. 

It's just a convenient way of passing the TokenTree around in the monad.


-}

-- | Append a Symbol or SymbolList to the SymbolTree        
appendST :: SymbolTree -> (TokenTree,Context) -> State SymbolTree (TokenTree,Context)
appendST st2 (tt,ctxt) = state (\st ->
        let
            (st3,ctxt2)=updateSLH st st2 ctxt
            ctxt3=ctxt2{reflabel=emptyGT}
        in
            ((tt,ctxt3),st3)    
    )

{-
if one of them is 0, take the other.
if one is 11 and the other 12:
e.g. (lambda:11 (s:12... (... (lambda:16 ... (s:17...
-}

newLambda :: Integer -> Integer -> Integer
newLambda l1 l2 
    | (l1 == l2) && (l1==0) = 1 -- Ad Hoc 
    | l1 == 0 = l2
    | l2 == 0 = l1
    | l1 <= l2 = l1 -- dubious
    | l1 > l2 = l2

-- this function returns the actual service instance for operators
-- i.e. for '+' it will return ALU, 
-- for 'head' it will return 'head', not 'let', meaning this will be resolved in the Numerifier 

{-
To use Services instead of Aliases for the ALU, we need this:
Just opname -> if hasMethod "ALU" opname then (GannetLabelS "ALU") else gls
            
-}

   
{-
Below is an attempt to create my own monad.
After creating my own TState Monad, I saw I could just use the ordinary State Monad
So the key is NOT so much the actual Monad, but the wrapping/unwrapping functions that go with it!

What totally baffled me is that the state MUST be the first argument
I guess the "TState st" below is partial application, whereas I thought it refered to the lambda
I tried to remove the "tt" from this, but then I get a "kind error".

newtype TState st tt = TState (st -> (tt,st))
                        
instance Monad (TState st) where
    return tt = TState (\st -> (tt,st))
    (TState fr) >>= g = TState $ \st1 -> 
                    let
                        (tt1,st2) =fr st1
                        (TState gr) = g tt1                            
                    in      
                        gr st2                  


unwrapST :: TState SymbolTree TokenTree -> SymbolTree -> SymbolTree
unwrapST lst st0 =
            let
                (TState lf)=lst
                (tt,st) = lf st0
            in 
                st      
                
appendST :: SymbolTree-> TokenTree -> TState SymbolTree TokenTree
appendST st2 tt= TState (\st->(tt,(SymbolList ((getSL st)++[st2]))))                                   
-}

