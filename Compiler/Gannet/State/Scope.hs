-- | Types for storing scope and functions for manipulating them.
module Gannet.State.Scope (
    VarMap,
--    DataStore,
    ScopeTable,
    ScopeRecord(..),
    initScope,
    getScope,
    getGSfromScope,
    appendScope,
    emptyScope
--    emptyDataStore,
--    appendData
) where
import Gannet.SBA.Types
import Gannet.Warning
import qualified Data.Map as Hash

--------------------------------------------------------------------------------
-- Lexical Scope Table
--------------------------------------------------------------------------------
-- |ScopeTable keeps a list of all vars in a scope
type ScopeTable = Hash.Map Integer ScopeRecord

{- | ScopeRecord stores
 
>    * the enclosing scope
>    * a flag indicating the LET is in a LAMBDA
>    * the kind of symbol         
>    * a map from the symbol's name to the actual symbol and the corresponding number
 
-}    
data ScopeRecord = MkScopeRecord {     
                    enclosing :: Integer, -- ^ Points to the enclosing block
                    isinlambda :: Integer, -- ^ subtask count of enclosing LAMBDA
                    symbolkind :: GSymbolKind, -- ^ D,L or A
                    varmap :: VarMap -- a map from  token to (symbol,count)
                    }
instance Show ScopeRecord where show = showSR
showSR sr = (show (enclosing sr)) ++ " " ++ (show (varmap sr)) ++ "\n"
                   
initScopeRec :: Integer -> Integer -> ScopeRecord
initScopeRec enclosing isinlambda = MkScopeRecord enclosing isinlambda K_R emptyVarMap
                                        
type VarMap = Hash.Map GannetToken (GannetSymbol,Integer)

emptyVarMap :: VarMap
emptyVarMap = Hash.empty

-- |Returns the GannetSymbol for a lexically scoped variable

-- This loops forever on K_L inside LAMBDA. I think the reason is that the 
-- K_L symbol gets put into the scope when leaving the ASSIGN
getGSfromScope :: GannetToken -> Integer -> [Integer] -> ScopeTable -> Integer -> GannetSymbol
getGSfromScope var currentscope inassignstack scopes niters =
    case Hash.lookup currentscope scopes of
        Just scoperec -> 
            case Hash.lookup var (varmap scoperec) of
                Just (gs,_) -> 
                    if ((foldl (&&) True (map (\s->(subtask gs)/=s) inassignstack))&&(kind gs==K_L))||(kind gs/=K_L)
                        then gs --{task=1000*(length inassignstack),name=join inassignstack}
                        else 
                            if enclosing scoperec == 0
                                then
                                    MkGannetSymbol K_Unknown T_x 0 0 0 0 var currentscope 0 M_normal emptyGReg
                                else
                                    if niters>0 then
                                        getGSfromScope var (enclosing scoperec) inassignstack scopes (niters-1)
                                    else error $ "Deep recursion in scope table ("++(show niters)++"): \n"
                                    ++" while looking up "++(show var)++" from scope "++(show currentscope)++"\n"
                                    ++ "inassignstack: "++(show inassignstack)++"; gs: "++(show gs)++"\n"
                                    ++"\n<"
                                    ++(show scopes)++">\n"    
                Nothing -> if enclosing scoperec == 0
                                then
                                    MkGannetSymbol K_Unknown T_x 0 0 0 0 var currentscope 0 M_normal emptyGReg 
                                else
                                if niters>0 then
                                        getGSfromScope var (enclosing scoperec) inassignstack scopes (niters-1)
                                    else error $ "Deep recursion in scope table (2): "++(show scopes)++"\n" 
                                    
        --Nothing -> error $ "Variable "++(show var)++" not in scope\n" -- errorGS
        Nothing -> error $ "Scope "++(show currentscope)++" not in scopes.\n"++(show (Hash.keys scopes))++"\n" -- errorGS

--join inassignstack= GannetTokenL (GannetLabelS (foldl (++) " " (map show inassignstack)))

{-
If we have something like this:
(let
(assign 'a 1.0)
(let
(assign 'a (... (assign 'a ( ... (assign 'a (...a...))))))
))
Then the lookup for a must give the one that is not part of the stack. 
So basically we must compare every scope with all values in the assignstack

so instead of passing inassign, we must pass (assignstack ctxt)
Then instead of inassign/=(assign scoperec) we need something like

foldl True (&&) (map (\s->(assign scoperec)/=s) assignstack)

But that's still not correct: the value can be successfully bound to one of the assigns as long as it's not in the assgin's body
I think it's like this: 

when a LET is encountered, all values inside that let are local to it
expect those inside ASSIGN blocks. For the latter we must climb up.
But again we must always check for LET blocks as well. 
So the actual behaviour requires us to keep a combined stack of LET and ASSIGN,
with an identifier [("let",Integer),...,("assign",Integer),...]

What we really need to do is this:

1. Keep track of the assignstack for every separate variable:
AssignStack = Hash.Map GannetToken [Integer]
2. When looking up the scope for an ASSIGN variable, check if none of the
elements in the assign stack for that var has a Subtask corresponding to the
Subtask in the GannetSymbol part of the VarMap:

getAssignStack :: GannetToken -> Context -> [Integer]
getAssignStack var ctxt = 
    case Hash.lookup var (assignstack ctxt) of
        Just st -> st
        Nothing -> []

    (foldl1 (&&) (map (\s->(subtask gs)/=s) assignvarstack))

Furthermore, managing the assignstack is a bit tricky.
-use currentassign to store the subtask count of the current assign? its GannetToken? both?.
- anyway we need a stack for this too. Maybe like this:
On entering ASSIGN, or at least in appendQSym:
nassignstack=currentassign ctxt
ncurrentassign=(subtaskc ctxt, var)
On leaving a block, test this:
let
 (currentassignsub,_)=currentassign ctxt
in
    if subtaskc ctxt==currentassignsub
        then
            ncurrentassign:nassignstack=assignstack ctxt            
        else
            ncurrentassign:nassignstack=(currentassign ctxt):(assignstack ctxt)
            
-On entering an ASSIGN block: 

var=GannetTokenL (GannetLabelS varname)
pushAssignStack :: GannetToken Context -> Context
pushAssignStack var ctxt =
    let
        ncurrentassign=var -- in ctxt
        assignvarstack = case Hash.lookup var assignstack of
                        Just st -> st
                        Nothing -> []
        nassignstack = Hash.insert (GannetTokenL (numerify (GannetLabelS varname))) (subtaskc:assignvarstack) (assignstack ctxt)
    in
        ctxt{currentassign=ncurrentassign,assignstack=nassignstack}

-on leaving an ASSIGN block:
popAssignStack :: Context -> Context
popAssignStack ctxt = 
    let         
        assignvarstack = tail (case Hash.lookup (currentassign ctxt) (assignstack ctxt) of
                        Just st -> st
                        Nothing -> []
                        )
        nassignstack=Hash.insert (currentassign ctxt) assignvarstack (assignstack ctxt)
    in
        ctxt{currentassign=ncurrentassign,assignstack=nassignstack}
-}

-- |Returns the symbol Kind, the count of the var and the lambda of the var
getScope :: GannetToken -> Integer -> ScopeTable -> (GSymbolKind,Integer,Integer)
getScope var currentscope scopes =
    case Hash.lookup currentscope scopes of
        Just scoperec -> 
            if (Hash.member var (varmap scoperec)) 
                then ((symbolkind scoperec), currentscope, (isinlambda scoperec))
                else getScope var (enclosing scoperec) scopes
        Nothing -> (K_E,0,0)

{- | The scope table must be initialized with an "empty" record to keep
track of the enclosing scopes. This is required for LET without ASSIGN
-}
initScope :: Integer -> Integer -> Integer -> ScopeTable -> ScopeTable
initScope currentscope enclosingscope inlambda scopes = 
    case Hash.lookup currentscope scopes of
            Just scoperec -> scopes
            Nothing -> 
                let
                    initscoperec= initScopeRec enclosingscope inlambda
                in
                    Hash.insert currentscope initscoperec scopes                        

{- | appendScope adds a ScopeRecord to the ScopeTable. 
It constructs the ScopeRecord from 
    Integer enclosingscope
    GannetSymbol gsym ,Integer varc
    Integer inlambda    
    GSymbolKind skind
and maps it to Integer currentscope
-}     

appendScope :: GannetSymbol -> Integer -> GSymbolKind -> Integer -> Integer -> Integer -> ScopeTable -> ScopeTable
appendScope gsym varc skind currentscope enclosingscope inlambda scopes = let
    var=name gsym
    cscoperec=
        case Hash.lookup currentscope scopes of
            Just scoperec ->
                let
                    cvarmap= Hash.insert var (gsym,varc) (varmap scoperec)
                in                    
                      scoperec{varmap=cvarmap,isinlambda=inlambda}
            Nothing -> 
                let
                    cvarmap= Hash.singleton var (gsym,varc)
-- We could maybe add varc (gsym,varc) and then use (name gsym) as var
                    nvarmap= Hash.insert (GannetTokenL (GannetLabelI varc)) (gsym,varc) -- FIXME: unused!
-- A better solution is to add a proper lookup table in the context
-- GannetToken -> GannetToken, the key is GannetLabelI, the value is GannetLabelS
-- This is possible as the numeric values are global. not scoped
                in
                    MkScopeRecord enclosingscope inlambda skind cvarmap
    in
        Hash.insert currentscope cscoperec scopes                        
        
emptyScope :: ScopeTable
emptyScope = Hash.empty -- ScopeTable


--------------------------------------------------------------------------------
-- Data Store
--------------------------------------------------------------------------------
--type DataStore = Hash.Map GannetToken GannetBuiltin
--
--appendData :: GannetToken -> GannetBuiltin -> DataStore -> DataStore
--appendData var val store = Hash.insert var val store
--
--emptyDataStore :: DataStore
--emptyDataStore = Hash.empty

