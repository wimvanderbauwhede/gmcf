{-# LANGUAGE CPP #-}
-- | Functions for turning a SymbolTree into a Gannet PacketList.
-- Functions for emitting human-readable and bytecode strings.

-- WV20110609 for old config handling, revert to before r4987 and undef NEW

module Gannet.Packetizer (
    packetize,
    gplToWords,
--    writeData    
) where
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration
import Gannet.Numerifier
import Gannet.Bytecodizer
import Gannet.State.Scope
import Gannet.State.Context
import Gannet.Warning (warning)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8

import Control.Monad.State 
import qualified Data.Map as Hash

packetize :: SymbolTree -> Context -> Bool -> PacketList
packetize st ctxt num= pl_lrefs -- warning pl_remapped (show pl_lrefs) -- 
    where
        stw=SymbolList ([st],emptySLH)
        st_r=(stw,emptyGS)
        scopes = scope ctxt
        (pl,lrefs) = unwrapPL (st2plm st_r scopes Hash.empty num) emptyGPL
        pl_lrefs = appendLambdaRefs pl lrefs num
        pl_remapped = Hash.fromList $ remapSubtasksPL (Hash.toList pl_lrefs) ctxt emptySM []
        

type LambdaRefs = Hash.Map Integer [GannetSymbol]
        
appendLambdaRefs :: PacketList -> LambdaRefs -> Bool -> PacketList
appendLambdaRefs pl lrefs num = Hash.fromList (map (\(r,p)->appendIfLambda r p lrefs num) (Hash.toList pl))
{-
So we have the lamdba refs in a hash, what we need to do is append them to the actual lambda packet, isn't it?    
So we have a pref=>p hash.
If this pref is a lambda, then we must append the lambda refs
so:
-}
appendIfLambda :: GannetSymbol ->  GannetPacket -> LambdaRefs -> Bool -> (GannetSymbol,GannetPacket)
appendIfLambda pref p lrefs num 
    |testLambda =
        case Hash.lookup (subtask pref) lrefs of
            Just reflist -> (pref,appendRefs p reflist) 
            _           -> (pref,p)
    | otherwise = (pref,p)
    where
        testLambda = (newtestServiceGT (name pref) "lambda" num) -- `warning` ("TEST LAMBDA: "++(show (subtask pref))++"=>"++(show ((newtestServiceGT (name pref) "lambda" num))))

                
{-
appendIfLambda :: GannetSymbol ->  GannetPacket -> LambdaRefs -> Bool -> (GannetSymbol,GannetPacket)
appendIfLambda pref p lrefs num
    -- | newtestServiceGT (name pref) "lambda" num = (pref,p) -- NEW
    | testLambda = (pref `warning` (show (pref,p)),p) -- NEW
    | otherwise = case Hash.lookup (subtask pref) lrefst of
            Just reflist -> (pref,appendRefs p reflist) `warning` ("Appending "++(show p)++" to "++(show $ name pref)) -- FIXME: somehow this is wrong!
            _           -> (pref,p)
    where
        lrefst = lrefs `warning` (("lrefs: "++(show lrefs))++(" subtask: "++(show pref)))
        testLambda = (newtestServiceGT (name pref) "lambda" num) -- `warning` ("TEST LAMBDA: "++(show (subtask pref))++"=>"++(show ((newtestServiceGT (name pref) "lambda" num))))
-}
appendRefs :: GannetPacket -> [GannetSymbol] -> GannetPacket
appendRefs p reflist =
    let
        (gph,gppl) = p 
        rootref=head $ reverse gppl
        reflist_norootref = filter (\gs->(gs{quoted=0}/=rootref{quoted=0})) reflist      
    in
        (gph{plength=(plength gph)+(length reflist_norootref)},gppl++reflist_norootref)      
        

{-
unwrapPLOLD :: State PacketList LambdaRefs -> PacketList -> (PacketList,LambdaRefs)
unwrapPLOLD lst pl0 =
    let
        State lf=lst
        (lrefs,pl) = lf pl0
    in 
        (pl,lrefs)
-}
unwrapPL :: State PacketList LambdaRefs -> PacketList -> (PacketList,LambdaRefs)
unwrapPL lst pl0 =
    let
--        State lf=lst
        (lrefs,pl) = runState lst pl0 
    in 
        (pl,lrefs)

updatePL :: PacketList -> SymbolTree -> GannetSymbol -> ScopeTable -> LambdaRefs -> Bool -> (LambdaRefs,PacketList)
updatePL pl st r scopes lrefs num = 
    let
        slh = getSLH st
        ref = label slh
        nref 
            | (rlambda slh >1) =ref{ext=1,lambda=rlambda slh} --WV: ext=1 indicates it's "tainted". But that should be redundant
            | otherwise = ref
        nnref=numerify nref num scopes
        lrefs1
            | to_apply slh /= False = appendLRef nnref lrefs
            | otherwise = lrefs
        p_to 
            | to_apply slh = newnumServiceGL (getGSNameStr (label slh)) num
            | otherwise = newnumService (lto slh) num -- NEW: this is the nodeId, so use lookupNodeIdFromFQN
        pn = case Hash.lookup r pl of
            Just p -> 
                let 
                    (nph,nppl) = p 
                    (nppl2,nref2,plen2)
                        | nppl == [] = ([],nnref,0)
                        | kind (last nppl)==K_Q = 
                            let
                                gss:rest=nppl
                                nppl1=gss{count=(count gss)-1}:rest
                            in
                                (init nppl1,nnref{quoted=1},(plength nph)-1)
                        | otherwise = (nppl,nnref,plength nph)
                in 
                    if last nppl2 /= nref2{quoted=1} 
                        then
                            (nph{plength=(plen2+1)},nppl2++[nref2])
                        else
                            (nph{plength=plen2},nppl2)
            Nothing -> (emptyGH,[nnref])
        pl2 = Hash.insert r pn pl
        p_ret  -- = numServiceGL "gateway" num -- silly!
            | num = GannetLabelI 0
            | otherwise =  GannetLabelS "gateway"
        -- WV12082008: this works, but why does numerify not work?
        refnamestr = (\ref -> let (GannetTokenL (GannetLabelS namestr)) = name ref in namestr) nref
        nnref2=nref{name=newnumServiceGT refnamestr num}
        ph = MkGannetHeader P_code 0 0 0 p_to p_ret nullGS nnref        
    in
        (lrefs1,Hash.insert nnref (ph,[]) pl2)

appendPL :: SymbolTree -> GannetSymbol -> ScopeTable -> LambdaRefs -> Bool -> State PacketList LambdaRefs 
--appendPL x r scopes lrefs num = State (\pl->(updatePL pl x r scopes lrefs num))        
appendPL x r scopes lrefs num = state (\pl->(updatePL pl x r scopes lrefs num))        
        
{-
This one takes a PacketList and a key, and a GannetSymbol to be added at the
end of the packet payload
-}
updateP :: PacketList ->  SymbolTree -> GannetSymbol -> PacketList
updateP pl x r =
    let
        Symbol gs = x 
        pn= case Hash.lookup r pl of
                Just p -> 
                    let 
                        (nph,nppl) = p 
                        (nppl2,ngs,plen2)
                            | nppl == [] = ([],gs,0)
                            | kind (last nppl)==K_Q = 
                                let
                                    gss:rest=nppl
                                    nppl1=gss{count=(count gss)-1}:rest
                                in
                                    (init nppl1,gs{quoted=1},(plength nph)-1)                            
                            | otherwise = (nppl,gs,plength nph)
                        nppl3
                            | (kind ngs)==K_R && (ext ngs)==1 && (quoted ngs)==0 && (count ngs)==(subtask r) =
                                let
                                    gss:rest=nppl2                                    
                                in
                                    gss{count=(count gss)+1}:rest
                            | otherwise = nppl2
                        egs=extendGS ngs
                    in 
                        (nph{plength=(plen2+(length egs))},nppl3++egs)
                Nothing -> (emptyGH,extendGS gs)
    in
        Hash.insert r pn pl    
-- appendP (Symbol (numerify xgs num scopes)) r1l lambdarefs num
appendP :: SymbolTree -> GannetSymbol -> LambdaRefs -> Bool -> State PacketList LambdaRefs 
--appendP x r lrefs num = State (\pl->(updateLRefs x r lrefs num,updateP pl x r))
appendP x r lrefs num = state (\pl->(updateLRefs x r lrefs num,updateP pl x r))

updateLRefs ::  SymbolTree -> GannetSymbol -> LambdaRefs -> Bool -> LambdaRefs
updateLRefs  x r lrefs num

-- if gs is numerified, we need to compare the 
     | kind gs == K_S && isLambda gs num = Hash.insert (subtask gs) [] lrefs -- NEW: main question is if this is numeric or string
     | otherwise = lrefs 
    where
        Symbol gs = x

isLambda gs num =
    let
        gt = name gs
    in
        if num
            then
                let
                    (_,lsclid,lscid,lopcode) = lookupFQNIds "lambda"
                    mlnum = (\(GannetTokenS (GannetLabelI mlnum))->mlnum) gt
                    mlsclid = getSCLId mlnum
                    mlscid = getSCId mlnum
                    mlopcode = getOpcode mlnum
                in
                    lsclid==mlsclid && lscid==mlscid && lopcode==mlopcode    
            else
                ("APPLY","lambda")==lookupPQN ((\(GannetTokenS (GannetLabelS lstr))->lstr) gt) True        


appendLRef :: GannetSymbol -> LambdaRefs -> LambdaRefs      
appendLRef nref lrefs =
    case Hash.lookup (count nref) lrefs of -- looks like we use count to determine a lambda ref. But the .insert uses subtask
        Just reflist -> Hash.update (\v->Just (v++[nref{quoted=1}])) (count nref) lrefs
        Nothing ->  Hash.insert (count nref) [nref{quoted=1}] lrefs -- I don't like this: the entry should have been created already!

-- transform Symbol Tree into Packet List Monad
-- GannetSymbol (r in st_r) is the key in the PacketList hashtable 
                      
st2plm :: (SymbolTree,GannetSymbol) -> ScopeTable -> LambdaRefs -> Bool -> State PacketList LambdaRefs -- (SymbolTree,GannetSymbol)
st2plm st_r scopes lambdarefs num
    | length sl==0 = do return lambdarefs -- st_r
    | isSL x = -- it's a list of symbols
        do
            let
                rn = (numerify r num scopes)
            lrefs1 <- appendPL x rn scopes lambdarefs num
            lrefs2 <- st2plm (x,rn) scopes lrefs1 num
            st2plm st_r1 scopes lrefs2 num
    | otherwise = -- it's a symbol
        do                
            let 
                Symbol xgs = x 
            lrefs1 <- appendP (Symbol (numerify xgs num scopes)) r1l lambdarefs num
            st2plm st_r1 scopes lrefs1 num
    where
        (SymbolList st,r)=st_r
        (sl,slh)=st
        x:xs=sl
        st1=SymbolList (xs,slh)
        -- create a reference for the packet. 
        r1=numerify (label slh) num scopes
        r1w = r1 -- warning r1 ("Reference: "++(show r1))
        r1l 
            | (rlambda slh >1) = r1w{ext=1,lambda=rlambda slh}
            | otherwise = r1w
        st_r1=(st1,r1l)

-- | Either turn a PacketList into a pretty-print string or into a bytecode string for writing to a .tdc file.        

gplToWords :: PacketList -> Bool -> BS.ByteString
gplToWords_OLD gpl numeric = 
    let 
        rootref = case Hash.lookup emptyGS gpl of 
            Just (ph,ppl) -> head ppl -- i.e. first elt of payload, should be K_S
            Nothing -> error $ "No rootref in "++show gpl
        (rph,rppl) = case Hash.lookup rootref gpl of 
            Just (ph,ppl) -> (ph,ppl)         
            Nothing -> error $ "Rootref "++show rootref++"not in "++show (Hash.toList gpl)
        rph2=rph{ptype=P_subtask}
        gpl1    =Hash.insert rootref (rph2,rppl) gpl    
        gpl2=Hash.delete emptyGS gpl1
        labels = Hash.keys gpl2
        npackets
            | numeric==True = intToBytes $ toInteger (length labels)
            | otherwise = BSC8.pack ("NPackets: "++show (length labels))
        strlist=    map (\label -> (gpToWords ((\label gpl2 -> case Hash.lookup label gpl2 of Just p -> p) label gpl2) numeric)) labels        
    in    
        BS.append npackets (BS.concat strlist)
        
-- | Either turn a PacketList into a pretty-print string or into a bytecode string for writing to a .tdc file.        
gplToWords gpl numeric = 
    let 
        p = case Hash.lookup emptyGS gpl of 
            Just p -> p
            Nothing -> error $ "No rootref in "++show gpl
        (ph,ppl) = p
        rootref= head ppl
        (rph,rppl) = case Hash.lookup rootref gpl of 
            Just (ph,ppl) -> (ph,ppl)         
            Nothing -> error $ "Rootref "++show rootref++"not in "++show (Hash.toList gpl)
--        gpl1    =Hash.insert rootref p gpl    
        gpl1 = gpl
        rootref2 = emptyGS{kind=K_R, name=name (return_as rph)}
        rph2=rph{ptype=P_reference,plength=1,to=to rph}
        rppl2 = [ return_as rph2 ]
        gpl12=Hash.delete emptyGS gpl1
        gpl2    =Hash.insert rootref2 (rph2,rppl2) gpl12    
--        gpl12=gpl1
--        gpl2=gpl12
        labels = Hash.keys gpl2
{-        
        npackets
            | numeric==True = intToBytes $ toInteger (length labels)
            | otherwise = "NPackets: "++show (length labels)
        strlist=    map (\label -> (gpToWords ((\label gpl2 -> case Hash.lookup label gpl2 of Just p -> p) label gpl2) numeric)) labels        
        --strlist= map (\label -> ["\n--------\nLABEL: ",show label," =>"]++(gpToWords ((\label gpl2 -> case Hash.lookup label gpl2 of {Just p -> p; Nothing -> (emptyGH,[emptyGS])}) label gpl2) numeric)) labels
    in    
        npackets++(concat (concat strlist))
-}        
        npackets
            | numeric==True = intToBytes $ toInteger (length labels)
            | otherwise = BSC8.pack ("NPackets: "++show (length labels))
        strlist=    map (\label -> (gpToWords ((\label gpl2 -> case Hash.lookup label gpl2 of Just p -> p) label gpl2) numeric)) labels        
    in    
        BS.append npackets (BS.concat strlist)
        
-- gpToWords :: GannetPacket -> Bool -> [String]
gpToWords :: GannetPacket -> Bool -> BS.ByteString
gpToWords gp numeric
    | numeric==True = BS.concat $ bytecodize gp
    | otherwise = 
        let 
            (gph,gppl) = gp 
        in
             BS.append (BSC8.pack (show gph)) (BS.concat (map addNL gppl))
--gpToWords (gph,gppl) = map gsToBC gppl
--gpToWords gp = bytecodize gp


addNL :: GannetSymbol -> BS.ByteString
addNL gs =  BSC8.pack $ (show gs)++"\n"

-- For FQN, we should have a new showFQN function

--gsToBC :: GannetSymbol -> [Char]
--gsToBC gs = bytecodize gs

{-
what we need to do is
-get value for label: numvals = map (\gt->(numData gt num scopes) labels
-convert label to numeric
-combine into string
-}
-- | Either turn the contents of the DataStore into a pretty-print string or into a string for writing to a .data file.
-- This functionality is deprecated.
--writeData :: Context -> String
--writeData ctxt = 
--    let
--        datalist = datastore ctxt
--        labels = Hash.keys datalist
--        ndata = length labels        
--        num = numeric ctxt
--        scopes = scope ctxt
--        numkeys = map (\gt->(numData gt num scopes)) labels
--        datavals = map (\lb->(case Hash.lookup lb datalist of Just v -> v)) labels
--        datapairs = zip numkeys datavals
--        datapairstr 
--            | num = foldl (\x y ->(x ++ (pair2str y))) ((show ndata)++"\n") datapairs
--            | otherwise = show datalist
--    in
--        datapairstr


pair2str     :: (Integer,GannetBuiltin) -> String
pair2str (k,v)= (show k)++" "++(show v)++"\n"

{-
Strategy for emitting C from PacketList:
1. Emit function declarations
2. Emit the main function, i.r. run()
3. Emit the function definitions.

1. Function declarations:
If untainted, it's simply type Rx(); with type based on the Datatype field
If tainted, it's more difficult. What we need to do is find the parent lambda
and determine its signature. How? A possible way is via the apply call to the
lambda. But that might fail, as it might not end on leaf nodes.


-}


{-
 type PacketList = Hash.Map GannetSymbol GannetPacket
 type GannetPacket = (GannetHeader,[GannetSymbol])
for every packet in the list:
1. remap the reference
2. remap the return_as
3. remap and refs in the payload 
 
We need a datastructure Subtask->new subtask which we update on the fly . Should put it in Context?
-}
-- map old_subtask => (sid,new_subtask)
type SubtaskMap = Hash.Map Integer (Integer,Integer)

emptySM :: SubtaskMap
emptySM = Hash.empty

remapSubtasksPL :: PacketRefList -> Context -> SubtaskMap -> PacketRefList -> PacketRefList
remapSubtasksPL prl ctxt subtaskmap prl_r
    | length prl ==0 = prl_r
    | otherwise =
        let
            pp:prl2 = prl 
            (pp_r,ctxt2,subtaskmap2)=remapSubtaskP pp ctxt subtaskmap
        in
            remapSubtasksPL prl2 ctxt2 subtaskmap2 (pp_r:prl_r)


remapSubtaskP :: (GannetSymbol,GannetPacket) -> Context -> SubtaskMap -> ((GannetSymbol,GannetPacket),Context,SubtaskMap)
remapSubtaskP pp ctxt subtaskmap =
    let
        (pref,(ph,ppl))=pp
        (pref_r,ctxt2,subtaskmap2) = remapSubtask pref ctxt subtaskmap
        r_as = return_as ph        
        (r_as_r,ctxt3,subtaskmap3) = remapSubtask r_as ctxt2 subtaskmap2
        ph_r=ph{return_as=r_as_r}
        (ppl_r,nctxt,nsubtaskmap) = remapSubtaskPPL ppl ctxt3 subtaskmap3 []
        pp_r=(pref_r,(ph_r,ppl_r))
--        (pp_r,nctxt,nsubtaskmap)= ((pref_r,(ph,ppl)),ctxt2,subtaskmap2)
    in
        (pp_r,nctxt,nsubtaskmap)                                 

remapSubtaskPPL :: [GannetSymbol] -> Context -> SubtaskMap -> [GannetSymbol] -> ([GannetSymbol],Context,SubtaskMap)
remapSubtaskPPL ppl ctxt subtaskmap ppl_r
    | length ppl ==0 = (ppl_r,ctxt,subtaskmap)
    | otherwise =
        let
            p:ppl2 = ppl
            (p_r,ctxt2,subtaskmap2)            
                | kind p == K_R || kind p == K_C  = remapSubtask p ctxt subtaskmap
                | otherwise = (p,ctxt,subtaskmap)
        in
            remapSubtaskPPL ppl2 ctxt2 subtaskmap2 (ppl_r++[p_r])


-- remap the subtask using per-service address counters
                
remapSubtask :: GannetSymbol -> Context -> SubtaskMap -> (GannetSymbol,Context,SubtaskMap)
remapSubtask gs ctxt subtaskmap =
    if gs==emptyGS
    then (gs,ctxt,subtaskmap)
    else
        case Hash.lookup (subtask gs) subtaskmap of
            Just (sid,st_r) -> (gs{subtask=st_r},ctxt,subtaskmap)
            Nothing -> 
                let
                    sid = case name gs of
                         GannetTokenS (GannetLabelI gsnum) -> getSNId gsnum
                         GannetTokenL (GannetLabelI gsnum) -> getSNId gsnum 
                         otherwise -> lookupNodeIdFromFQN (getStrFromToken (name gs))
                    maddr = case Hash.lookup sid (addrcounters ctxt) of
                                Just a -> a
                                Nothing -> -1
                    nctxt 
                        | maddr>0 =
                            let
                                naddr = maddr+1
                                ncounters=Hash.insert sid naddr (addrcounters ctxt)
                            in
                                ctxt{addrcounters=ncounters}
                        | otherwise = ctxt    
                    addr
                        | maddr>0 = maddr
                        | otherwise = (subtask gs)
                    nsubtaskmap = Hash.insert (subtask gs) (sid,addr) subtaskmap
                in
                    (gs{subtask=addr},nctxt,nsubtaskmap)
