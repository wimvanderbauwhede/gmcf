-- | Emitter for Skua, the Gambit Scheme back-end
module Gannet.Emitters.Skua (
emitSkuaCode
) where
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration    
import Gannet.Emitters.Common
import Data.Char (toUpper)
--import Control.Monad.State               
--import qualified Data.Map as Hash 

{-
emitSkuaCode emits Scheme code for Skua, a reference implementation of Gannet
in Gambit Scheme
-}

headerStr = ";Skua\n\n" 

-- (define S_alu (ALU))
-- (gw 
openGWStr="(gw\n"
closeGWStr=")\n"

methodCallStr = " '" 

instantiateObjStr str objstr = "(define S_"++str++" ("++objstr++"))\n"

--noServiceStr str = "# No service for " ++ str ++ "\n"
noServiceStr str = 
        let 
                objstr = (toUpper $ str !! 0):((toUpper $ str !! 1): (tail (tail str) ))
        in 
                instantiateObjStr str objstr


openQuoteExprStr =  "(delay "
closeQuoteExprStr = ")"
openExprStr = "(~> "
closeExprStr=")"
openQuoteVarStr =  "'"
closeQuoteVarStr =   ""

noQuote = emptyStr

openParenStr = " "
closeParenStr = " "
commaStr = " "
newlinetabStr = "\n\t"
newlineStr = "\n"
-- <maybe a comma> <maybe an opening quote> <the arg string> <maybe a closing quote>
argStr mc mqo str mqc =mc ++mqo++openExprStr++"S_" ++ str ++ closeParenStr ++closeExprStr++mqc++"\n"
--(~> S_b 'read 'v)
readLetVarStr varnamestr = openExprStr++"S_let"++methodCallStr++"read "++openQuoteVarStr++ varnamestr ++ closeQuoteVarStr++closeQuoteExprStr
-- "(~> S_f 'lambda "++(lambda (x) (lambda (y)++"(...)++"))"
substLambdaVarStr varnamestr = varnamestr 
declLambdaVarStr varnamestr = "(lambda ("++ varnamestr ++ ")"

openLambdaExprStr= "" --(~> S_f 'lambda "
closeLambdaExprStr n = foldl (++) "" (replicate n ")")
openLambdaArgsExprStr=""
closeLambdaArgsExprStr=""
openLambdaBodyExprStr=""
closeLambdaBodyExprStr=""


tagVarDeclOpenStr tagvarname = " (label "++ tagvarname ++ " "
tagVarDeclCloseStr = ")"
tagVarInstStr tagvarname ="(delay ("++ tagvarname ++"))"

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
        tagVarDeclOpenStr
        tagVarDeclCloseStr
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

emitSkuaCode :: SymbolTree -> String
emitSkuaCode st = emitRuntimeCode st codestrings
