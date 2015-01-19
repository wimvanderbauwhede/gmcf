-- Emitter for Puffin, the Perl 6 back-end (works with Pugs, not with Rakudo)
module Gannet.Emitters.Puffin (
emitPuffinCode
) where
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration    
import Gannet.Emitters.Common
import Data.Char (toUpper)
--import Control.Monad.State               
--import qualified Data.Map as Hash 

{-
emitPuffinCode emits Perl 5 code for Puffin, a reference implementation of Gannet
in Perl 5
-}

headerStr = "use v6-alpha;\n\nuse Puffin;\n" ++
                        "if (@ARGS && (@ARGS[0] eq '-v')) {\n" ++
                        "        @ARGS.shift;\n" ++
                        "        $Puffin::v=1;\n" ++
                        "} else {\n" ++
                        "        $Puffin::v=0;\n" ++
                        "}\n\n" 

openGWStr="Puffin::gw(\n"
closeGWStr=");\n"
varSigil=['$']
methodCallStr = "." 

instantiateObjStr str objstr = "my "++['$']++"S_"++str++"="++objstr++methodCallStr++"new();\n"

--noServiceStr str = "# No service for " ++ str ++ "\n"
noServiceStr str = 
        let 
                objstr = (toUpper $ str !! 0):((toUpper $ str !! 1): (tail (tail str) ))
        in 
                instantiateObjStr str objstr


openQuoteExprStr =  "{" -- was "\\{"
closeQuoteExprStr = "}"
openExprStr = "async {"
closeExprStr="}"

openLambdaExprStr="{ sub { "
closeLambdaExprStr n="}}"
openLambdaArgsExprStr="my (";
closeLambdaArgsExprStr=")=@_;"
openLambdaBodyExprStr=openExprStr++['$']++"S_"
closeLambdaBodyExprStr=" "

openQuoteVarStr =  "'"
closeQuoteVarStr =   "'"
noQuote = emptyStr

openParenStr = "("
closeParenStr = ")"
commaStr = ","
newlinetabStr = "\n\t"
newlineStr = "\n"
-- <maybe a comma> <maybe an opening quote> <the arg string> <maybe a closing quote>
argStr mc mqo str mqc 
        | c==')' = str ++ closeParenStr ++closeExprStr++"\n"
        | otherwise = mc ++mqo++openExprStr++['$']++"S_" ++ str ++ closeParenStr ++closeExprStr++mqc++"\n"
        where
                c:_=str
readLetVarStr varnamestr = openExprStr++['$']++"S_let"++methodCallStr++"read("++openQuoteVarStr++ varnamestr ++ closeQuoteVarStr++")"++closeQuoteExprStr
--substLambdaVarStr varnamestr = openExprStr++['$']++"S_apply"++methodCallStr++"subst("++openQuoteVarStr ++ varnamestr ++ closeQuoteVarStr++")"++closeExprStr
substLambdaVarStr varnamestr = ['$']++ varnamestr
declLambdaVarStr lvarname = ['$']++lvarname

{-
(lambda 'xv 'yv '(...))
$S_apply.lambda( ++ {sub { my ( ++ $xv,$yv, ++ )=@_; ++ async {...}}})

So we need

openLambdaExprStr = "{ sub {"
openLambdaArgsStr=" ("
closeLambdaArgsStr = ")"
openLambdaBodyStr= "= @_; "
closeLambdaBodyStr= ""
closeLambdaExprStr = "}}"
lambdaVarDeclStr varnamestr = "my "++['$']++varnamestr

But we must change the compiler to understand this.
In particular:
-distinguish between Lambda and Assign
-don't assume joining commas
With minimal changes we can arrive at
$S_apply.lambda( ++ {sub { ( ++ my $xv, my $yv ++ , ++ async {...})

-Maybe we can put in a hook to have a closeLambdaExprStr call if the service is lambda
That would result in 
$S_apply.lambda( ++ {sub { ( ++ my $xv, my $yv ++ , ++ async {...} ++ }})
So all that remains to be fixed is the comma! 
maybeComma looks at the Kind. Could we use this?
also, we could do a hack: a comma after the last lambdavar is OK
so we can have the comma in lambdaVarDeclStr:
        lambdaVarDeclStr varnamestr = "my " ++ ['$'] ++ varnamestr ++ ","
Then what we should do is suppress the commas in the lambda call.
-}

tagVarDeclStr tagvarname = " "++['$']++"*"++ tagvarname ++ " = "
tagVarInstStr tagvarname = " "++['$']++"*"++ tagvarname

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

emitPuffinCode :: SymbolTree -> String
emitPuffinCode st = emitRuntimeCode st codestrings
