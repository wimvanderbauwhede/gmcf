-- | The tokenizer takes a string (the contents of a .td file) 
-- and parses it into a TokenTree 
module Gannet.Tokenizer (
    tokenize
) where
import Gannet.SBA.Types
import Gannet.TokenParser
import Text.ParserCombinators.Parsec
{- 
TODO: Here in the Tokenizer we should do the syntax checks, 
to make sure that the tree is sound.
-}
tokenize :: String -> TokenTree
tokenize input = tl2tt tl emptyTL []
    where
        tl = stringToTokens input

-- | This is a very simple Parsec lexer which turns a Gannet text file into a list of strings
stringToTokens :: String -> [String]
stringToTokens input = case parse parseTokens "" input of
    Left err -> ["ERROR: " ++ show err]
    Right val -> val

parseTokens :: Parser [String]
parseTokens =  do {
                x <- parseComment <|> parseLParen <|> parseRParen <|> parseQuote <|> parseSymbol <|> parseString;
                spaces;
                do {
                    xs <- parseTokens;
                    return $ [x]++xs
                    }
                <|> return [x]
                }
                

parseLParen :: Parser String
parseLParen = 
    do
        c <- char '('
        return [c]

parseRParen :: Parser String
parseRParen = 
    do
        c <- char ')'
        return [c]
                
parseQuote :: Parser String
parseQuote = 
    do
        c <- char '\''
        return [c]
                
parseSymbol :: Parser String
parseSymbol = many1 (noneOf ")(\'\" \n")

parseComment :: Parser String
parseComment = 
    do
        c <- char ';'
        ws <- spaces
        xs <- many1 (noneOf "\n")
        return $ [c]++xs

parseString :: Parser String
parseString = 
    do
        oq <- char '\"'
        xs <-  many1 (noneOf "\"")
        cq <- char '\"'
        return $ [oq]++xs++[cq]

{-
This code turns a list of strings with paren separators into a tree
using a simple algebraic datatype TokenTree: Token String | TokenList [TokenTree]
So we end up with a tree of lists of strings
-}

{-
How this works:
-we start with an empty TokenTree tree
- we take a token from the tokenlist and keep tokens
- a "(" means we put the current context on the stack, clear the tree and recurse
- a ")" means we take the context from the stack, add the current subtree to the context and recurse
- any other token means we "unwrap" the current tree, add the token to it and wrap it again

The tricky bit is wrapping/unwrapping of the TokenTree: without it, we get
(TokenList (TokenList TokenTree)+TokenTree), whereas we need (TokenList TokenTree+TokenTree)
-}

{-
To modify this to turn a Scheme let into a Gannet let:
if the previous token was "let" and the token is "("
then drop the paren, i.e. append the next expr to the current tokenlist, not to a new one
i.e use the 'otherwise' rule. The problem is what to do when we encounter the closing ")", or
more precisely how do we know it is the closing ")"? We need to set 'in_let' or something,
but then we might have nested lets. So in_let is a stack as well?
The best way I think is to reparse the TokenTree from tl2tt
i-}
sc2td :: TokenTree -> TokenTree -> TokenTree
sc2td tt ntt
    | (length tl==0) = ntt 
    | isToken t = sc2td tt2 (TokenList (ntl++[t])) 
    | otherwise = let
                (TokenList sl)=sc2td t (TokenList [])
            in
                sc2td tt2 (TokenList (ntl++sl))
            --where
            --(TokenList sl)=sc2td (TokenList t) (TokenList [])
    where
    (TokenList tl) = tt
    t:ts = tl
    tt2=(TokenList ts)
    (TokenList ntl) = ntt

isToken :: TokenTree -> Bool
isToken (Token tstr) = True
isToken (TokenList tl) = False

-- | parse a list of strings into a TokenTree. 
-- TODO: see if use of the State Monad would make this nicer
tl2tt :: [String] -> TokenTree -> [TokenTree] -> TokenTree
tl2tt tokenlist tree stack
    | length tokens == 0    = tree
    | allComments tokens    = tree
    | token=="("            = tl2tt tokens (TokenList ([])) (tree:stack)
    | token==")"             = tl2tt tokens (TokenList (context++[tree])) stack'
--    | token==")" && length tokens > 1           = tl2tt tokens (TokenList (context++[tree])) stack'
--    | token==")" && length tokens == 1 && isComment (head tokens) = tree
    | isComment token        = tl2tt tokens tree stack
    | otherwise         = tl2tt tokens (TokenList (tree'++[Token (parseGToken token tree)])) stack 
    where
        token:tokens = tokenlist
        (TokenList context):stack' = stack        
        (TokenList tree')=tree

isComment :: String -> Bool
isComment x 
    | c == ';' = True
    | otherwise = False
    where
        c:cs=x                
    
allComments :: [String] -> Bool
allComments xs = foldl1 (&&) (map isComment xs)
