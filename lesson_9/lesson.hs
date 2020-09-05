import Data.Char
import Data.Maybe
import Data.List

import System.Environment
import System.IO
import System.Exit

newtype Parser a = P (String -> Maybe (a, String))

runParser :: Parser t -> String -> Maybe (t, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p input = case runParser p input of
    Just (result, "") -> Just result
    _ -> Nothing -- handles both no result and leftover input

noParserP :: Parser a
noParserP = P (\_ -> Nothing)

pureParserP :: a -> Parser a
pureParserP x = P (\input -> Just (x,input))

instance Functor Parser where
    fmap f p = P p'
      where
        p' input = case runParser p input of
            Just (result, rest) -> Just (f result, rest)
            Nothing             -> Nothing

instance Applicative Parser where
    pure = pureParserP
    p1 <*> p2 = P $ \input -> do
        (f, rest1) <- runParser p1 input
        (x, rest2) <- runParser p2 rest1
        return (f x, rest2)


instance Monad Parser where
    return = pure
    p1 >>= k = P $ \input -> do
        (x, rest1) <- runParser p1 input
        runParser (k x) rest1

anyCharP :: Parser Char
anyCharP = P $ \input -> case input of
    (c:rest) -> Just (c, rest)
    []       -> Nothing

charP :: Char -> Parser ()
charP c = do
    c' <- anyCharP
    if c == c' then return ()
               else noParserP

anyCharButP :: Char -> Parser Char
anyCharButP c = do
    c' <- anyCharP
    if c /= c' then return c'
               else noParserP

letterOrDigitP :: Parser Char
letterOrDigitP = do
    c <- anyCharP
    if isAlphaNum c then return c else noParserP

orElseP :: Parser a -> Parser a -> Parser a
orElseP p1 p2 = P $ \input -> case runParser p1 input of
    Just r -> Just r
    Nothing -> runParser p2 input

manyP :: Parser a -> Parser [a]
manyP p = (pure (:) <*> p <*> manyP p) `orElseP` pure []

many1P :: Parser a -> Parser [a]
many1P p = pure (:) <*> p <*> manyP p

sepByP :: Parser a -> Parser () -> Parser [a]
sepByP p1 p2 = ((:) <$> p1 <*> (manyP (p2 *> p1))) `orElseP` pure []

data RHS
    = Terminal String
    | NonTerminal String
    | Choice RHS RHS
    | Sequence RHS RHS
    | Optional RHS
    | Repetition RHS
    deriving (Show, Eq)

mkChoices :: RHS -> [RHS] -> RHS
mkChoices = foldl Choice

mkSequences :: RHS -> [RHS] -> RHS
mkSequences = foldl Sequence

ppRHS :: RHS -> String
ppRHS = handleElement 0
    where
        handleElement _ (Terminal s)        = surround "'" "'" $ concatMap quote s
        handleElement _ (NonTerminal s)     = s
        handleElement a (Choice x1 x2)      = p a 1 $ handleElement 1 x1 ++ " | " ++ handleElement 1 x2
        handleElement a (Sequence x1 x2)    = p a 2 $ handleElement 2 x1 ++ ", " ++ handleElement 2 x2
        handleElement _ (Optional x)        = surround "[" "]" $ handleElement 0 x
        handleElement _ (Repetition x)      = surround "{" "}" $ handleElement 0 x

        surround c1 c2 x = c1 ++ x ++ c2

        p a n   | a > n         = surround "(" ")"
                | otherwise     = id

        quote '\'' = "\\'"
        quote '\\' = "\\\\"
        quote c = [c]

type Production = (String, RHS)
type BNF = [Production]

ppBNF :: BNF -> String
ppBNF = unlines . map (\(i, rhs) -> i ++ " = " ++ ppRHS rhs ++ ";")

newtype Grammar a = G (BNF, RHS)

ppGrammar :: String -> Grammar a -> String
ppGrammar main (G (prods, rhs)) = ppBNF $ prods ++ [(main, rhs)] 

mergeProds :: [Production] -> [Production] -> [Production]
mergeProds prods1 prods2 = nub $ prods1 ++ prods2

anyCharG :: Grammar Char
anyCharG = G ([], NonTerminal "char")

charG :: Char -> Grammar ()
charG c = G ([], Terminal [c])

orElseG :: Grammar a -> Grammar a -> Grammar a
orElseG (G (prods1, rhs1)) (G (prods2, rhs2)) =
    G (mergeProds prods1 prods2, Choice rhs1 rhs2)

instance Functor Grammar where
    fmap _ (G bnf) = G bnf 

instance Applicative Grammar where
    pure x = G ([], Terminal "")
    G (prods1, Terminal "") <*> G (prods2, rhs2)
        = G (mergeProds prods1 prods2, rhs2)
    G (prods1, rhs1) <*> G (prods2, Terminal "")
        = G (mergeProds prods1 prods2, rhs1)
    G (prods1, rhs1) <*> G (prods2, rhs2)
        = G (mergeProds prods1 prods2, Sequence rhs1 rhs2)

manyG :: Grammar a -> Grammar [a]
manyG (G (prods, rhs)) = G (prods, Repetition rhs)

many1G :: Grammar a -> Grammar [a]
many1G p = pure (:) <*> p <*> manyG p

sepByG :: Grammar a -> Grammar () -> Grammar [a]
sepByG p1 p2 = ((:) <$> p1 <*> (manyG (p2 *> p1))) `orElseG` pure []

dottedWordsG :: Grammar [String]
dottedWordsG = many1G (manyG anyCharG <* charG '.')

parseCSVP :: Parser [[String]]
parseCSVP = manyP parseLine
    where
        parseLine = parseCell `sepByP` charP ',' <* charP '\n'
        parseCell = charP '"' *> manyP (anyCharButP '"') <* charP '"'

primitiveG :: String -> Grammar a
primitiveG s = G ([], NonTerminal s)

parseCSVG :: Grammar [[String]]
parseCSVG = manyG parseLine
    where
        parseLine = nonTerminal "line" $
            parseCell `sepByG` charG ',' <* newLineG
        parseCell = nonTerminal "cell" $
            charG '"' *> manyG (primitiveG "not-quote") <* charG '"'

newLineG :: Grammar ()
newLineG = primitiveG "newline"

nonTerminalG :: String -> Grammar a -> Grammar a
nonTerminalG name (G (prods, rhs))
    = G (prods ++ [(name, rhs)], NonTerminal name)

class Applicative f => Descr f where
    char :: Char -> f ()
    many :: f a -> f [a]
    orElse :: f a -> f a -> f a
    primitive :: String -> Parser a -> f a
    nonTerminal :: String -> f a -> f a

instance Descr Parser where
    char = charP
    many = manyP
    orElse = orElseP
    primitive _ p = p
    nonTerminal _ p = p

instance Descr Grammar where
    char = charG
    many = manyG
    orElse = orElseG
    primitive s _ = primitiveG s
    nonTerminal s g = nonTerminalG s g

many1 :: Descr f => f a -> f [a]
many1 p = pure (:) <*> p <*> many p

anyChar :: Descr f => f Char
anyChar = primitive "char" anyCharP

dottedWords :: Descr f => f [String]
dottedWords = many1 (many anyChar <* char '.')

sepBy :: Descr f => f a -> f () -> f [a]
sepBy p1 p2 = ((:) <$> p1 <*> (many (p2 *> p1))) `orElse` pure []

newline :: Descr f => f ()
newline = primitive "newline" (charP '\n')
