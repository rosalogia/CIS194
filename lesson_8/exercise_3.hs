import Data.Char (Char)
import qualified Data.Char as C
import System.Environment
import System.IO
import System.Exit

data Parser a = P (String -> Maybe (a, String))

runParser :: Parser a -> (String -> Maybe (a, String))
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse parser input = unpackValue . (runParser parser) $ input
    where
        unpackValue :: Maybe (a, String) -> Maybe a
        unpackValue Nothing = Nothing
        unpackValue (Just (x, "")) = Just x
        unpackValue (Just (_, _)) = Nothing


noParser :: Parser a
noParser = P (\_ -> Nothing)

pureParser :: a -> Parser a
pureParser input = P (\s -> Just (input, s))

instance Functor Parser where
    -- fmap :: (a -> b) -> (Parser a) -> (Parser b)
    -- fmap f p = applyFunction (runParser p)
    --     where
    --         applyFunction :: (String -> Maybe (a, String)) -> (String -> Maybe (b, String))
    --         applyFunction parser = (\result -> result >>= (\(x, s) ->  Just (f x, s))

    fmap f p = P $ apply (runParser p)
        where
            apply parser string = do
                let maybeB = parser string
                (result, remaining) <- maybeB
                return (f result, remaining)

instance Applicative Parser where
    pure = pureParser
    fp <*> fx = P $ apply (runParser fp) (runParser fx)
        where
            apply fnParser inParser s = do
                let maybeFn = fnParser s
                (functionResult, remainingString) <- maybeFn
                let inputFn = inParser remainingString
                (inputResult, finalString) <- inputFn
                return (functionResult inputResult, finalString)

instance Monad Parser where
    return = pureParser
    fa >>= k = P $ apply (runParser fa)
        where
            apply parser s = do
                let maybeResult = parser s
                (res, rem) <- maybeResult
                let parserB = runParser $ k res
                parserB rem

anyChar :: Parser Char
anyChar = P $ f
    where
        f "" = Nothing
        f (c:cs) = Just (c, cs)

char :: Char -> Parser ()
char c = do
    parsedChar <- anyChar
    if parsedChar == c then return () else noParser

anyCharBut :: Char -> Parser Char
anyCharBut c = do
    parsedChar <- anyChar
    if parsedChar == c then noParser else return parsedChar

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = P $ apply1 (runParser p1)
    where
        apply1 parser s =
            case (parser s) of
                Nothing -> runParser p2 s
                Just _ -> parser s

many :: Parser a -> Parser [a]
many p = do
    res <- p 
    (\a -> res:a) <$> (many p `orElse` return [])
    `orElse`
    return []

many1 :: Parser a -> Parser [a]
many1 p = do
    res <- p 
    (\a -> res:a) <$> (many1 p `orElse` return [])

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p1 p2 = (helper p1 p2 [] True) `orElse` (pureParser [])
    where
        switch :: Bool -> Bool
        switch False = True
        switch True = False

        helper :: Parser x -> Parser y -> [x] -> Bool -> Parser [x]
        helper pc pn vals b = doBlock 
            where doBlock = do
                    res <- if b then (\x -> [x]) <$> pc else (pn >> return [])
                    (helper pc pn (vals ++ res) (switch b)) `orElse` (return (vals ++ res))

parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
        char '"'
        content <- many (anyCharBut '"')
        char '"'
        return content

letterOrDigit :: Parser Char
letterOrDigit = P $ f
    where
        f "" = Nothing
        f (c:cs) = if C.isAlphaNum c then Just (c, cs) else Nothing

type Identifier = String
type Declaration = (Identifier, String)
type Section = (Identifier, [Declaration])
type INIFile = [Section]

parseINI :: Parser INIFile
parseINI = many parseSection
    where
        parseSection = do
            char '['
            identifier <- many (anyCharBut ']')
            char ']'
            char '\n'
            declarations <- (many1 parseDeclaration) `orElse` return []
            (many1 $ char '\n') `orElse` 
            return (identifier, declarations)
            where
                parseDeclaration = do
                    declarationId <- many1 $ letterOrDigit
                    many (char ' ')
                    char '='
                    many (char ' ')
                    declarationContent <- many1 $ anyCharBut '\n'
                    many1 $ char '\n'
                    return (declarationId, declarationContent)
 
main :: IO ()
main = do
    args <- getArgs
    input <- case args of
        [] -> getContents
        [fileName] -> readFile fileName
        _ -> hPutStrLn stderr "Too many arguments given" >> exitFailure
    case parse parseINI input of
        Just i -> print i
        Nothing -> do
            hPutStrLn stderr "Failed to parse INI file."
            exitFailure
