import Data.Maybe (Maybe)
import qualified Data.Maybe as M
import Data.Char (Char)
import qualified Data.Char as C
import qualified Data.List as L

halveEvens :: [Integer] -> [Integer]
halveEvens [] = []
halveEvens xs = M.mapMaybe halve . filter evens $ xs
    where
        halve :: Integer -> Maybe Integer
        halve x = x `dividedBy` 2
            where
                dividedBy :: Integer -> Integer -> Maybe Integer
                dividedBy num1 num2
                    | num1 `mod` num2 == 0  = Just (floor ((fromIntegral num1) / (fromIntegral num2)))
                    | otherwise             = Nothing

        evens :: Integer -> Bool
        evens x = (x `mod` 2) == 0

safeString :: String -> String
safeString s = map replaceInvalid s
    where
        replaceInvalid :: Char -> Char
        replaceInvalid c
            | C.isControl c     = '_'
            | not (C.isAscii c) = '_'
            | otherwise         = c

holes :: [a] -> [[a]]
holes [] = []
holes l = holesIn (length l) l
    where
        holesIn :: Int -> [a] -> [[a]]
        holesIn 0 _ = []
        holesIn n xs = (removeNth 0 ((length xs) - n) xs) : (holesIn (n - 1) xs)
            where
                removeNth :: Int -> Int -> [a] -> [a]
                removeNth counter index (x:xs)
                    | index > (length l)    = (x:xs)
                    | counter == index      = xs
                    | otherwise             = x : (removeNth (counter+1) index xs)

longestText :: Show a => [a] -> a
longestText xs = L.maximumBy (\a b -> compare (displayLength a) (displayLength b)) xs
    where displayLength = length . show

adjacents :: [a] -> [(a,a)]
adjacents [] = []
adjacents [x] = []
adjacents (x1:x2:xs) = (x1,x2) : (adjacents (x2:xs))

commas :: [String] -> String
commas [] = ""
commas [s] = s
commas (s1:ss) = s1 ++ ", " ++ (commas ss)

addPolynomials :: [[Integer]] -> [Integer]
addPolynomials [[]] = []
addPolynomials [xs] = xs
addPolynomials polynomials = addPolys (length polynomials)
    where
        addPolys :: Int -> [Integer]
        addPolys 0 = []
        addPolys ind = (nthSum ((length polynomials) - ind) polynomials) : (addPolys (ind-1))

        nthSum :: Int -> [[Integer]] -> Integer
        nthSum n l = sum (nthValues n l)

        nthValues :: Int -> [[a]] -> [a]
        nthValues n [] = []
        nthValues n [x] = [getNth n x]
        nthValues n (xs:xss) = ((getNth n xs) : (nthValues n xss))

        getNth :: Int -> [a] -> a
        getNth n l = l !! n

sumNumbers :: String -> Integer
sumNumbers [] = 0
sumNumbers s = sum $ map read $ words . map convertNonDigit $ s
    where
        convertNonDigit :: Char -> Char
        convertNonDigit c
            | C.isDigit c = c
            | otherwise = ' '

ex_halveEvens =
    [ halveEvens [] == []
    , halveEvens [1,2,3,4,5] == [1,2]
    , halveEvens [6,6,6,3,3,3,2,2,2] == [3,3,3,1,1,1]
    ]

ex_safeString =
    [ safeString [] == []
    , safeString "Hello World!" == "Hello World!"
    , safeString "That’s your line:\n" == "That_s your line:_"
    , safeString "🙋.o(“Me Me Me”)" == "_.o(_Me Me Me_)"
    ]

ex_holes =
   [ holes "" == []
   , holes "Hello" == ["ello", "Hllo", "Helo", "Helo", "Hell"]
   ]

ex_longestText =
   [ longestText [True,False] == False
   , longestText [2,4,16,32] == (32::Int)
   , longestText (words "Hello World") == "World"
   , longestText (words "Olá mundo") ==  "Olá" ]

ex_adjacents =
   [ adjacents "" == []
   , adjacents [True] == []
   , adjacents "Hello" == [('H','e'),('e','l'),('l','l'),('l','o')]
   ]

ex_commas =
   [ commas [] == ""
   , commas ["Hello"] == "Hello"
   , commas ["Hello", "World"] == "Hello, World"
   , commas ["Hello", "", "World"] == "Hello, , World"
   , commas ["Hello", "new", "World"] == "Hello, new, World"
   ]

ex_addPolynomials =
   [ addPolynomials [[]] == []
   , addPolynomials [[0, 1], [1, 1]] == [1, 2]
   , addPolynomials [[0, 1, 5], [7, 0, 0], [-2, -1, 5]] == [5, 0, 10]
   ]

ex_sumNumbers =
   [ sumNumbers "" == 0
   , sumNumbers "Hello world!" == 0
   , sumNumbers "a1bc222d3f44" == 270
   , sumNumbers "words0are1234separated12by3integers45678" == 46927
   , sumNumbers "000a." == 0
   , sumNumbers "0.00a." == 0
   ]



testResults :: [(String, [Bool])]
testResults = [ ("halveEvens",      ex_halveEvens)
              , ("safeString",      ex_safeString)
              , ("holes",           ex_holes)
              , ("longestText",     ex_longestText)
              , ("adjacents",       ex_adjacents)
              , ("commas",          ex_commas)
              , ("addPolynomials",  ex_addPolynomials)
              , ("sumNumbers",      ex_sumNumbers)
              ]

unpackResults :: [Bool] -> (Int, Int)
unpackResults results = ((length . filter id $ results), (length . filter (not . id) $ results))

formatTests :: [(String, [Bool])] -> String
formatTests results = L.unlines $ map showResult results
    where
        showResult :: (String, [Bool]) -> String
        showResult (fnName, outcomes) = fnName ++ ": " ++ (show passed) ++ "/" ++ (show $ length outcomes) ++ " tests succeeded"
            where (passed,_) = unpackResults outcomes

main = putStrLn $ formatTests testResults
