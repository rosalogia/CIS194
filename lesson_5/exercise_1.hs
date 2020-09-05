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
