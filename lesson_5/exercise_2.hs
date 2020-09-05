import qualified Data.List as L

adjacents :: [a] -> [(a,a)]
adjacents [] = []
adjacents [x] = []
adjacents (x1:x2:xs) = (x1,x2) : (adjacents (x2:xs))

similar :: Eq a => (a,a) -> Bool
similar (a, b) = a == b

longestText :: Show a => [a] -> a
longestText xs = L.maximumBy (\a b -> compare (displayLength a) (displayLength b)) xs
    where displayLength = length . show

numberOfLines :: String -> String
numberOfLines s = "Number of lines: " ++ (show . length . lines $ s)

numberOfEmpties :: String -> String
numberOfEmpties s = "Number of empty lines: " ++ (show $ 1 + (length $ (filter (\(a,b) -> (a,b) == ('\n','\n')) $ adjacents s)))

numberOfWords :: String -> String
numberOfWords s = "Number of words: " ++ (show . length . words $ s)

numberOfUniqueWords :: String -> String
numberOfUniqueWords s = "Number of unique words: " ++ (show $ (length . L.nub . words $ s))

numberOfAdjacents :: String -> String
numberOfAdjacents s = "Number of words followed by themselves: " ++ (show $ (length . filter similar . adjacents . words $ s))

lengthOfLongestLine :: String -> String
lengthOfLongestLine s = "Length of the longest line: " ++ (show $ length . longestText . lines $ s)

wordCount :: String -> String
wordCount s = L.unlines entries
    where
        entries = [ numberOfLines s
                    , numberOfEmpties s
                    , numberOfWords s
                    , numberOfUniqueWords s
                    , numberOfAdjacents s
                    , lengthOfLongestLine s ]

main = interact wordCount 
