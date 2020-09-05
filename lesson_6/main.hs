main =  putStrLn "Please enter a number: " >>
        readLn >>= \n ->
        let m = n + 1 in
        putStrLn (show m)
