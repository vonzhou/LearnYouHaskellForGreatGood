main = interact shortLinesOnly 

shortLinesOnly :: String -> String 
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines 
