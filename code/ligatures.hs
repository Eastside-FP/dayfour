convertToLigatures:: String -> String

convertToLigatures [] = []
convertToLigatures ('f' : 'f' : 'i' : rest) = 'ﬃ' : convertToLigatures rest 
convertToLigatures ('f' : 'f' : 'l' : rest) = 'ﬄ' : convertToLigatures rest 
convertToLigatures ('f' : 'f' : rest) = 'ﬀ' : convertToLigatures rest 
convertToLigatures ('f' : 'i' : rest) = 'ﬁ' : convertToLigatures rest 
convertToLigatures ('f' : 'l' : rest) = 'ﬂ' : convertToLigatures rest 
convertToLigatures ('i' : 'i' : rest) = 'ĳ' : convertToLigatures rest 
convertToLigatures (other : rest) = other : convertToLigatures rest 

main = do
    putStr $ convertToLigatures "difficult to find fluid wiffle"