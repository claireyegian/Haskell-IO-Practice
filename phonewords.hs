{-
    This program will build on dictionary.hs and wordsToPhone from a previous
    assignment. You can copy your wordsToPhone source code here or you can simply
    include the line:
    
    import PTfuncsyntax
    
    and run this program in the same directory with your PFfuncsyntax.hs file.
    
    This program will ask the user to enter a 4-digit number. It will then list 
    off all of the english words that can be formed from that number on a standard 
    telephone keypad.
    
    Example of use:
    
    *Main> main
    Type a four-digit number:
    2376
    "Afro"
    "Bern"
    "berm"
    *Main> 

-}

charToPhoneDigit :: Char -> Int
charToPhoneDigit c
  | c `elem` "ABCabc" = 2
  | c `elem` "DEFdef" = 3
  | c `elem` "GHIghi" = 4
  | c `elem` "JKLjkl" = 5
  | c `elem` "MNOmno" = 6
  | c `elem` "PQRSpqrs" = 7
  | c `elem` "TUVtuv" = 8
  | c `elem` "WXYZwxyz" = 9
  | otherwise = 0

numListToNum :: [Int] -> Int
numListToNum intList = (read :: String -> Int) (numString intList)
numString [] = ""
numString (x:nums) = show x ++ (numString nums)

wordsToPhone :: String -> Int
wordsToPhoneList wordString = [charToPhoneDigit x|x <- wordString, x `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"]
wordsToPhone string = numListToNum (wordsToPhoneList string)

main = do
    putStrLn "Type a four-digit number:"
    num <- readLn
    dictionary <- readFile "/usr/share/dict/american-english"
    mapM_ putStrLn [show x | x <- (lines dictionary), (wordsToPhone x) == num]