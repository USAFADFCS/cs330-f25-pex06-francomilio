-- pex6.hs 
-- unKnot Haskell

-- name: Franco Milio

{- DOCUMENTATION:
Used class notes and google for knowledge on Haskell functions
Used test cases designed by other cadet shared in teams
-}
unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "unknot"
-- | typeIExists tripCode = show (typeIMove [] tripCode)
   | typeIIExists tripCode = show (typeIIMove [] tripCode)
   | typeIIWrapExists tripCode = show (typeIIWrapMove [] tripCode)
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

typeIExists :: [(Char, Char)] -> Bool
typeIExists list 
   | length list <= 1 = False
   | fst(head list) == fst(head(tail list)) = True
   | otherwise = typeIExists (tail list)

typeIMove :: [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
typeIMove tripNew tripCode
   | length tripCode <= 1 = tripNew
   | fst(head tripCode) == fst(head(tail tripCode)) = tripNew ++ tail(tail tripCode)
   | otherwise = typeIMove (tripNew ++ [head tripCode]) (tail tripCode)

typeIIExists :: [(Char, Char)] -> Bool
typeIIExists list
   | length list <= 4 = False
   | snd(head list) == snd(head(tail list)) = typeIIExistsHelper (tail (tail list)) (snd(head list)) (fst(head list)) (fst(head(tail list)))
   | otherwise = typeIIExists (tail list)

typeIIExistsHelper :: [(Char, Char)] -> Char -> Char -> Char -> Bool
typeIIExistsHelper list ou letO letT
   | length list <= 1 = False
   | (snd(head list) /= ou) && (snd(head list) == snd(head(tail list)) && ((fst(head list) == letO) && (fst(head(tail list)) == letT))) = True
   | otherwise = typeIIExistsHelper (tail list) ou letO letT

typeIIMove :: [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
typeIIMove tripNew tripCode
   | length tripCode <= 4 = tripNew
   | snd(head tripCode) == snd(head(tail tripCode)) = tripNew ++ typeIIMoveHelper [] (tail (tail tripCode)) (snd(head tripCode)) (fst(head tripCode)) (fst(head(tail tripCode)))
   | otherwise = typeIIMove (tripNew ++ [head tripCode]) (tail tripCode)

typeIIMoveHelper :: [(Char, Char)] -> [(Char, Char)] -> Char -> Char -> Char -> [(Char, Char)]
typeIIMoveHelper listNew tripCode ou letO letT
   | length tripCode <= 2 = listNew 
   | (snd(head tripCode) /= ou) && (snd(head tripCode) == snd(head(tail tripCode)) && ((fst(head tripCode) == letO) && (fst(head(tail tripCode)) == letT))) = listNew ++ tail (tail tripCode)
   | otherwise = typeIIMoveHelper (listNew ++ [head tripCode]) (tail tripCode) ou letO letT

typeIIWrapExists :: [(Char, Char)] -> Bool
typeIIWrapExists list
   | length list <= 2 = False
   | snd(head list) == snd(last list) = typeIIExistsWrapHelper (tail list) (snd(head list)) (fst(head list)) (fst(last list))
   | otherwise = typeIIWrapExists (tail list)

typeIIExistsWrapHelper :: [(Char, Char)] -> Char -> Char -> Char -> Bool
typeIIExistsWrapHelper list ou letO letT
   | length list <= 2 = False
   | (snd(head list) /= ou) && (snd(head list) == snd(head(tail list)) && ((fst(head list) == letO) && (fst(head(tail list)) == letT))) = True
   | otherwise = typeIIExistsHelper (tail list) ou letO letT

typeIIWrapMove:: [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
typeIIWrapMove tripNew tripCode
   | length tripCode <= 4 = allButLast [] tripNew
   | snd(head tripCode) == snd(last tripCode) = tripNew ++ typeIIWrapMoveHelper [] (tail tripCode) (snd(head tripCode)) (fst(head tripCode)) (fst(last tripCode))
   | otherwise = typeIIWrapMove (tripNew ++ [head tripCode]) (tail tripCode)

typeIIWrapMoveHelper :: [(Char, Char)] -> [(Char, Char)] -> Char -> Char -> Char -> [(Char, Char)]
typeIIWrapMoveHelper listNew tripCode ou letO letT
   | length tripCode <= 2 = listNew 
   | (snd(head tripCode) /= ou) && (snd(head tripCode) == snd(head(tail tripCode)) && ((fst(head tripCode) == letO) && (fst(head(tail tripCode)) == letT))) = listNew ++ tail (tail tripCode)
   | otherwise = typeIIMoveHelper (listNew ++ [head tripCode]) (tail tripCode) ou letO letT

allButLast :: [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
allButLast listNew tripCode
   | length tripCode <= 2 = listNew
   | otherwise = allButLast (listNew ++ [head tripCode]) (tail tripCode)

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)
   let t05 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('a','u'),('b','o'),('c','o'),('e','o'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
   print("   test case t05 - tripcode: " )
   print(t05)
   print("   result:" ++ unKnot t05)
   let t06 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('a','u'),('b','u'),('b','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('b','o')]
   print("   test case t05 - tripcode: " )
   print(t06)
   print("   result:" ++ unKnot t06)  

