module VarBindings ( VarManager(NewVarManager) , getBoundV , getFreeV , getSizeV , bindV , freeV ) where

data VarManager = MakeVarM [Int] [Int] Int | NewVarManager

getBoundV :: VarManager -> [Int]
getBoundV (MakeVarM b f max) = b
getBoundV NewVarManager = []

getFreeV :: VarManager -> [Int]
getFreeV (MakeVarM b f max) = f
getFreeV NewVarManager = []

getSizeV :: VarManager -> Int
getSizeV (MakeVarM b f max) = max
getSizeV NewVarManager = 0


bindV :: VarManager -> (Int, VarManager)
bindV (MakeVarM b (f : fs) max) = (f, (MakeVarM (f : b) fs max))
bindV (MakeVarM b [] max) = (max, (MakeVarM (max : b) [] (max + 1)))
bindV NewVarManager = (0, (MakeVarM (0 : []) [] 1))

tryDeleteV :: [Int] -> Int -> ([Int], Bool)
tryDeleteV (x : xs) item | (x == item) = (xs, True)
                         | otherwise = ((x : (fst (tryDeleteV xs item))), (snd (tryDeleteV xs item)))
tryDeleteV [] item = ([], False)

shiftInwardV :: [Int] -> [Int]
shiftInwardV (0 : xs) = (0 : xs)
shiftInwardV (1 : xs) = (1 : xs)
shiftInwardV (2 : xs) = (2 : xs)
shiftInwardV (3 : xs) = (3 : xs)
shiftInwardV (x : (0 : xs)) = (0 : (shiftInwardV (x : xs)))
shiftInwardV (x : (1 : xs)) = (1 : (shiftInwardV (x : xs)))
shiftInwardV (x : (2 : xs)) = (2 : (shiftInwardV (x : xs)))
shiftInwardV (x : (3 : xs)) = (3 : (shiftInwardV (x : xs)))
shiftInwardV (x : xs) = (x : xs)
shiftInwardV [] = []

freeV :: VarManager -> Int -> VarManager
freeV (MakeVarM b f max) x = let bb = tryDeleteV b x
                             in MakeVarM (fst bb) (if (snd bb) then (shiftInwardV (x : f)) else f) max
freeV NewVarManager x = NewVarManager




-- Benjamin Hellstern, Magdalena Sannwald
-- Seminar Compilerbau WS 14/15
-- WSI Informatik, Ernst-Bloch-Universität Tübingen