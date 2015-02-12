module Indexing ( Indexed , getIndex , getData , IndexLookup(NewIndexLookup) , insert , listInsert , checkIndex , toList , (!#) , (#) , (?#) ) where

-- import System.Random -- ! for testing purposes only !
-- import System.Random.Shuffle -- ! for testing purposes only !

type Indexed a = (Int, a)

getIndex :: (Indexed a) -> Int
getIndex x = fst x

getData :: (Indexed a) -> a
getData x = snd x


data IndexLookup a = NewIndexLookup                                                         -- Empty Index Lookup Table
                   | ILBranch (IndexLookup a) (Indexed a) Int (Maybe Int) (IndexLookup a)   -- Indexed Item


depthOf :: IndexLookup a -> Int

depthOf(NewIndexLookup) = 0
depthOf(ILBranch l i b (Just d) r) = d
depthOf(ILBranch l i b Nothing r) = -1


recalcDepth :: IndexLookup a -> IndexLookup a

recalcDepth(ILBranch l i b Nothing r) = ILBranch l i b (Just ((max (depthOf l) (depthOf r)) + 1)) r
recalcDepth(x) = x


balanceOf :: IndexLookup a -> Int

balanceOf(NewIndexLookup) = 0
balanceOf(ILBranch l i b d r) = b


rotate :: IndexLookup a -> (IndexLookup a)

rotate(ILBranch l i 2 d (ILBranch rl ri 1 (Just rd) rr)) = ILBranch (ILBranch l i 0 (Just (depthOf(rl) + 1)) rl) ri 0 (Just rd) rr
rotate(ILBranch l i 2 d (ILBranch rl ri 0 (Just rd) rr)) = ILBranch (ILBranch l i 1 (Just (depthOf(rl) + 1)) rl) ri (-1) (Just (rd - 1)) rr

rotate(ILBranch (ILBranch ll li (-1) (Just ld) lr) i (-2) d r) = ILBranch ll li 0 (Just ld) (ILBranch lr i 0 (Just (depthOf(lr) + 1)) r)
rotate(ILBranch (ILBranch ll li 0 (Just ld) lr) i (-2) d r) = ILBranch ll li 1 (Just (ld - 1)) (ILBranch lr i (-1) (Just (depthOf(lr) + 1)) r)

rotate(ILBranch l i 2 d (ILBranch (ILBranch rll rli 0 rld rlr) ri (-1) rd rr)) = ILBranch (ILBranch l i 0 (Just (depthOf(rr) + 1)) rll) rli 0 (Just (depthOf(rr) + 2)) (ILBranch rlr ri 0 (Just (depthOf(rr) + 1)) rr)
rotate(ILBranch l i 2 d (ILBranch (ILBranch rll rli 1 rld rlr) ri (-1) rd rr)) = ILBranch (ILBranch l i (-1) (Just (depthOf(rr) + 1)) rll) rli 0 (Just (depthOf(rr) + 2)) (ILBranch rlr ri 0 (Just (depthOf(rr) + 1)) rr)
rotate(ILBranch l i 2 d (ILBranch (ILBranch rll rli (-1) rld rlr) ri (-1) rd rr)) = ILBranch (ILBranch l i 0 (Just (depthOf(rr) + 1)) rll) rli 0 (Just (depthOf(rr) + 2)) (ILBranch rlr ri 1 (Just (depthOf(rr) + 1)) rr)

rotate(ILBranch (ILBranch ll li 1 ld (ILBranch lrl lri 0 lrd lrr)) i (-2) d r) = ILBranch (ILBranch ll li 0 (Just (depthOf(ll) + 1)) lrl) lri 0 (Just (depthOf(ll) + 2)) (ILBranch lrr i 0 (Just (depthOf(ll) + 1)) r)
rotate(ILBranch (ILBranch ll li 1 ld (ILBranch lrl lri (-1) lrd lrr)) i (-2) d r) = ILBranch (ILBranch ll li 0 (Just (depthOf(ll) + 1)) lrl) lri 0 (Just (depthOf(ll) + 2)) (ILBranch lrr i 1 (Just (depthOf(ll) + 1)) r)
rotate(ILBranch (ILBranch ll li 1 ld (ILBranch lrl lri 1 lrd lrr)) i (-2) d r) = ILBranch (ILBranch ll li (-1) (Just (depthOf(ll) + 1)) lrl) lri 0 (Just (depthOf(ll) + 2)) (ILBranch lrr i 0 (Just (depthOf(ll) + 1)) r)

rotate(x) = x


update :: IndexLookup a -> IndexLookup a

update(ILBranch l i b Nothing r) = rotate(upB(recalcDepth(ILBranch (update l) i b Nothing (update r)))) where upB(ILBranch l i b d r) = ILBranch l i (depthOf r - depthOf l) d r
update(x) = x


insert_sub :: IndexLookup a -> Indexed a -> IndexLookup a

insert_sub NewIndexLookup x = ILBranch NewIndexLookup x 0 (Just 1) NewIndexLookup
insert_sub (ILBranch l i b d r) x | ((fst i) == (fst x)) = ILBranch l x b d r
                                  | ((fst i) > (fst x)) = ILBranch (insert_sub l x) i b Nothing r
                                  | ((fst i) < (fst x)) = ILBranch l i b Nothing (insert_sub r x)




insert :: IndexLookup a -> Indexed a -> IndexLookup a

insert NewIndexLookup x = ILBranch NewIndexLookup x 0 (Just 1) NewIndexLookup
insert il x = update(insert_sub il x)


(!#) :: IndexLookup a -> Int -> (a -> IndexLookup a)
il !# x = \ y -> insert il (x, y)


(#) :: (a -> IndexLookup a) -> a -> IndexLookup a
f # x = f x


listInsert :: IndexLookup a -> [Indexed a] -> IndexLookup a
listInsert il [] = il
listInsert il (x : xs) = listInsert (insert il x) xs


checkIndex :: IndexLookup a -> Int -> Maybe a
checkIndex NewIndexLookup x = Nothing
checkIndex (ILBranch l i b d r) x | ((fst i) == x) = Just (snd i)
                                  | ((fst i) > x) = checkIndex l x
                                  | ((fst i) < x) = checkIndex r x


(?#) :: IndexLookup a -> Int -> Maybe a
il ?# i = checkIndex il i



toList :: IndexLookup a -> [Indexed a]

toList NewIndexLookup = []
toList (ILBranch l i b d r) = (toList l) ++ (i : (toList r))

instance (Show a) => Show (IndexLookup a) where show x = show (toList x)


-- ! for testing purposes only !
-- uncomment all for testing

-- testInsert :: IndexLookup String -> Int -> Int -> IndexLookup String
-- testInsert il min max | (min < max) = testInsert (insert il (min, "")) (min + 1) max
--                       | otherwise = il

-- testDepth :: IndexLookup a -> Int
-- testDepth NewIndexLookup = 0
-- testDepth (ILBranch l i b d r) = (max (testDepth l) (testDepth r)) + 1

-- testBalance :: IndexLookup a -> Bool
-- testBalance NewIndexLookup = True
-- testBalance (ILBranch l i b d r) = (b < 2) && (b > (-2)) && (testBalance l) && (testBalance r)

-- randomList :: Int -> [(Int, String)]
-- randomList m = shuffle' (newlist (m - 1)) (m) (mkStdGen 1) where newlist 0 = (0, "") : []
--                                                                  newlist n = (n, "") : (newlist (n - 1))

-- testRandom :: IndexLookup String -> Int -> IndexLookup String
-- testRandom il max = listInsert il (randomList max)

-- checkOrder :: [Indexed a] -> Bool
-- checkOrder [] = True
-- checkOrder (x : []) = True
-- checkOrder ((x, xx) : ((y, yy) : tail)) | (x > y) = False
--                                         | otherwise = checkOrder ((y, yy) : tail)


-- ! for testing purposes only !


-- Tests for AVL-Tree-Structure of IndexLookup :

-- testBalance (testRandom NewIndexLookup 10000)
-- checkOrder (toList (testRandom NewIndexLookup 10000))
-- testDepth (testRandom NewIndexLookup 10000)
-- toList (testRandom NewIndexLookup 10000)
-- testBalance (testInsert NewIndexLookup 0 10000)
-- checkOrder (toList (testInsert NewIndexLookup 0 10000))
-- testDepth (testInsert NewIndexLookup 0 10000)
-- toList (testInsert NewIndexLookup 0 10000)
-- toList (testInsert (NewIndexLookup # 987654 "test") 0 1000)




-- Benjamin Hellstern, Magdalena Sannwald
-- Seminar Compilerbau WS 14/15
-- WSI Informatik, Ernst-Bloch-Universität Tübingen