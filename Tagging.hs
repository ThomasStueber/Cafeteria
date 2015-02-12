module Tagging ( Tagged , getTag , getTData , TagLookup(NewTagLookup) , insertT , listInsertT , checkTag , toListT , (!%) , (%) , (?%) ) where

type Tagged a = (String, a)

getTag :: (Tagged a) -> String
getTag x = fst x

getTData :: (Tagged a) -> a
getTData x = snd x


data TagLookup a = NewTagLookup                                                       -- Empty Tag Lookup Table
                 | TLBranch (TagLookup a) (Tagged a) Int (Maybe Int) (TagLookup a)    -- Tagged Item


depthOfT :: TagLookup a -> Int

depthOfT(NewTagLookup) = 0
depthOfT(TLBranch l i b (Just d) r) = d
depthOfT(TLBranch l i b Nothing r) = -1


recalcDepthT :: TagLookup a -> TagLookup a

recalcDepthT(TLBranch l i b Nothing r) = TLBranch l i b (Just ((max (depthOfT l) (depthOfT r)) + 1)) r
recalcDepthT(x) = x


balanceOfT :: TagLookup a -> Int

balanceOfT(NewTagLookup) = 0
balanceOfT(TLBranch l i b d r) = b


rotateT :: TagLookup a -> (TagLookup a)

rotateT(TLBranch l i 2 d (TLBranch rl ri 1 (Just rd) rr)) = TLBranch (TLBranch l i 0 (Just (depthOfT(rl) + 1)) rl) ri 0 (Just rd) rr
rotateT(TLBranch l i 2 d (TLBranch rl ri 0 (Just rd) rr)) = TLBranch (TLBranch l i 1 (Just (depthOfT(rl) + 1)) rl) ri (-1) (Just (rd - 1)) rr

rotateT(TLBranch (TLBranch ll li (-1) (Just ld) lr) i (-2) d r) = TLBranch ll li 0 (Just ld) (TLBranch lr i 0 (Just (depthOfT(lr) + 1)) r)
rotateT(TLBranch (TLBranch ll li 0 (Just ld) lr) i (-2) d r) = TLBranch ll li 1 (Just (ld - 1)) (TLBranch lr i (-1) (Just (depthOfT(lr) + 1)) r)

rotateT(TLBranch l i 2 d (TLBranch (TLBranch rll rli 0 rld rlr) ri (-1) rd rr)) = TLBranch (TLBranch l i 0 (Just (depthOfT(rr) + 1)) rll) rli 0 (Just (depthOfT(rr) + 2)) (TLBranch rlr ri 0 (Just (depthOfT(rr) + 1)) rr)
rotateT(TLBranch l i 2 d (TLBranch (TLBranch rll rli 1 rld rlr) ri (-1) rd rr)) = TLBranch (TLBranch l i (-1) (Just (depthOfT(rr) + 1)) rll) rli 0 (Just (depthOfT(rr) + 2)) (TLBranch rlr ri 0 (Just (depthOfT(rr) + 1)) rr)
rotateT(TLBranch l i 2 d (TLBranch (TLBranch rll rli (-1) rld rlr) ri (-1) rd rr)) = TLBranch (TLBranch l i 0 (Just (depthOfT(rr) + 1)) rll) rli 0 (Just (depthOfT(rr) + 2)) (TLBranch rlr ri 1 (Just (depthOfT(rr) + 1)) rr)

rotateT(TLBranch (TLBranch ll li 1 ld (TLBranch lrl lri 0 lrd lrr)) i (-2) d r) = TLBranch (TLBranch ll li 0 (Just (depthOfT(ll) + 1)) lrl) lri 0 (Just (depthOfT(ll) + 2)) (TLBranch lrr i 0 (Just (depthOfT(ll) + 1)) r)
rotateT(TLBranch (TLBranch ll li 1 ld (TLBranch lrl lri (-1) lrd lrr)) i (-2) d r) = TLBranch (TLBranch ll li 0 (Just (depthOfT(ll) + 1)) lrl) lri 0 (Just (depthOfT(ll) + 2)) (TLBranch lrr i 1 (Just (depthOfT(ll) + 1)) r)
rotateT(TLBranch (TLBranch ll li 1 ld (TLBranch lrl lri 1 lrd lrr)) i (-2) d r) = TLBranch (TLBranch ll li (-1) (Just (depthOfT(ll) + 1)) lrl) lri 0 (Just (depthOfT(ll) + 2)) (TLBranch lrr i 0 (Just (depthOfT(ll) + 1)) r)

rotateT(x) = x


updateT :: TagLookup a -> TagLookup a

updateT(TLBranch l i b Nothing r) = rotateT(upB(recalcDepthT(TLBranch (updateT l) i b Nothing (updateT r)))) where upB(TLBranch l i b d r) = TLBranch l i (depthOfT r - depthOfT l) d r
updateT(x) = x


insertT_sub :: TagLookup a -> Tagged a -> TagLookup a

insertT_sub NewTagLookup x = TLBranch NewTagLookup x 0 (Just 1) NewTagLookup
insertT_sub (TLBranch l i b d r) x | ((fst i) == (fst x)) = TLBranch l x b d r
                                  | ((fst i) > (fst x)) = TLBranch (insertT_sub l x) i b Nothing r
                                  | ((fst i) < (fst x)) = TLBranch l i b Nothing (insertT_sub r x)




insertT :: TagLookup a -> Tagged a -> TagLookup a

insertT NewTagLookup x = TLBranch NewTagLookup x 0 (Just 1) NewTagLookup
insertT tl x = updateT(insertT_sub tl x)


(!%) :: TagLookup a -> String -> (a -> TagLookup a)
tl !% x = \ y -> insertT tl (x, y)


(%) :: (a -> TagLookup a) -> a -> TagLookup a
f % x = f x


listInsertT :: TagLookup a -> [Tagged a] -> TagLookup a
listInsertT tl [] = tl
listInsertT tl (x : xs) = listInsertT (insertT tl x) xs


checkTag :: TagLookup a -> String -> Maybe a
checkTag NewTagLookup x = Nothing
checkTag (TLBranch l i b d r) x | ((fst i) == x) = Just (snd i)
                                | ((fst i) > x) = checkTag l x
                                | ((fst i) < x) = checkTag r x


(?%) :: TagLookup a -> String -> Maybe a
tl ?% i = checkTag tl i



toListT :: TagLookup a -> [Tagged a]

toListT NewTagLookup = []
toListT (TLBranch l i b d r) = (toListT l) ++ (i : (toListT r))

instance (Show a) => Show (TagLookup a) where show x = show (toListT x)




-- Benjamin Hellstern, Magdalena Sannwald
-- Seminar Compilerbau WS 14/15
-- WSI Informatik, Ernst-Bloch-Universität Tübingen