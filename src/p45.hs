import Data.List

triangle =   map (truncate . (\n -> n * (n+1) / 2)) [1 ..]
pentagonal = map (truncate . (\n -> n * (3*n-1) / 2)) [1 ..]
hexagonal =  map (truncate . (\n -> n * (2*n-1))) [1 ..]

common tss@(t:ts) pss@(p:ps) hss@(h:hs)
  | t > p = common tss ps hss
  | t > h = common tss pss hs
  | p > t = common ts pss hss
  | p > h = common tss pss hs
  | h > t = common ts pss hss
  | h > p = common tss ps hss
  | otherwise = t : common ts ps hs
