module TupleSort where
       
gt :: (a -> a -> Ordering) -> a -> a -> Bool
gt cmp a b = a `cmp` b == GT 

lt :: (a -> a -> Ordering) -> a -> a -> Bool
lt cmp a b = a `cmp` b == LT 


gte :: (a -> a -> Ordering) -> a -> a -> Bool
gte cmp a b = c == GT || c == EQ  
  where
    c = cmp a b
    
tuple5SortBy :: (a -> a -> Ordering) -> (a,a,a,a,a) -> (a,a,a,a,a)
tuple5SortBy cmp = step4 . step3 . step2 . step1
  where
    step1 (a,b,c,d,e) | gt cmp a b = (b,a,c,d,e)
                      | otherwise = (a,b,c,d,e)
                                    
    step2 (a,b,c,d,e) | gt cmp c d = (a,b,d,c,e)
                      | otherwise = (a,b,c,d,e)    
                                    
    step3 (a,b,c,d,e) | gt cmp b d = (c,d,a,b,e)
                      | otherwise = (a,b,c,d,e)
                                
    step4 (a,b,c,d,e) | gte cmp e b = step5 (a,b,c,d,e) -- e >= b
                      | otherwise = step6 (a,b,c,d,e) -- e < b
                                
    step5 (a,b,c,d,e) | lt cmp e d = step7 (a,b,c,d,e) -- e < d
                      | otherwise = step8 (a,b,c,d,e) -- e >= d
                                
    step6 (a,b,c,d,e) | lt cmp e a = step12 (a,b,c,d,e) 
                      | otherwise = step13 (a,b,c,d,e)
                                
    step7 (a,b,c,d,e) | gte cmp c b = step9 (a,b,c,d,e) -- complete
                      | otherwise = step10 (a,b,c,d,e) -- complete
                                
    step8 (a,b,c,d,e) | gte cmp c b = (a,b,c,d,e) -- complete
                      | otherwise = step11 (a,b,c,d,e) -- complete
                                
    step9 (a,b,c,d,e) | lt cmp c e = (a,b,c,e,d) -- complete
                      | otherwise = (a,b,e,c,d) -- complete
                                
    step10 (a,b,c,d,e) | lt cmp c a = (c,a,b,e,d) -- complete
                       | otherwise = (a,c,b,e,d) -- complete
                                 
    step11 (a,b,c,d,e) | lt cmp c a = (c,a,b,d,e) -- Complete
                       | otherwise = (a,c,b,d,e) -- Complete
                                 
    step12 (a,b,c,d,e) | gte cmp c a = step14 (a,b,c,d,e) -- complete
                       | otherwise = step15 (a,b,c,d,e) -- complete
                                 
    step13 (a,b,c,d,e) | gte cmp c e = step16 (a,b,c,d,e)
                       | otherwise = step17 (a,b,c,d,e)
                                 
    step14 (a,b,c,d,e) | lt cmp c b = (e,a,c,b,d) -- complete
                       | otherwise = (e,a,b,c,d) -- complete
                                 
    step15 (a,b,c,d,e) | lt cmp c e = (c,e,a,b,d) -- complete
                       | otherwise = (e,c,a,b,d) -- complete
                                 
    step16 (a,b,c,d,e) | lt cmp c b = (a,e,c,b,d)
                       | otherwise = (a,e,b,c,d)
                                 
    step17 (a,b,c,d,e) | lt cmp c a = (c,a,e,b,d)
                       | otherwise = (a,c,e,b,d)
                                 
                                
                                
                                
