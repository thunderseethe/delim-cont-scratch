module CC.CPS where

newtype K ans a = K (a -> ans)
newtype M ans a = M (K ans a -> ans)

instance Functor (M ans) where
    fmap f (M e) = M (\(K k) -> e (K (k . f)))

instance Applicative (M ans) where
    pure v = M (\ (K k) -> k v)
    (M f) <*> (M x) = M (\(K k) -> f (K (\f -> x (K (k . f)))))

instance Monad (M ans) where
    return = pure
    (M e1) >>= e2 = M (\k -> e1 (K (\v1 -> let M c = e2 v1 in c k)))

callcc :: (K ans a -> M ans a) -> M ans a
callcc f = M (\k -> let M c = f k in c k)

abort :: ans -> M ans a
abort a = M (const a)

throw :: K ans a -> M ans a -> M ans b
throw k (M e) = M (const (e k))

c :: (K ans a -> ans) -> M ans a
c f = callcc (abort . f)

runM :: M ans ans -> ans
runM (M e) = e (K id)