{-# LANGUAGE RankNTypes #-}
module CC.Func where

import Prompt
import Seq

newtype Cont ans a b = Cont (a -> MC ans b)
unCont (Cont k) = k

type MetaCont ans a = Seq Cont ans a
type SubCont ans a b = SubSeq Cont ans a b

newtype CC ans a = CC (forall b. Cont ans a b -> MC ans b)
unCC (CC c) = c

newtype MC ans b = MC (MetaCont ans b -> P ans ans)
unMC (MC m) = m

newtype ContT m a = ContT (forall b. (a -> m b) -> m b)

instance Functor (CC ans) where
  fmap f (CC e1) = CC (\(Cont k) -> 
    let k' = Cont (k . f) in e1 k')


  {-instance Monad (CC ans) where
  return e = CC (\ (Cont k) -> k e)
  (CC e1) >>= e2 = CC (\k -> e1 (Cont (\v1 -> unCC (e2 v1) k)))-}
