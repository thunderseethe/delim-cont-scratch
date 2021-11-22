{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module CC.Control where

import Prompt
import Seq
import CC.CPS

newtype Cont ans a b = Cont (K (MC ans b) a)
unCont :: Cont ans a b -> K (MC ans b) a
unCont (Cont k) = k

type MetaCont ans a = Seq Cont ans a
type SubCont ans a b = SubSeq Cont ans a b

newtype CC ans a = CC (forall b. M (MC ans b) a)
unCC :: CC ans a -> M (MC ans b) a
unCC (CC c) = c

instance Functor (CC ans) where
    fmap f (CC c) = CC (fmap f c)

instance Applicative (CC ans) where
    pure v = CC (pure v)
    (CC f) <*> (CC x) = CC (f <*> x)

instance Monad (CC ans) where
    return = pure
    (CC e) >>= f = CC (do v1 <- e; unCC (f v1))

newtype MC ans b = MC (MetaCont ans b -> P ans ans)
unMC :: MC ans b -> MetaCont ans b -> P ans ans
unMC (MC m) = m

appseg :: Cont ans a b -> a -> MC ans b
appseg (Cont k) a = runM (throw k (return a))

runOne :: CC ans a -> MC ans a
runOne m = runM (do a <- unCC m; initkF a)
    where
        initkF a = return (MC (\mk -> appmk mk a))

appmk :: MetaCont ans a -> a -> P ans ans
appmk EmptySeq a = return a
appmk (PushP _ mk') a = appmk mk' a
appmk (PushSeg k mk') a = unMC (appseg k a) mk'

runTwo :: MC ans ans -> P ans ans
runTwo c = unMC c EmptySeq

runCC :: (forall ans. CC ans a) -> a
runCC = runP . runTwo. runOne

cont :: (forall b. K (MC ans b) a -> MetaCont ans b -> P ans ans) -> CC ans a
cont f = CC (c (\k -> MC (\mk -> f k mk)))

newPrompt :: CC ans (Prompt ans a)
newPrompt = cont $ \k mk -> do 
    p <- newPromptName
    unMC (appseg (Cont k) p) mk

pushPrompt :: Prompt ans a -> CC ans a -> CC ans a
pushPrompt p e = cont $ \k -> unMC (runOne e) . PushP p . PushSeg (Cont k)

withSubCont :: Prompt ans b -> (SubCont ans a b -> CC ans b) -> CC ans a
withSubCont p f = cont $ \k mk ->
    let (subk, mk') = splitSeq p mk
        e = f (appendSubSeq (PushSeg (Cont k)) subk)
     in unMC (runOne e) mk'

pushSubCont :: SubCont ans a b -> CC ans a -> CC ans b
pushSubCont subk e = cont $ \k mk ->
    unMC (runOne e) (pushSeq subk (PushSeg (Cont k) mk))