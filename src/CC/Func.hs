{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module CC.Func where

import Prompt
import Seq

newtype Cont ans a b = Cont (a -> MC ans b)

unCont :: Cont ans a b -> a -> MC ans b
unCont (Cont k) = k

type MetaCont ans a = Seq Cont ans a
type SubCont ans a b = SubSeq Cont ans a b

newtype CC ans a = CC (forall b. Cont ans a b -> MC ans b)

unCC :: CC ans a -> Cont ans a b -> MC ans b
unCC (CC c) = c

newtype MC ans b = MC (MetaCont ans b -> P ans ans)

unMC :: MC ans b -> MetaCont ans b -> P ans ans
unMC (MC m) = m

newtype ContT m a = ContT (forall b. (a -> m b) -> m b)

instance Functor (CC ans) where
  fmap f (CC e1) = CC (\(Cont k) ->
    let k' = Cont (k . f) in e1 k')

instance Applicative (CC ans) where
  pure e = CC (\(Cont k) -> k e)
  f <*> v = CC (\(Cont k) -> unCC f (Cont (\g -> unCC v (Cont (k . g)))))

instance Monad (CC ans) where
  return e = CC (\ (Cont k) -> k e)
  (CC e1) >>= e2 = CC (\k -> e1 (Cont (\v1 -> unCC (e2 v1) k)))

appseg :: Cont ans a b -> a -> MC ans b
appseg (Cont k) = k

runOne :: CC ans a -> MC ans a
runOne c = unCC c initK
  where
    initK = Cont (\a -> MC (\mk -> appmk mk a))

appmk :: MetaCont ans a -> a -> P ans ans
appmk seq a = case seq of
  EmptySeq -> return a
  PushP _ mk' -> appmk mk' a
  PushSeg k mk' -> unMC (appseg k a) mk'

runTwo :: MC ans ans -> P ans ans
runTwo c = unMC c EmptySeq

runCC :: (forall ans. CC ans a) -> a
runCC = runP . runTwo . runOne

cont :: (forall b . Cont ans a b -> MetaCont ans b -> P ans ans) -> CC ans a
cont f = CC (MC . f)

newPrompt :: CC ans (Prompt ans a)
newPrompt = cont $ \k mk -> do
    p <- newPromptName
    unMC (appseg k p) mk

pushPrompt :: Prompt ans a -> CC ans a -> CC ans a
pushPrompt p e = cont $ \k mk -> unMC (runOne e) (PushP p (PushSeg k mk))

withSubCont :: Prompt ans b -> (SubCont ans a b -> CC ans b) -> CC ans a
withSubCont p f = cont $ \k mk ->
  let
    (subk, mk') = splitSeq p mk
    e = f (appendSubSeq (PushSeg k) subk)
   in unMC (runOne e) mk'

pushSubCont :: SubCont ans a b -> CC ans a -> CC ans b
pushSubCont subk e = cont $ \k mk -> unMC (runOne e) (pushSeq subk (PushSeg k mk))