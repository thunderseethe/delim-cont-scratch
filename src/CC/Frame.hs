{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module CC.Frame where

import Unsafe.Coerce
import Data.Type.Equality
import Seq
import Prompt

newtype Frame ans a b = Frame (a -> CC ans b)
type Cont ans a = Seq Frame ans a
type SubCont ans a b = SubSeq Frame ans a b

newtype CC ans a = CC (Cont ans a -> P ans ans)

unCC :: CC ans a -> Cont ans a -> P ans ans
unCC (CC e) = e

instance Functor (CC ans) where
  fmap f (CC ak) = CC (ak . PushSeg (Frame (pure . f)))

instance Applicative (CC ans) where
  pure v = CC (\k -> appk k v)
  (CC fk) <*> x = CC (fk . PushSeg (Frame (\g -> fmap g x)))

instance Monad (CC ans) where
  return = pure
  (CC e1) >>= e2 = CC (e1 . PushSeg (Frame e2))

appseg :: Frame ans a b -> a -> CC ans b
appseg (Frame fr) = fr

appk :: Cont ans a -> a -> Prompt.P ans ans
appk EmptySeq v = return v
appk (PushP _ k) v = appk k v
appk (PushSeg seg k) v = unCC (appseg seg v) k

runTerm :: CC ans a -> Prompt.P ans ans
runTerm c = unCC c (unsafeCoerce EmptySeq)

runCC :: (forall ans. CC ans a) -> a
runCC ce = Prompt.runP (runTerm ce)

newPrompt :: CC ans (Prompt ans a)
newPrompt = CC (\k -> do p <- newPromptName; appk k p)

pushPrompt :: Prompt ans a -> CC ans a -> CC ans a
pushPrompt p (CC e) = CC (e . PushP p)

withSubCont :: Prompt ans b -> (SubCont ans a b -> CC ans b) -> CC ans a
withSubCont p f = CC (\k -> 
  let (subk, k') = splitSeq p k
    in unCC (f subk) k')

pushSubCont :: SubCont ans a b -> CC ans a -> CC ans b
pushSubCont subk (CC e) = CC (e  . pushSeq subk)
