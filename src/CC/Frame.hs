{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
module CC.Frame where

import Unsafe.Coerce
import Data.Type.Equality
import Seq
import qualified Prompt

newtype Frame ans a b = Frame (a -> CC ans b)
type Cont ans a = Seq Frame ans a
type SubCont ans a b = SubSeq Frame ans a b

newtype CC ans a = CC (Cont ans a -> Prompt.P ans ans)
unCC (CC e) = e

instance Functor (CC ans) where
  fmap f (CC ak) = CC (\k -> ak (PushSeg (Frame (pure . f)) k))

instance Applicative (CC ans) where
  pure v = CC (\k -> appk k v)
  (CC fk) <*> (CC xk) = undefined
  --liftA2 f (CC xk) (CC yk) = CC (\k -> let a = xk (PushSeg (Frame (pure . f)) k) in _)


instance Monad (CC ans) where
  return = pure
  (CC e1) >>= e2 = CC (\k -> e1 (PushSeg (Frame e2) k))

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
