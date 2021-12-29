{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Prompt(P, Prompt(..), runP, newPromptName, eqPrompt) where

import Data.Bifunctor
import Data.Type.Equality
import Unsafe.Coerce

newtype P ans a = P (Int -> (Int, a))
newtype Prompt ans a = Prompt Int

unP (P f) = f

instance Functor (P ans) where
  fmap f (P action) = P (second f . action)

instance Applicative (P ans) where
  pure a = P (\s -> (s, a))
  (P pf) <*> (P px) = P (\s0 ->
    let
      (s1, f) = pf s0
      (s2, x) = px s1
     in (s2, f x))

instance Monad (P ans) where
  return = pure
  (P x) >>= f = P (\s0 ->
    let
      (s1, a) = x s0
      P b = f a
    in b s1)

runP :: P ans ans -> ans
runP p = snd (unP p 0)

newPromptName :: P ans (Prompt ans a)
newPromptName = P (\np -> (np+1, Prompt np))

eqPrompt :: Prompt r a -> Prompt r b -> Maybe (a :~: b)
eqPrompt (Prompt p1) (Prompt p2)
  | p1 == p2 = Just (unsafeCoerce Refl)
  | otherwise = Nothing
