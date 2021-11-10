{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
module Seq where

import Data.Bifunctor
import Data.Type.Equality
import qualified Prompt

data Seq contseg ans a where
  EmptySeq :: Seq contseq ans ans
  PushP :: Prompt.Prompt ans a -> Seq contseg ans a -> Seq contseg ans a
  PushSeg :: contseg ans a b -> Seq contseg ans b -> Seq contseg ans a

type SubSeq contseg ans a b = Seq contseg ans b -> Seq contseg ans a

pushSeq :: SubSeq contseg ans a b -> Seq contseg ans b -> Seq contseg ans a
pushSeq = ($)

appendSubSeq :: SubSeq contseg ans a b -> SubSeq contseg ans b c -> SubSeq contseg ans a c
appendSubSeq = (.)

splitSeq :: Prompt.Prompt ans b -> Seq contseg ans a -> (SubSeq contseg ans a b, Seq contseg ans b)
splitSeq p = \case
  EmptySeq -> error "Could not find prompt on stack"
  PushP p' sk -> case Prompt.eqPrompt p p' of
                   Just Refl -> (id, sk)
                   Nothing -> let (sub, s) = splitSeq p sk in (appendSubSeq (PushP p') sub, s)
  PushSeg seg sk -> let (sub, s) = splitSeq p sk in (appendSubSeq (PushSeg seg) sub, s)


