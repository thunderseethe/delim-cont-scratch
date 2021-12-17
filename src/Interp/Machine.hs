module Interp.Machine where

newtype Prompt = Prompt Int

data Ins 
    = NewPrompt

data Machine = Machine
    { machCode :: [Ins]
    , machStack :: [Int]
    , machHeap :: [Int]
    , machPrompts :: [Prompt]
    }
