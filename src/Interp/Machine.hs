module Interp.Machine where

newtype Prompt = Prompt Int

data Ins 
    = NewPrompt

data Machine = Machine
    { code :: [Ins]
    , stack :: [Int]
    , heap :: [Int]
    , prompts :: [Prompt]
    }