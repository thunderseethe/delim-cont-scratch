module Lib where

import Prompt
import Seq
import CC.Control

someFunc :: IO ()
someFunc = putStrLn "some string"

productM :: [Int] -> Int
productM xs = runCC $ do
    p <- newPrompt
    pushPrompt p (loop xs p)
  where
    loop [] p = return 1
    loop (0:_) p = withSubCont p (const (return 0))
    loop (x:xs) p = do r <- loop xs p; return (x*r)
