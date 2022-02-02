{-# OPTIONS_GHC -F -pgmF htfpp #-}
{- HLINT ignore "Use camelCase" -}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} ParserTest
import {-@ HTF_TESTS @-} SemaTest

main = htfMain htf_importedTests
