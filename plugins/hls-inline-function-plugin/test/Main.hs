{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Ide.Plugin.InlineFunction as InlineFunction
import           Test.Hls

main :: IO ()
main = defaultTestRunner test

plugin :: PluginTestDescriptor InlineFunction.Log
plugin = mkPluginTestDescriptor InlineFunction.descriptor "inline-function"

test :: TestTree
test = testGroup "inline-function" []
