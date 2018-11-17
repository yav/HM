{-# Language TemplateHaskell #-}
module HMPanic (panic) where

import Panic hiding (panic)
import qualified Panic

data HM = HM

instance PanicComponent HM where
  panicComponentName _     = "HM"
  panicComponentIssues _   = "https://github.com/yav/HM"
  panicComponentRevision   = $useGitRevision

panic :: String -> [String] -> a
panic = Panic.panic HM

