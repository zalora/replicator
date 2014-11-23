{-# LANGUAGE TemplateHaskell #-}
module Replicator.Flags where

import HFlags (defineFlag)

defineFlag "f:force" False "Force action, e. g. overwrite dumps"
defineFlag "t:timeline" True "Show timestamps for each task"
defineFlag "a:all" False "Act on all channels"
defineFlag "c:config" "channels.ini" "Path to configuration file"
$(return []) -- GHC 7.8 https://github.com/errge/hflags/issues/8

