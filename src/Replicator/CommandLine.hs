module Replicator.CommandLine where

import qualified Data.ConfigFile as Cf
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)

import Replicator.Config (get, options)

makeCommandLine :: Cf.ConfigParser -> Cf.SectionSpec -> String -> String
makeCommandLine conf sec cmd = unwords $ cmd':args where
    cmd' = get conf sec cmd
    args = map mkOption opts
    opts = filter (isPrefixOf prefix) $ options conf sec
    prefix = cmd ++ "-"
    mkOption o = case name of
        "databases" -> "--" ++ name ++ " " ++ value
        _ -> "--" ++ name ++ "=" ++ show value
        where name = fromJust (stripPrefix prefix o)
              value = get conf sec o
