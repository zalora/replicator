module Replicator.CommandLine (
    makeCommandLine
) where

import qualified Data.ConfigFile as Cf
import Data.List (isPrefixOf, stripPrefix, partition)
import Data.Maybe (fromJust)

import Replicator.Config (get, options)


-- defaults-file and defaults-extra-file
-- must go first, see http://bugs.mysql.com/bug.php?id=31312
-- databases and all-databases at the end is just a matter of taste
reorder :: [String] -> [String]
reorder a = fst ++ other ++ lst
    where (fst, a') = partition (`elem` first) a
          (lst, other) = partition (`elem` last) a'
          first = ["defaults-file", "defaults-extra-file"]
          last = ["databases", "all-databases"]

makeCommandLine :: Cf.ConfigParser -> Cf.SectionSpec -> String -> String
makeCommandLine conf sec cmd = unwords $ cmd':args where
    cmd' = get conf sec cmd
    args = map mkArgument opts
    all_options = options conf sec
    prefix = cmd ++ "-"
    opts = reorder $
        map (fromJust . stripPrefix prefix) (filter (isPrefixOf prefix) all_options)
    mkArgument name = case name of
        "databases" -> "--" ++ name ++ " " ++ value
        _ -> "--" ++ name ++ "=" ++ show value
        where value = get conf sec (prefix ++ name)
