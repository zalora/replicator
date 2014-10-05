module Replicator.Config where

import qualified Data.ConfigFile as Cf
import Data.Either.Utils (forceEither)
import Data.Set (fromList, toList)

get :: Cf.ConfigParser -> Cf.SectionSpec -> Cf.OptionSpec -> String
get conf sec opt =
    forceEither $ Cf.interpolatingAccess 10 conf sec opt

-- Lists all options including defaults
options :: Cf.ConfigParser -> Cf.SectionSpec -> [String]
options conf sec = (toList . fromList) $
    forceEither (Cf.options conf sec) ++
    forceEither (Cf.options conf "DEFAULT")
