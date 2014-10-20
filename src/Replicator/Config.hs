module Replicator.Config where

import qualified Data.ConfigFile as Cf
import Data.Either.Utils (forceEither)
import Data.Set (fromList, toList)

get :: Cf.ConfigParser -> Cf.SectionSpec -> Cf.OptionSpec -> String
get conf sec opt =
    forceEither $ Cf.interpolatingAccess 10 conf sec opt

options :: Cf.ConfigParser -> Cf.SectionSpec -> [String]
options conf sec = (toList . fromList) $
    forceEither (Cf.options conf sec) ++
    forceEither (Cf.options conf "DEFAULT")

addChannelNames :: Cf.ConfigParser -> Cf.ConfigParser
addChannelNames conf = go conf $ Cf.sections conf
    where go cf [] = cf
          go cf (x:xs) = if ch /= "auto" then go cf xs else go cf' xs
            where ch = get cf x "channel"
                  cf' = forceEither $ Cf.set cf x "channel" x

addDefaults :: [Cf.OptionSpec] -> Cf.ConfigParser -> Cf.ConfigParser
addDefaults [] conf = conf
addDefaults (o:os) conf = if Cf.has_option conf "DEFAULT" o
    then addDefaults os conf else addDefaults os conf'
    where conf' = forceEither $ Cf.set conf "DEFAULT" o o
