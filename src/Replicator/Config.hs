{-# LANGUAGE QuasiQuotes #-}
module Replicator.Config (
    get,
    makeCommandLine,
    options,
    openConfig
) where

import qualified Data.ConfigFile as Cf
import Control.Applicative ((<$>))
import Data.Either.Utils (forceEither)
import Data.List (union, isPrefixOf, stripPrefix, partition)
import Data.Maybe (fromJust)
import Text.RawString.QQ (r)

get :: Cf.ConfigParser -> Cf.SectionSpec -> Cf.OptionSpec -> String
get conf sec opt = forceEither $ Cf.get conf sec opt

options :: Cf.ConfigParser -> Cf.SectionSpec -> [Cf.OptionSpec]
options conf sec = forceEither (Cf.options conf sec)
                    `union` forceEither (Cf.options conf "DEFAULT")

openConfig :: FilePath -> IO Cf.ConfigParser
openConfig f = (addChannelNames . forceEither) <$> Cf.readfile defaultCP f

addChannelNames :: Cf.ConfigParser -> Cf.ConfigParser
addChannelNames conf = go conf $ Cf.sections conf
    where go cf [] = cf
          go cf (x:xs) = if ch /= "auto" then go cf xs else go cf' xs
            where ch = get cf x "channel"
                  cf' = forceEither $ Cf.set cf x "channel" x

defaults :: String
defaults = [r|[DEFAULT]
channel   = auto
log-file  = auto
log-pos   = auto
mysql     = mysql
mysqldump = mysqldump
mysqldump-master-data = 2
start-slave-sql  = START SLAVE;
stop-slave-sql   = STOP SLAVE;
begin-import-sql = SET autocommit = 0; SET unique_checks = 0; SET foreign_key_checks = 0;
end-import-sql   = COMMIT;
|]

defaultCP = forceEither $ Cf.readstring
            Cf.emptyCP {Cf.accessfunc = Cf.interpolatingAccess 10}
            defaults

makeCommandLine :: Cf.ConfigParser -> Cf.SectionSpec -> String -> String
makeCommandLine conf sec cmd = unwords $ cmd':args where
    cmd' = get conf sec cmd
    args = map mkArgument opts
    all_options = options conf sec
    prefix = cmd ++ "-"
    opts = reorderMySQLOptions $
        map (fromJust . stripPrefix prefix) (filter (isPrefixOf prefix) all_options)
    mkArgument name = case name of
        "databases" -> "--" ++ name ++ " " ++ value
        _ -> "--" ++ name ++ "=" ++ show value
        where value = get conf sec (prefix ++ name)

-- defaults-file and defaults-extra-file
-- must go first, see http://bugs.mysql.com/bug.php?id=31312
-- databases and all-databases at the end is just a matter of taste
reorderMySQLOptions :: [String] -> [String]
reorderMySQLOptions a = fst ++ other ++ lst
    where (fst, a') = partition (`elem` first) a
          (lst, other) = partition (`elem` last) a'
          first = ["defaults-file", "defaults-extra-file"]
          last = ["databases", "all-databases"]

