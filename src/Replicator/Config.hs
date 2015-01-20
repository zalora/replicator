{-# LANGUAGE QuasiQuotes #-}
module Replicator.Config (
    Cf.sections,
    defaults,
    get,
    openConfig,
    set
) where

import Control.Applicative ((<$>))
import Data.Char (toUpper, toLower, isAlpha)
import Data.Either.Utils (forceEither)
import Data.List (union, isPrefixOf, stripPrefix, partition, intercalate)
import Data.Maybe (fromJust)
import Data.String.Utils (startswith)
import Text.RawString.QQ (r)
import qualified Data.ConfigFile as Cf

get :: Cf.ConfigParser -> Cf.SectionSpec -> Cf.OptionSpec -> String
get conf sec opt = v' where
    v' = if startswith "sql-" opt && last v /= ';' then v ++ ";" else v
    v = forceEither $ Cf.interpolatingAccess 10 conf sec opt

openConfig :: FilePath -> IO Cf.ConfigParser
openConfig f = (addOptions . forceEither) <$> Cf.readfile defaultCP f

set :: Cf.ConfigParser -> Cf.SectionSpec -> Cf.OptionSpec -> String -> Cf.ConfigParser
set conf sec opt val = forceEither $ Cf.set conf sec opt val

getOptions :: String -> Cf.ConfigParser -> Cf.SectionSpec -> [String]
getOptions prefix conf sec = map (fromJust . stripPrefix prefix')
                                (filter (isPrefixOf prefix') (options conf sec))
                        where prefix' = prefix ++ "-"

getOption :: String -> Cf.ConfigParser -> Cf.SectionSpec -> Cf.OptionSpec -> String
getOption prefix conf sec opt = if v == "auto" then "%(" ++ opt' ++ ")s" else v
    where v = forceEither $ Cf.simpleAccess conf sec opt'
          opt' = prefix ++ "-" ++ opt

options :: Cf.ConfigParser -> Cf.SectionSpec -> [Cf.OptionSpec]
options conf sec = forceEither (Cf.options conf sec)
                    `union` forceEither (Cf.options conf "DEFAULT")

buildOption :: Cf.ConfigParser
            -> (Cf.OptionSpec,  Cf.ConfigParser -> Cf.SectionSpec -> String)
            -> Cf.ConfigParser
buildOption conf (opt, builder) = foldl go conf (Cf.sections conf) where
    go cf s = if Cf.has_option cf s opt && ("auto" /= get cf s opt) then cf
              else set cf s opt (builder cf s)

addOptions :: Cf.ConfigParser -> Cf.ConfigParser
addOptions conf = foldl buildOption conf opts where
    opts = [ ("channel", \_ s -> s)
           , ("cmd-mysqldump", makeCommand "mysqldump")
           , ("cmd-mysql", makeCommand "mysql")
           , ("sql-change-master", makeSqlChangeMaster)
           , ("sql-stop-slave", makeSqlSlave "STOP SLAVE")
           , ("sql-start-slave", makeSqlSlave "START SLAVE")
           , ("sql-set-slave-skip-counter", makeSetSlaveSkipCounter)
           ]

makeSqlChangeMaster :: Cf.ConfigParser -> Cf.SectionSpec -> String
makeSqlChangeMaster conf sec = "CHANGE MASTER" ++ channelSQL conf sec ++
    " TO " ++ master ++ ";"
    where master = intercalate ", " master_args
          master_args = map mkArgument opts
          opts = getOptions "master" conf sec
          mkArgument name = name' ++ "=" ++ value' where
                name' = "MASTER_" ++ map (toUpper . tr) name
                value = getOption "master" conf sec name
                tr '-' = '_'
                tr c = c
                value' = case name' of
                    "MASTER_LOG_POS" -> value
                    "MASTER_SSL" -> value
                    _ -> "'" ++ value ++ "'"

makeSqlSlave :: String -> Cf.ConfigParser -> Cf.SectionSpec -> String
makeSqlSlave cmd conf sec = cmd ++ channelSQL conf sec ++ ";"

multiSource :: String -> String
multiSource a = map toLower $ take 5 $ filter isAlpha a

channelSQL :: Cf.ConfigParser -> Cf.SectionSpec -> String
channelSQL conf sec = case multi of
    "maria" -> " '" ++ channel ++ "'"
    "mysql" -> " FOR CHANNEL '" ++ channel ++ "'"
    _       -> ""
    where multi = multiSource $ get conf sec "multi-source"
          channel = get conf sec "channel"

makeSetSlaveSkipCounter :: Cf.ConfigParser -> Cf.SectionSpec -> String
makeSetSlaveSkipCounter conf sec = case multi of
    "maria" -> "SET @@default_master_connection='%(channel)s'; SET GLOBAL SQL_SLAVE_SKIP_COUNTER=1;"
    _       -> "SET GLOBAL SQL_SLAVE_SKIP_COUNTER=1;" -- FIXME: MySQL?
    where multi = multiSource $ get conf sec "multi-source"

makeCommand :: String -> Cf.ConfigParser -> Cf.SectionSpec -> String
makeCommand cmd conf sec = unwords $ cmd':args where
    cmd' = get conf sec cmd
    args = map mkArgument opts
    opts = reorderMySQLOptions $ getOptions cmd conf sec
    mkArgument name = case name of
        "database"  -> value
        "databases" -> o ++ " " ++ value
        "password"  -> o ++ "='" ++ value ++ "'"
        _           -> o ++ "=" ++ show value
        where value = getOption cmd conf sec name
              o = "--" ++ name

-- defaults-file and defaults-extra-file
-- must go first, see http://bugs.mysql.com/bug.php?id=31312
-- databases and all-databases at the end is just a matter of taste
reorderMySQLOptions :: [String] -> [String]
reorderMySQLOptions a = b ++ m ++ e
    where (b, a') = partition (`elem` begin) a
          (e, m) = partition (`elem` end) a'
          begin = ["defaults-file", "defaults-extra-file"]
          end = ["database", "databases", "all-databases"]

defaults :: String
defaults = [r|[DEFAULT]
dump = %(dump-dir)s/replicator-%(channel)s.mysql.gz
dump-dir = .
master-log-file = auto
master-log-pos  = auto
multi-source = no
mysql = mysql
mysql-default-character-set = utf8
mysql-host = localhost
mysql-user = root
mysqldump = mysqldump
mysqldump-comments = 0
mysqldump-compress = 1
mysqldump-default-character-set = utf8
mysqldump-host = %(master-host)s
mysqldump-master-data = 2
mysqldump-single-transaction = 1
sql-begin-import = SET AUTOCOMMIT=0;
sql-end-import = COMMIT;
|]

defaultCP :: Cf.ConfigParser
defaultCP = forceEither $ Cf.readstring
            Cf.emptyCP
            defaults

