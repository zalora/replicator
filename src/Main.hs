{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Char (isDigit)
import Data.Either.Utils (forceEither)
import Data.IORef (newIORef, writeIORef, readIORef)
import Data.List (intercalate)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ConfigFile as Cf

import Control.Monad (when)
import Control.Monad.Error (runErrorT)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (liftIO)

import HFlags (defineFlag, initHFlags, arguments)

import System.Directory (renameFile, doesFileExist)
import System.IO (stderr, withFile, IOMode(WriteMode))

import Pipes ((>->), await, yield)
import Pipes.Shell ((>?>), runShell, producerCmd', pipeCmd, ignoreOut)
import qualified Pipes.ByteString as PBS
import qualified Pipes.Group as PG

import qualified Lens.Family as LF

import Text.Regex.Applicative ((<$>), (*>), (<*), (<*>), (=~),
                                RE, string, anySym, psym, many, few, some)

usage :: String
usage = "Usage: repl [options] [channels]"

defineFlag "f:force" False "Force action, e. g. overwrite dumps"
defineFlag "a:all" False "Act on all channels"
defineFlag "c:config" "channels.ini" "Path to configuration file"

type Action = Cf.ConfigParser -> Cf.SectionSpec -> IO Cf.ConfigParser
        
type MasterLogFile = String
type MasterLogPos = Int
data MasterLog = MasterLog MasterLogFile MasterLogPos deriving Show

masterLogFile :: RE Char MasterLogFile
masterLogFile = some $ psym (/= '\'')

masterLogPos :: RE Char MasterLogPos
masterLogPos = read <$> some (psym isDigit)

masterLog :: RE Char MasterLog
masterLog = MasterLog <$> (many anySym *> string "MASTER_LOG_FILE='" *>
                            masterLogFile <* string "'" <* few anySym)
                      <*> (string "MASTER_LOG_POS=" *> masterLogPos <* many anySym)


get :: Cf.ConfigParser -> Cf.SectionSpec -> Cf.OptionSpec -> String
get conf sec opt =
    forceEither $ Cf.interpolatingAccess 10 conf sec opt

runSql :: Cf.ConfigParser -> Cf.SectionSpec -> String -> IO Cf.ConfigParser
runSql conf sec cmd = if null sql then return conf else do
    putStrLn $ "echo " ++ show sql ++ " | " ++ mysql
    runShell $ yield (BSC.pack sql) >?> pipeCmd mysql >->
                ignoreOut >-> PBS.toHandle stderr
    return conf
    where mysql = get conf sec "mysql"
          sql = get conf sec cmd

actionStopSlave :: Action
actionStopSlave conf sec = runSql conf sec "stop-slave-sql"

actionStartSlave :: Action
actionStartSlave conf sec = runSql conf sec "start-slave-sql"

actionAfterImportSql :: Action
actionAfterImportSql conf sec = runSql conf sec "after-import-sql"

actionChangeMaster :: Action
actionChangeMaster conf sec = do
    conf' <- actionMasterLog conf sec
    runSql conf' sec "change-master-sql"


-- TODO: get rid of IORef
actionMasterLog :: Action
actionMasterLog conf sec = if log_file /= "auto" && log_pos /= "auto"
    then return conf else do
    putStrLn $ reader ++ " | head -n " ++ show nlines ++ " | grep -F 'CHANGE MASTER TO'"
    master_log <- newIORef Nothing
    getMasterLog' master_log
    readIORef master_log >>= \case
        Nothing -> error "Could not get master log position"
        Just (MasterLog file pos) -> do
            rv <- runErrorT $ do
                conf' <- Cf.set conf sec "log-file" file
                Cf.set conf' sec "log-pos" (show pos)
            case rv of
                Left _ -> error "Failed to set master log position"
                Right cf -> return cf
    where
        nlines = 60
        log_file = get conf sec "log-file"
        log_pos = get conf sec "log-pos"
        reader = get conf sec "read-dump-cmd"
        getMasterLog' out = runShell $ handful (producerCmd' reader) >?> grep
            where
                handful = LF.over PBS.lines (PG.takes nlines)
                grep = await >>= \case
                    Nothing -> liftIO $ writeIORef out Nothing
                    Just bs -> case BSC.unpack bs =~ masterLog of
                            Nothing -> grep
                            Just r -> liftIO $ writeIORef out (Just r)

actionDump :: Action
actionDump conf sec = do
    exists <- doesFileExist dump
    when (not exists || flags_force) $ do
        putStrLn $ producer ++ " > " ++ show dump
        withFile dump_tmp WriteMode ( \h -> runShell $
            producerCmd' producer >-> PBS.toHandle h )
        renameFile dump_tmp dump
    return conf
    where
        dump = get conf sec "dump"
        dump_tmp = dump ++ ".tmp"
        producer = get conf sec "produce-dump-cmd"

actionImport :: Action
actionImport conf sec = do
    putStrLn $ "(echo " ++ show begin ++ "; \\\n" ++
                reader ++ "; \\\necho " ++ show end ++
                ") | " ++ mysql
    runShell $ sql >?> pipeCmd mysql >-> ignoreOut >-> PBS.toHandle stderr
    return conf
    where
        begin = get conf sec "begin-import-sql"
        end = get conf sec "end-import-sql"
        mysql = get conf sec "mysql"
        reader = get conf sec "read-dump-cmd"
        sql = do
            yield (BSC.pack $ begin ++ "\n")
            producerCmd' reader
            yield (BSC.pack $ end ++ "\n")


run :: [Action] -> Action
run [] c _ = return c
run (a:aa) c s = do c' <- a c s; run aa c' s

actionReplicate :: Action
actionReplicate = run [ actionDump
                      , actionStopSlave
                      , actionImport
                      , actionAfterImportSql
                      , actionChangeMaster
                      , actionStartSlave
                      ]


addChannelNames :: MonadError Cf.CPError m => Cf.ConfigParser -> m Cf.ConfigParser
addChannelNames conf = go conf $ Cf.sections conf
    where go cf [] = return cf
          go cf (x:xs) = if ch /= "auto" then go cf xs else do
            cf' <- Cf.set cf x "channel" x
            go cf' xs
            where ch = get cf x "channel"

main :: IO()
main = $initHFlags usage >> do
    conf <- fmap (forceEither . addChannelNames . forceEither) $
                Cf.readfile Cf.emptyCP flags_config
    let all_sections = Cf.sections conf
        all_channels = map (\s -> get conf s "channel") all_sections
        channels = arguments
        sections = if flags_all then all_sections
                   else filter (\s -> get conf s "channel" `elem` channels)
                               all_sections
        missing = filter (`notElem` all_channels) channels
    when (not $ null missing) $ error $ "No such channels: " ++ intercalate ", " missing
    when (null sections) $ error "No channels to act on."
    mapM_ (actionReplicate conf) sections
    