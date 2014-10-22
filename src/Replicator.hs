{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Char (isDigit)
import Data.Either.Utils (forceEither)
import Data.IORef (newIORef, writeIORef, readIORef)
import Data.List (sort)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ConfigFile as Cf

import Control.Monad (when, forever)
import Control.Monad.Error (runErrorT)
import Control.Monad.IO.Class (liftIO, MonadIO)

import HFlags (defineFlag, initHFlags, arguments)

import System.Directory (renameFile, doesFileExist)
import System.IO (stderr, withFile, IOMode(WriteMode, ReadMode))

import Pipes ((>->), await, yield, Pipe, Producer, for)
import Pipes.Shell ((>?>), runShell, producerCmd, pipeCmd, ignoreOut)
import Pipes.Safe (MonadSafe)
import qualified Pipes.ByteString as PBS
import qualified Pipes.Group as PG

import qualified Lens.Family as LF

import Text.Regex.Applicative ((<$>), (*>), (<*), (<*>), (=~),
                                RE, string, anySym, psym, many, few, some)

import Replicator.CommandLine (makeCommandLine)
import Replicator.Config (get, addChannelNames, addDefaults)
import Replicator.Compress (compress, decompress)

usage :: String
usage = "Usage: repl [options] {command} [channel ...]\n\n" ++
        "Commands:\n\n" ++
        "  repl   - start replicating given channels\n" ++
        "  dump   - only create dump for given channels\n" ++
        "  list   - list all channels defined in config file\n\n" ++
        "Options:"

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


runSql :: Cf.ConfigParser -> Cf.SectionSpec -> String -> IO Cf.ConfigParser
runSql conf sec cmd = if null sql then return conf else do
    putStrLn $ "Executing " ++ show sql
    runShell $ yield (BSC.pack sql) >?> pipeCmd mysql >->
                ignoreOut >-> PBS.toHandle stderr
    return conf
    where mysql = makeCommandLine conf sec "mysql"
          sql = get conf sec cmd

actionStopSlave :: Action
actionStopSlave conf sec = runSql conf sec "stop-slave-sql"

actionStartSlave :: Action
actionStartSlave conf sec = runSql conf sec "start-slave-sql"

actionChangeMaster :: Action
actionChangeMaster conf sec = do
    conf' <- actionMasterLog conf sec
    runSql conf' sec "change-master-sql"


printError :: MonadIO m => Pipe (Either BSC.ByteString BSC.ByteString) BSC.ByteString m ()
printError = forever $ await >>= \case
    Left e  -> yield e >-> PBS.toHandle stderr
    Right o -> yield o

producerCmd'' :: MonadSafe m => String -> Producer BSC.ByteString m ()
producerCmd'' cmd = producerCmd cmd >-> printError

-- TODO: get rid of IORef
actionMasterLog :: Action
actionMasterLog conf sec = if log_file /= "auto" && log_pos /= "auto"
    then return conf else do
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
        dump = get conf sec "dump"
        getMasterLog' out = withFile dump ReadMode ( \h -> runShell $
            handful (decompress dump $ PBS.fromHandle h) >?> grep )
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
        putStrLn $ "Creating " ++ show dump
        withFile dump_tmp WriteMode ( \h -> runShell $
            for (compress dump $ producerCmd'' mysqldump)
                (liftIO . BSC.hPutStr h) )
        renameFile dump_tmp dump
    return conf
    where
        dump = get conf sec "dump"
        dump_tmp = dump ++ ".tmp"
        mysqldump = makeCommandLine conf sec "mysqldump"

actionImport :: Action
actionImport conf sec = do
    putStrLn $ "Importing " ++ show dump
    withFile dump ReadMode ( \h -> runShell $
        sql h >?> pipeCmd mysql >-> ignoreOut >-> PBS.toHandle stderr )
    return conf
    where
        begin = get conf sec "begin-import-sql"
        end = get conf sec "end-import-sql"
        dump = get conf sec "dump"
        mysql = makeCommandLine conf sec "mysql"
        sql h = do
            yield (BSC.pack $ begin ++ "\n")
            decompress dump $ PBS.fromHandle h
            yield (BSC.pack $ end ++ "\n")


run :: [Action] -> Action
run [] c _ = return c
run (a:aa) c s = do c' <- a c s; run aa c' s

actionReplicate :: Action
actionReplicate = run [ actionDump
                      , actionStopSlave
                      , actionImport
                      , actionChangeMaster
                      , actionStartSlave
                      ]

main :: IO()
main = $initHFlags usage >> do
    conf <- fmap (addDefaults ["mysql", "mysqldump"] . addChannelNames . forceEither) $
                Cf.readfile Cf.emptyCP flags_config
    let (cmd:channels) = arguments
        all_sections = Cf.sections conf
        all_channels = sort $ map (\s -> get conf s "channel") all_sections
        sections = if flags_all || length all_sections == 1 then all_sections
                   else filter (\s -> get conf s "channel" `elem` channels)
                               all_sections
        missing = filter (`notElem` all_channels) channels
    when (null arguments) $ error "No command specified"
    when (not $ null missing) $ error $ "No such channels: " ++ unwords missing
    case cmd of
        "repl" -> mapM_ (actionReplicate conf) sections
        "dump" -> mapM_ (actionDump conf) sections
        "list" -> putStrLn $ unwords all_channels
        _      -> error $ "Unknown command: " ++ cmd

