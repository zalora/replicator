{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Replicator.Command (
    clean,
    createDump,
    replicate,
    startSlave,
    stopSlave
) where

import Prelude hiding (replicate)

import Control.Monad (when, forever)
import Control.Monad.Error (runErrorT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IORef (newIORef, writeIORef, readIORef)
import Pipes ((>->), await, yield, Pipe, Producer, for)
import Pipes.Safe (MonadSafe)
import Pipes.Shell ((>?>), runShell, producerCmd, pipeCmd, ignoreOut)
import Replicator.Compress (compress, decompress)
import Replicator.Config (get)
import Replicator.Flags (flags_force, flags_timeline)
import Replicator.Regex (masterLog, MasterLog(..), (=~))
import Replicator.Timeline (timestamp, seconds)
import System.Directory (doesFileExist, removeFile)
import System.IO (stderr, withFile, IOMode(WriteMode, ReadMode))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ConfigFile as Cf
import qualified Lens.Family as LF
import qualified Pipes.ByteString as PBS
import qualified Pipes.Group as PG

data Context = Context {
    conf :: Cf.ConfigParser
 ,  sec  :: Cf.SectionSpec
 ,  zero :: Integer
}

type Task = Context -> IO Context
type Command = Cf.ConfigParser -> Cf.SectionSpec -> IO ()


quack :: Integer -> String -> IO()
quack z m = if flags_timeline then timestamp z m else putStrLn m

runSql :: String -> Task
runSql cmd Context{..} = if null sql then return Context{..} else do
    quack zero $ "Executing " ++ show sql
    runShell $ yield (BSC.pack sql) >?> pipeCmd mysql >->
                ignoreOut >-> PBS.toHandle stderr
    return Context{..}
    where mysql = get conf sec "cmd-mysql"
          sql = get conf sec cmd

printError :: MonadIO m =>
    Pipe (Either BSC.ByteString BSC.ByteString) BSC.ByteString m ()
printError = forever $ await >>= \case
    Left e  -> yield e >-> PBS.toHandle stderr
    Right o -> yield o

producerCmd'' :: MonadSafe m => String -> Producer BSC.ByteString m ()
producerCmd'' cmd = producerCmd cmd >-> printError



taskStopSlave :: Task
taskStopSlave = runSql "sql-stop-slave"

taskStartSlave :: Task
taskStartSlave = runSql "sql-start-slave"

taskChangeMaster :: Task
taskChangeMaster = runSql "sql-change-master"

-- TODO: get rid of IORef
taskGetMasterLog :: Task
taskGetMasterLog Context{..} = if log_file /= "auto" && log_pos /= "auto"
    then return Context{..} else do
    master_log <- newIORef Nothing
    getMasterLog' master_log
    readIORef master_log >>= \case
        Nothing -> error "Could not get master log position"
        Just (MasterLog file pos) -> do
            rv <- runErrorT $ do
                conf' <- Cf.set conf sec "master-log-file" file
                Cf.set conf' sec "master-log-pos" (show pos)
            case rv of
                Left _ -> error "Failed to set master log position"
                Right cf -> return Context {conf = cf, ..}
    where
        nlines = 60
        log_file = get conf sec "master-log-file"
        log_pos = get conf sec "master-log-pos"
        dump = get conf sec "dump"
        getMasterLog' out = withFile dump ReadMode ( \h -> runShell $
            handful (decompress dump $ PBS.fromHandle h) >?> grep )
                where
                    handful = LF.over PBS.lines (PG.takes nlines)
                    grep = await >>= \case
                        Nothing -> liftIO $ writeIORef out Nothing
                        Just bs -> case BSC.unpack bs =~ masterLog of
                            Nothing -> grep
                            Just m -> liftIO $ writeIORef out (Just m)

taskCreateDump :: Task
taskCreateDump Context{..} = do
    exists <- doesFileExist dump
    when (not exists || flags_force) $ do
        quack zero $ "Creating " ++ show dump
        withFile dump WriteMode ( \h -> runShell $
            for (compress dump $ producerCmd'' mysqldump)
                (liftIO . BSC.hPutStr h) )
    return Context{..}
    where
        dump = get conf sec "dump"
        mysqldump = get conf sec "cmd-mysqldump"

taskImportDump :: Task
taskImportDump Context{..} = do
    quack zero $ "Importing " ++ show dump
    withFile dump ReadMode ( \h -> runShell $
        sql h >?> pipeCmd mysql >-> ignoreOut >-> PBS.toHandle stderr )
    return Context{..}
    where
        begin = get conf sec "sql-begin-import"
        end = get conf sec "sql-end-import"
        dump = get conf sec "dump"
        mysql = get conf sec "cmd-mysql"
        sql h = do
            yield (BSC.pack $ begin ++ "\n")
            decompress dump $ PBS.fromHandle h
            yield (BSC.pack $ end ++ "\n")


taskClean :: Task
taskClean Context{..} = mapM_ rm files >> return Context{..} where
    rm f = do
        exists <- doesFileExist f
        when exists $ do
            putStrLn $ "Removing " ++ show f
            removeFile f
    dump = get conf sec "dump"
    files = [ dump ]


taskDone :: Task
taskDone Context{..} = quack zero "Done." >> return Context{..}

run :: [Task] -> Command
run tasks conf sec = if flags_timeline
    then do
        zero <- seconds
        run' Context{conf, sec, zero} (tasks ++ [taskDone])
    else do
        run' Context{conf, sec, zero = 0} tasks
    where
        run' _ [] = return ()
        run' ctx (t:tt) = do
            ctx' <- t ctx;
            run' ctx' tt

replicate :: Command
replicate = run [ taskCreateDump
                , taskGetMasterLog
                , taskStopSlave
                , taskImportDump
                , taskChangeMaster
                , taskStartSlave
                ]

clean :: Command
clean = run [ taskClean ]

stopSlave :: Command
stopSlave = run [ taskStopSlave ]

startSlave :: Command
startSlave = run [ taskStartSlave ]

createDump :: Command
createDump = run [ taskCreateDump ]

