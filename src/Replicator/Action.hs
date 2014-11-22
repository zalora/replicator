{-# LANGUAGE LambdaCase #-}
module Replicator.Action (
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
import Replicator.Flags (flags_force)
import Replicator.Regex (masterLog, MasterLog(..), (=~))
import System.Directory (doesFileExist, removeFile)
import System.IO (stderr, withFile, IOMode(WriteMode, ReadMode))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ConfigFile as Cf
import qualified Lens.Family as LF
import qualified Pipes.ByteString as PBS
import qualified Pipes.Group as PG

type Action = Cf.ConfigParser -> Cf.SectionSpec -> IO Cf.ConfigParser

runSql :: String -> Cf.ConfigParser -> Cf.SectionSpec -> IO Cf.ConfigParser
runSql cmd conf sec = if null sql then return conf else do
    putStrLn $ "Executing " ++ show sql
    runShell $ yield (BSC.pack sql) >?> pipeCmd mysql >->
                ignoreOut >-> PBS.toHandle stderr
    return conf
    where mysql = get conf sec "cmd-mysql"
          sql = get conf sec cmd

printError :: MonadIO m =>
    Pipe (Either BSC.ByteString BSC.ByteString) BSC.ByteString m ()
printError = forever $ await >>= \case
    Left e  -> yield e >-> PBS.toHandle stderr
    Right o -> yield o

producerCmd'' :: MonadSafe m => String -> Producer BSC.ByteString m ()
producerCmd'' cmd = producerCmd cmd >-> printError



stopSlave :: Action
stopSlave = runSql "sql-stop-slave"

startSlave :: Action
startSlave = runSql "sql-start-slave"

changeMaster :: Action
changeMaster = runSql "sql-change-master"

-- TODO: get rid of IORef
getMasterLog :: Action
getMasterLog conf sec = if log_file /= "auto" && log_pos /= "auto"
    then return conf else do
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
                Right cf -> return cf
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

createDump :: Action
createDump conf sec = do
    exists <- doesFileExist dump
    when (not exists || flags_force) $ do
        putStrLn $ "Creating " ++ show dump
        withFile dump WriteMode ( \h -> runShell $
            for (compress dump $ producerCmd'' mysqldump)
                (liftIO . BSC.hPutStr h) )
    return conf
    where
        dump = get conf sec "dump"
        mysqldump = get conf sec "cmd-mysqldump"

importDump :: Action
importDump conf sec = do
    putStrLn $ "Importing " ++ show dump
    withFile dump ReadMode ( \h -> runShell $
        sql h >?> pipeCmd mysql >-> ignoreOut >-> PBS.toHandle stderr )
    return conf
    where
        begin = get conf sec "sql-begin-import"
        end = get conf sec "sql-end-import"
        dump = get conf sec "dump"
        mysql = get conf sec "cmd-mysql"
        sql h = do
            yield (BSC.pack $ begin ++ "\n")
            decompress dump $ PBS.fromHandle h
            yield (BSC.pack $ end ++ "\n")

run :: [Action] -> Action
run [] c _ = return c
run (a:aa) c s = do c' <- a c s; run aa c' s

replicate :: Action
replicate = run [ createDump
                , getMasterLog
                , stopSlave
                , importDump
                , changeMaster
                , startSlave
                ]

clean :: Action
clean conf sec = mapM_ rm files >> return conf where
    rm f = do
        exists <- doesFileExist f
        when exists $ do
            putStrLn $ "Removing " ++ show f
            removeFile f
    dump = get conf sec "dump"
    files = [ dump ]
