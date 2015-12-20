{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Replicator.Command (
    clean,
    createDump,
    kickSlave,
    printMysql,
    printMysqldump,
    replicate,
    resetSlave,
    restartSlave,
    startSlave,
    stopSlave
) where

import Prelude hiding (replicate)

import Control.Applicative ((<$>))
import Control.Monad (when, forever)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IORef (newIORef, writeIORef, readIORef)
import Pipes ((>->), await, yield, Pipe, Producer, for)
import Pipes.Safe (MonadSafe)
import Pipes.Shell ((>?>), runShell, producerCmd, pipeCmd, ignoreOut)
import Replicator.Compress (compress, decompress)
import Replicator.Config (get)
import Replicator.Flags (flags_force, flags_timeline, flags_parallel, flags_progress)
import Replicator.Regex (masterLog, MasterLog(..), (=~))
import Replicator.Timeline (timestamp, seconds)
import System.Directory (doesFileExist, removeFile)
import System.IO (hPutStrLn, stderr, withFile, IOMode(WriteMode, ReadMode))
import System.Posix.Files (getFileStatus, fileSize)
import Text.Printf (printf)
import qualified Control.Monad.Parallel as Parallel
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ConfigFile as Cf
import qualified Lens.Family as LF
import qualified Pipes.ByteString as PBS
import qualified Pipes.Group as PG

data Context = Context {
    conf    :: Cf.ConfigParser
 ,  sec     :: Cf.SectionSpec
 ,  zero    :: Integer
 ,  complex :: Bool
}

type Command = Cf.ConfigParser -> [Cf.SectionSpec] -> IO ()
type PipeProgressState = Maybe (Integer, Integer) -- (percent, seconds)
type PipeProgressReport = PipeProgressState -> Integer -> IO PipeProgressState
type Task = Context -> IO Context

humanSize :: Integer -> String
humanSize b
    | b < 1024 = show b ++ " B"
    | k < 1024 = show k ++ " KiB"
    | m < 1024 = show m ++ " MiB"
    | otherwise = printf "%.1f" g ++ " GiB"
    where k = b `div` 1024
          m = k `div` 1024
          g :: Double
          g = fromIntegral m / 1024

quack :: Integer -> String -> IO()
quack z m = do
              m' <- if flags_timeline then timestamp z m else return m
              hPutStrLn stderr m'

getFileSize :: String -> IO Integer
getFileSize path = (toInteger . fileSize) <$> getFileStatus path

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

pipeProgress :: MonadIO m
             => PipeProgressReport
             -> Producer BSC.ByteString m ()
             -> Producer BSC.ByteString m ()
pipeProgress report producer = if not flags_progress
    then producer
    else producer >?> progress Nothing 0 where
    progress :: MonadIO m => PipeProgressState -> Integer
              -> Pipe (Maybe BSC.ByteString) BSC.ByteString m ()
    progress state c = await >>= \case
                    Nothing -> return ()
                    Just b  -> do
                        let c' = c + toInteger (BSC.length b)
                        state' <- liftIO $ report state c'
                        yield b
                        progress state' c'

sizeReport :: String -> Context -> PipeProgressReport
sizeReport msg Context{..} st s' = do
    t' <- seconds
    let progress = case st of
            Nothing -> if s' > 0 then Just (s', t') else Nothing
            Just (s, t) -> if (t' - t > 30) && ((t' - t > 600) || 10 * (s' - s) > s)
                        then Just (s', t')
                        else Nothing
    case progress of
        Nothing -> return st
        Just (s'', t'') -> do
            quack zero (msg ++ " ... " ++ humanSize s'')
            return (Just (s'', t''))


taskStopSlave :: Task
taskStopSlave = runSql "sql-stop-slave"

taskStartSlave :: Task
taskStartSlave = runSql "sql-start-slave"

taskResetSlave :: Task
taskResetSlave = runSql "sql-reset-slave"

taskChangeMaster :: Task
taskChangeMaster = runSql "sql-change-master"

taskSkipReplError :: Task
taskSkipReplError = runSql "sql-skip-repl-error"


-- TODO: get rid of IORef
taskGetMasterLog :: Task
taskGetMasterLog Context{..} = if log_file /= "auto" && log_pos /= "auto"
    then return Context{..} else do
    master_log <- newIORef Nothing
    getMasterLog' master_log
    readIORef master_log >>= \case
        Nothing -> error "Could not get master log position"
        Just (MasterLog file pos) -> do
            rv <- runExceptT $ do
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
    if complex && exists && not flags_force
    then do
        size <- getFileSize dump
        quack zero $ "Using existing " ++ show dump ++ " " ++ humanSize size
        return Context{..}
    else do
        quack zero msg
        withFile dump WriteMode ( \h -> runShell $
          for (pipeProgress report $ compress dump $ producerCmd'' mysqldump)
            (liftIO . BSC.hPutStr h) )
        amount <- liftIO $ getFileSize dump
        quack zero (msg ++ " ... " ++ humanSize amount)
        return Context{..}
    where
        dump      = get conf sec "dump"
        msg       = "Creating " ++ show dump
        mysqldump = get conf sec "cmd-mysqldump"
        report    = sizeReport msg Context{..}

taskImportDump :: Task
taskImportDump Context{..} = do
    quack zero msg
    withFile dump ReadMode ( \h -> runShell $
        sql h >?> pipeCmd mysql >-> ignoreOut >-> PBS.toHandle stderr )
    return Context{..}
    where
        msg = "Importing " ++ show dump
        begin = get conf sec "sql-begin-import"
        end = get conf sec "sql-end-import"
        dump = get conf sec "dump"
        mysql = get conf sec "cmd-mysql"
        report :: Integer -> PipeProgressReport
        report a st c = do
            t' <- seconds
            let p' = toInteger (100 * c) `div` a
                progress = case st of
                    Nothing -> if p' > 0 then Just (p', t') else Nothing
                    Just (p, t) -> if (p' > p) && (t' - t > 30 || p' == 100) && (
                                (t' - t > 300) ||
                                (p' `mod` 10 == 0) ||
                                p' < 5 || 100 - p' < 5
                              ) then Just (p', t')
                                else Nothing
            case progress of
                Nothing -> return st
                Just (p'', t'') -> do
                    quack zero (msg ++ " ... " ++ show p'' ++"%")
                    return (Just (p'', t''))
        sql h = do
            amount <- liftIO $ getFileSize dump
            yield (BSC.pack $ begin ++ "\n")
            decompress dump $ pipeProgress (report amount) (PBS.fromHandle h)
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


run :: [Task] -> Command
run tasks conf sections = do
    zero <- if flags_timeline then seconds else return 0
    _ <- map' (\s -> run' Context{sec = s, ..} tasks) sections
    when flags_timeline $ quack zero "Done."
    where
        complex = length tasks > 1
        map' = if flags_parallel then Parallel.mapM else mapM
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

restartSlave :: Command
restartSlave = run [ taskStopSlave, taskStartSlave ]

resetSlave :: Command
resetSlave = run [ taskResetSlave ]

createDump :: Command
createDump = run [ taskCreateDump ]

kickSlave :: Command
kickSlave = run [ taskSkipReplError ]

printMysqldump :: Command
printMysqldump conf = mapM_ info
  where
    info s = putStrLn $ get conf s "mysqldump"

printMysql:: Command
printMysql conf = mapM_ info
  where
    info s = putStrLn $ get conf s "mysql"

