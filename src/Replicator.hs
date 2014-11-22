{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when)
import Data.List (sort)
import Data.Version (showVersion)
import HFlags (initHFlags, arguments)
import Paths_replicator (version) -- from cabal
import Replicator.Config (get, openConfig, sections)
import Replicator.Flags (flags_all, flags_config)
import Text.RawString.QQ (r)
import qualified Replicator.Action as Action

usage :: String
usage = "replicator " ++ showVersion version ++ [r|
Automate creating MySQL multi-source slaves

Usage: replicator [options] {command} [channel ...]

Commands:

  list   - list all channels defined in the config file
  repl   - replicate the given channels from scratch
  dump   - only create dump for the given channels
  clean  - remove dumps and temporary files for the given channels

  stop   - pause replication for the given channels
  start  - continue replication for the given channels

Options:|]

main :: IO()
main = $initHFlags usage >> do
    conf <- openConfig flags_config
    let (cmd:channels) = arguments
        all_sections = sections conf
        all_channels = sort $ map (\s -> get conf s "channel") all_sections
        secs = if flags_all || length all_sections == 1 then all_sections
                   else filter (\s -> get conf s "channel" `elem` channels)
                               all_sections
        missing = filter (`notElem` all_channels) channels
    when (null arguments) $ error "No command specified"
    when (not $ null missing) $ error $ "No such channels: " ++ unwords missing
    case cmd of
        "list"  -> putStrLn $ unwords all_channels
        "repl"  -> mapM_ (Action.replicate conf) secs
        "dump"  -> mapM_ (Action.createDump conf) secs
        "clean" -> mapM_ (Action.clean conf) secs
        "stop"  -> mapM_ (Action.stopSlave conf) secs
        "start" -> mapM_ (Action.startSlave conf) secs
        _       -> error $ "Unknown command: " ++ cmd

