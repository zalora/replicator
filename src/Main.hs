{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when, unless)
import Data.List (sort)
import Data.Version (showVersion)
import HFlags (initHFlags, arguments)
import Paths_replicator (version) -- from cabal
import Replicator.Config (get, openConfig, sections, defaults)
import Replicator.Flags (flags_all, flags_config)
import Text.RawString.QQ (r)
import qualified Replicator.Command as Cmd

usage :: String
usage = "replicator " ++ showVersion version ++ [r|
Automate creating MySQL multi-source slaves

Usage: replicator [options] {command} [channel ...]

Primitive commands:

  clean     - remove dumps and temporary files for the given channels
  defaults  - print builtin default options
  dump      - create dumps for the given channels
  kick      - skip one statement (use on replication errors)
  list      - list all channels defined in the config file
  mysql     - print the mysql command used on slave
  mysqldump - print the mysqldump command used for dumping
  reset     - remove master info (use to make slave a master)
  start     - continue replication for the given channels
  stop      - pause replication for the given channels

Complex commands:

  repl    - replicate the given channels from scratch
  pipe    - same as repl but without intermediate dump file
  restart - stop, then start

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
    unless (null missing) $ error $ "No such channels: " ++ unwords missing
    case cmd of
      "defaults"  -> putStr defaults
      "default"   -> putStr defaults
      "list"      -> putStrLn $ unwords all_channels
      "mysql"     -> Cmd.printMysql conf secs
      "mysqldump" -> Cmd.printMysqldump conf secs
      "repl"      -> Cmd.replicate conf secs
      "pipe"      -> Cmd.pipe conf secs
      "dump"      -> Cmd.createDump conf secs
      "clean"     -> Cmd.clean conf secs
      "stop"      -> Cmd.stopSlave conf secs
      "start"     -> Cmd.startSlave conf secs
      "kick"      -> Cmd.kickSlave conf secs
      "restart"   -> Cmd.restartSlave conf secs
      "reset"     -> Cmd.resetSlave conf secs
      _           -> error $ "Unknown command: " ++ cmd

