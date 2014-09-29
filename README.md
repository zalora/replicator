About
=====
This is a utility to automate DB replication for
MySQL database family. It is primary aimed for multi-source
replication (MariaDB >= 10.0.1 and MySQL >= 5.7),
but can be used for a single-source replication.

Requirements
============
`Replicator` is written in Haskell with [GHC](http://www.haskell.org/ghc/).
All requried Haskell libraries are listed in `replicator.cabal`.
Use [cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install)
to fetch and build all pre-requisites automatically.

Installation
============
    git clone https://github.com/zalora/replicator.git
    cd replicator
    cabal install

Configuration
=============
See `channels.ini.example`.

Run
===
`Replicator` is intended to run on a slave host:

    repl [-c <config-file>] [options] { <channel> [, ...] | --all }

This command will fetch an SQL dump for channel `foo` defined in
`channels.ini`, import it, set up master options and start replication:

    repl foo -f

Run `repl -h` for help.

