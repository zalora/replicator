About
=====
This is a utility to automate DB replication for
MySQL database family. It is primarily aimed for multi-source
replication (MariaDB >= 10.0.1 and MySQL >= 5.7),
but can be used for a single-source replication.

Requirements
============
Replicator is written in Haskell with [GHC](http://www.haskell.org/ghc/).
All requried Haskell libraries are listed in [replicator.cabal](replicator.cabal).
Use [cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install)
to fetch and build all pre-requisites automatically.

Installation
============
    git clone https://github.com/zalora/replicator.git
    cd replicator
    cabal install

Configuration
=============
See the [examples](EXAMPLES.md) and [configuration details](CONFIGURATION.md).

Usage
=====

    replicator [options] {command} [channel ...]

Primitive commands:

  * `clean`    - remove dumps and temporary files for the given channels
  * `defaults` - print built-in default options
  * `dump`     - create dumps for the given channels
  * `list`     - list all channels defined in the config file
  * `start`    - continue replication for the given channels
  * `stop`     - pause replication for the given channels

Complex commands:

  * `kick`    - stop, skip one statement, then start
  * `repl`    - replicate the given channels from scratch
  * `reset`   - stop, then reset slave
  * `restart` - stop, then start

The following command will create an SQL dump for channel `foo` defined in
`channels.ini`, import it, set up master options and start replication:

    replicator repl foo -f

Run `replicator -h` for help.

