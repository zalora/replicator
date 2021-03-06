Replicator configuration
========================

Overview
--------

Replicator should be configured with an [INI-file](http://en.wikipedia.org/wiki/INI_file).
Default configuration file is `channels.ini` in the current directory.
This can be changed with the `--config` command line option, for example:

    replicator --config examples/dumps.ini

The name of section in the configuration file is the name of replication channel.
Each replication channel is a connection from slave to master to get the binary log events. 

Section `[DEFAULT]` is special: all options from this section implicitly apply to
all other sections, but can be explicitly overriden. Replicator also
provides built-in default options, see [src/Replicator/Config.hs](src/Replicator/Config.hs).
Built-in default options are overriden by the `[DEFAULT]` section and by the channel sections.

Any default option can be "removed" from the channel section with a glob pattern, e. g.:

    [DEFAULT]
    master-ssl = 1
    master-ssl-ca = /etc/ca.pem

    [foo]
    ...

    [bar]
    ...
    -master-ssl-*

In the above example channel "foo" inherits options "master-ssl" and "master-ssl-ca", while "bar"
inherits only "master-ssl".

Each option may refer to other options from the same section
(and indirectly from `[DEFAULT]` and built-in options). Each section
has an implicit option `channel` that holds the section name, for example:

    [foo]
    dump = %(dump-dir)s/replicator-%(channel)s.mysql.gz
    dump-dir = /dumps

In the above example option `dump` is rendered as `/dumps/replicator-foo.mysql.gz`.

See samples under the [examples](EXAMPLES.md) directory.


Top-level options
-----------------

These options are mandatory and used by Replicator to perform its tasks.

  * `dump`— a file for a database dump. Note that if it ends with `.gz` the dump will be compressed with gzip
     on creating and decompressed on importing. Gzip is not required, zlib library is used.
     Default is `"%(dump-dir)s/replicator-%(channel)s.mysql.gz"`, And `dump-dir` is `"."` (current directory).
  * `cmd-mysqldump`— full command for creating a dump from a master.
  * `cmd-mysql`— full command for executing SQL on a slave. This includes starting and stopping replication,
    importing the dump, etc.
  * `sql-begin-import`— SQL statement to be executed *right before* the dump import starts.
     Built-in default is `"SET AUTOCOMMIT=0;"`.
  * `sql-end-import`— SQL statement to be executed *right after* the dump import ends.
    Built-in default is `"COMMIT;"`.
  * `sql-stop-slave`— SQL statement for stopping replication of a channel.
  * `sql-start-slave`— SQL statement for starting/resuming replicatiion of a channel.
  * `sql-change-master`— SQL statement for changing master options of a channel.
  * `sql-skip-repl-error`— skip one replication error (per channel).
  * `sql-reset-slave`— `RESET SLAVE ALL;` per channel.
  * `multi-source`— defines syntax of replication commands.
    Possible values: *no*, *mysql*, *mariadb* (in any case: mySQL, MariaDB, etc.).
    Default is *no*. For example:

       multi-source | Syntax
      -------------:|-------------------------------------------------
       no           | CHANGE MASTER TO ...;<br>STOP SLAVE;
       mysql        | CHANGE MASTER FOR CHANNEL 'foo' TO ...;<br>STOP SLAVE FOR CHANNEL 'foo';
       mariadb      | CHANGE MASTER 'foo' TO ...;<br>STOP SLAVE 'foo';

Commands `cmd-mysqldump` and `cmd-mysql` are wrappers around `mysqldump` and `mysql` respectively.
By default both are equal to the namesakes.
You should not specify `mysqldump` and `mysql`. These options are built out of
[mysqldump options](#mysqldump-options) and [mysql options](#mysql-options).
But you may want to define`cmd-mysqldump` or `cmd-mysql` to wrap them into, for example, SSH command:

    [foo]
    ...
    cmd-mysqldump = ssh example.com %(mysqldump)s`
    ...

Option `sql-change-master` is composed of [master options](#master-options) unless defined explicitly.

The values of `sql-stop-slave`, `sql-start-slave`, `sql-change-master` and `sql-skip-repl-error`
depend on the value of `multi-source`. Of course you *can* specify these options explicitly for incredible flexibility.


master options
--------------

These options are used to construct an SQL statement for changing master (`sql-change-master`)
iff it is not defined explicitly. Syntax for changing master is defined by the `multi-source`
option (see above). For example, `master-host=example.com` will result in `MASTER_HOST='example.com'`;
`master-ssl=1` will produce `MASTER_SSL=1`;  `master-log-pos=1234`— `MASTER_LOG_POS=1234` (note: without quotes), and so on.

Thus the following command:

    CHANGE MASTER 'foo' MASTER_HOST='example.com', MASTER_USER='repl',
           MASTER_LOG_FILE='binlog.1234', MASTER_LOG_POS=56789;

will be constructed from this snippet:

    [foo]
    multi-source    = mariadb
    master-host     = example.com
    master-log-file = binlog.1234
    master-log-pos  = 56789
    master-user     = repl

Note that you don't need to specify `master-log-pos` and `master-log-file` options explicitly.
Their default values are `auto`. Than means that Replicator will extract these values
from the dump. You only need to set these options if the dump was not created with `--master-data=2`
option.

mysqldump options
-----------------

These options are used to construct an command for creating a dump (`mysqldump`) iff it is not defined explicitly. `mysqldump-*` options are directly mapped to the
[mysqldump](http://dev.mysql.com/doc/refman/5.7/en/mysqldump.html) options with a few exceptions. Note that MySQL supports a few variants of options, e. g.:
 * `--comments`
 * `--skip-comments`
 * `--comments=0|1`

The latter is prefered for readability.
These are built-in default options for `mysqldump`:

    mysqldump-add-drop-database = 1
    mysqldump-comments = 0
    mysqldump-compress = 1
    mysqldump-default-character-set = utf8
    mysqldump-extended-insert = 1
    mysqldump-host = %(master-host)s
    mysqldump-master-data = 2
    mysqldump-single-transaction = 1
    path-mysqldump = mysqldump

Thus `mysqldump` will look like `mysqldump --compress=1 --comments=0 --master-data=2 ...`

Replicator will make sure that `--defaults-file` and `defaults-extra-file` options
are put *before all other* options.

Option `path-mysqldump` holds the path to the *mysqldump* executable.

Option `databases = foo bar` will result in `--databases foo bar` *at the end* of `cmd-mysqldump`.

Option `database = foo` will result in `foo` *at the end* of `cmd-mysqldump`.

Note that `mysqldump-master-data = 1` is not recommented, but should be safe.
While `mysqldump-master-data = 0` makes impossible automatic [channel configuration](#master-options),
you will need to figure out and specify `master-log-file` and `master-log-pos` explicitly.

mysql options
-------------

These options are used to construct an command line for executing SQL statements on the slave (`mysql`) iff it is not defined explicitly. This is done in a way similar to the one described above for [mysqldump options](#mysqldump-options).
