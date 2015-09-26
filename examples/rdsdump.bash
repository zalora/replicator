#!/usr/bin/env bash
set -euo pipefail

mysql_args=
mysqldump_args=
while [ $# -gt 0 ]; do
  case $1 in
    --host=*|--password=*|--user=*|\
    --defaults-file=*|--defaults-extra-file=*|\
    --ssl=*|--ssl-ca=*|--ssl-key=*|--ssl-cert=*|\
    -h?*|-u?*|-p?*)
      mysql_args="$mysql_args $1"
      mysqldump_args="$mysqldump_args $1"
      shift 1;;
    --host|--user|\
    --defaults-file|--defaults-extra-file|\
    --ssl-ca|--ssl-key|--ssl-cert|\
    -h|-u)
      mysql_args="$mysql_args $1 $2"
      mysqldump_args="$mysqldump_args $1 $2"
      shift 2;;
    --master-data=*)
      shift;;
    --master-data)
      shift 2;;
    *)
      mysqldump_args="$mysqldump_args $1"
      shift;;
  esac
done

replica () {
  mysql $mysql_args "$@"
}

start_replication () {
  replica -N -e "CALL mysql.rds_start_replication;" >&2
}

stop_replication () {
  replica -N -e "CALL mysql.rds_stop_replication;" >&2
}

trap 'start_replication' EXIT
stop_replication

replica -e 'SHOW SLAVE STATUS\G' | awk -f <(cat - <<- 'AWK'
  /\<Exec_Master_Log_Pos\>/    { log_pos = $2 };
  /\<Relay_Master_Log_File\>/  { log_file = $2 };
  END {
    printf "-- CHANGE MASTER TO MASTER_LOG_FILE='%s', MASTER_LOG_POS=%d\n", log_file, log_pos
  }
AWK
)

mysqldump $mysql_args &
sleep 10

start_replication
trap - EXIT

wait
