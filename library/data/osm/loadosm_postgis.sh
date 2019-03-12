FILE=$1
DB=$2

PORT=5433

#TAG=highway
TAG=building

echo "Loading "$FILE" into DB : "$DB
#echo `pwd`

#DB=test

# drop old base
psql -p $PORT -d postgres -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = '"$DB"';"
psql -p $PORT -d postgres -c "DROP DATABASE "$DB";"
# recreates it
psql -p $PORT -d postgres -c "CREATE DATABASE "$DB";"

# create extensions : must be superuser -> run script with sudo
psql -p $PORT -d $DB -c "CREATE EXTENSION postgis;CREATE EXTENSION hstore;"

# load osm schemas
psql -p $PORT -d $DB -f pgsnapshot_schema_0.6.sql
psql -p $PORT -d $DB -f pgsnapshot_schema_0.6_linestring.sql

# pbf to postgis
#JAVACMD_OPTIONS="-Djava.io.tmpdir=/mnt/volume1/juste/tmp"
#sudo /home/juste/bin/osmosis --read-pbf $FILE --log-progress --tf accept-ways building=* --used-node --write-pgsql host=localhost:$PORT database=$DB user=juste #password=`cat sql/password`
osmosis --read-pbf $FILE --log-progress --tf accept-ways building=* --used-node --write-pgsql host=localhost:$PORT database=$DB


