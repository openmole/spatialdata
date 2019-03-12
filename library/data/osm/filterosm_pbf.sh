FILE=$1

#DB=$2

#PORT=5433

#TAG=highway
#TAG=building

#echo `pwd`

#DB=test

# pbf to postgis
#JAVACMD_OPTIONS="-Djava.io.tmpdir=/mnt/volume1/juste/tmp"
#sudo /home/juste/bin/osmosis --read-pbf $FILE --log-progress --tf accept-ways building=* --used-node --write-pgsql host=localhost:$PORT database=$DB user=juste #password=`cat sql/password`
osmosis --read-pbf $FILE.pbf --log-progress --tf accept-ways building=* --used-node --write-pbf $FILE.tmp.pbf


