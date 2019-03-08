FILE=$1
OUT=$2

#TAG=highway
TAG=building

osmosis --read-pbf $FILE --log-progress --tf accept-ways $TAG=* --used-node --write-pbf $OUT
