# directly insert as Geojson into mongo

FILE=$1
DB=$2

# 1) osmosis : pbf -> filtered pbf
#osmosis --read-pbf $FILE --log-progress --tf accept-ways $TAG=* --used-node --write-pbf $FILE.tmp.pbf
filterosm_pbf.sh $FILE

# 2) pbf -> GeoJson
#ogr2ogr -f GeoJSON $FILE.tmp.json $FILE.tmp.pbf
osmium $FILE.tmp.pbf -o $FILE.geojson

# 3) mongoimport
# requires mongo running on localhost:27017
python loadMongo.py $FILE.geojson $DB


