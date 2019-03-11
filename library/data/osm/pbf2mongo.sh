# directly insert as Geojson into mongo

FILE=$1
DB=$2

# 1) osmosis : pbf -> filtered pbf
osmosis --read-pbf $FILE --log-progress --tf accept-ways $TAG=* --used-node --write-pbf $FILE.tmp.pbf

# 2) pbf -> GeoJson
ogr2ogr -f GeoJSON $FILE.tmp.json $FILE.tmp.pbf

# 3) mongoimport
python loadMongo.py $FILE.tmp.json $DB
