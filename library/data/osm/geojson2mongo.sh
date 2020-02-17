

# import geojson to mongo
# (tailored for files exported from pbf by osmium - not sure would work with other : see grep in maketmp ; spplitting ; last comma removing )

FILE=$1
DB=$2
CHUNKSIZE=$3


# FIXME commented as rerun a WIP (failed import) - uncomment for a full run
#rm -rf tmp
#mkdir tmp

#tail -n +2 $FILE | head -n -1 > $FILE.tmp
#split -l $CHUNKSIZE $FILE.tmp --numeric-suffixes tmp/tmp

#head -n 1 $FILE > HEAD
#tail -n 1 $FILE > TAIL

mongo $2 --eval "printjson(db.dropDatabase())"

ls tmp | awk '{print "./maketmp.sh "$1";python loadMongo.py FILE '$DB'"}' | sh

# last will fail, specific run
ls -r tmp | head -n 1 | awk '{print "rm FILE;cat HEAD >> FILE; cat tmp/"$1" | grep LineString >> FILE; cat TAIL >> FILE;python loadMongo.py FILE '$DB'"}' | sh


# create spatial index
mongo $2 --eval "db.buildings.createIndex({'geometry':'2dsphere'})"



