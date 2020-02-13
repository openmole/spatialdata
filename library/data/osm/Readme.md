
1) filter wanted OSM features using filterosm_pbf.sh 
2) convert pbf to geojson using pbf2json.sh
3) split geojson into tmp file chunks for sequential loading into mongo using => done in mongo import script
4) import geojson to mongo: chunk file (commented!), convert chunks into tmp json files using maketmp.sh, load into mongo; using geojson2mongo.sh 

