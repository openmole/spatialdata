ps -ef | grep spatialdata | awk -F" " '{print "kill -9 "$2}' | sh
