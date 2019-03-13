
rm FILE
rm FILETMP
cat HEAD >> FILE
cat tmp/$1 | grep LineString > FILETMP
sed '$ s/.$//' FILETMP >> FILE
cat TAIL >> FILE

