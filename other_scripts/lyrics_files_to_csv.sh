rm ../worship_song_database/data_text_files/all_lyrics.csv
while read -r line
do
    id=`echo $line | sed 's/,.*//g'`
    filename=`echo $line | sed 's/[0-9]*,/lyrics_files\//g'`
    if [[ $id == [0-9]* ]]
    then
	    cat $filename | sed "s/<lyrics>/<lyrics id=\"$id\">/g" | sed "s/<\/lyrics>/<XXX>/g" | sed "s/<\([a-z0-9]*\)>/<p class=\"lyrics-\1\">/g" | sed "s/<\/[a-z0-9]*>/<\/p>/g" | sed "s/<XXX>/<\/lyrics>/g" | sed "s/\([^>]\)$/\1 <br\/>/g" | sed "s/\"/\"\"/g" | tr '\r\n' ' ' | sed "s/^\(.\)/$id,\"\1/g" | sed "s/\(.\)$/\1\"/g"
	echo $n
    fi
done < ../worship_song_database/data_text_files/lyrics_files.csv >> ../worship_song_database/data_text_files/all_lyrics.csv
