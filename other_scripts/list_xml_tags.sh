grep -ohP "</?[^<>]+>" ../lyrics_files/*.xml | tr -d '/<>' | sort | uniq
