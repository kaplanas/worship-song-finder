#####################################
# Download tables from the database #
#####################################

# Get songs table
songs.tab = dbGetQuery(wsf_con, "SELECT SongID, SongName, SongNameForSort FROM songs")
Encoding(songs.tab$SongName) = "UTF-8"
Encoding(songs.tab$SongNameForSort) = "UTF-8"

# Get topics table
topics.tab = dbGetQuery(wsf_con, "SELECT TopicID, TopicName FROM topics")

# Get song instances table
songinstances.tab = dbGetQuery(wsf_con, "SELECT SongInstanceID, SongInstance, ArrangementID, SongID FROM songinstances")
Encoding(songinstances.tab$SongInstance) = "UTF-8"

# Get lyrics table
lyrics.tab = dbGetQuery(wsf_con, "SELECT LyricsID, FirstLine, RefrainFirstLine, CopyrightYear, LanguageID, FileName FROM lyrics")
Encoding(lyrics.tab$FirstLine) = "UTF-8"
Encoding(lyrics.tab$RefrainFirstLine) = "UTF-8"

# Get artists table
artists.tab = dbGetQuery(wsf_con, "SELECT ArtistID, LastName, FirstName, Gender FROM artists")
Encoding(artists.tab$LastName) = "UTF-8"
Encoding(artists.tab$FirstName) = "UTF-8"

# Get genders table
genders.tab = dbGetQuery(wsf_con, "SELECT GenderID, GenderName FROM genders")

# Get languages table
languages.tab = dbGetQuery(wsf_con, "SELECT LanguageID, LanguageName FROM languages")

# Get meters table
meters.tab = dbGetQuery(wsf_con, "SELECT MeterID, Meter FROM meters")

# Get arrangement types table
arrangementtypes.tab = dbGetQuery(wsf_con, "SELECT ArrangementTypeID, ArrangementType FROM arrangementtypes")

# Get time signatures table
timesignatures.tab = dbGetQuery(wsf_con, "SELECT TimeSignatureID, TimeSignatureBeat, TimeSignatureMeasure FROM timesignatures")

# Get key signatures table
keysignatures.tab = dbGetQuery(wsf_con, "SELECT KeySignatureID, PitchID, AccidentalID, ModeID FROM keysignatures")

# Get pitches table
pitches.tab = dbGetQuery(wsf_con, "SELECT PitchID, PitchName FROM pitches")

# Get accidentals table
accidentals.tab = dbGetQuery(wsf_con, "SELECT AccidentalID, AccidentalSymbol FROM accidentals")
Encoding(accidentals.tab$AccidentalSymbol) = "UTF-8"

# Get modes table
modes.tab = dbGetQuery(wsf_con, "SELECT ModeID, ModeName FROM modes")

# Get scripture references table
scripturereferences.tab = dbGetQuery(wsf_con, "SELECT ScriptureReferenceID, BookID, Chapter, Verse FROM scripturereferences")

# Get books of the bible table
booksofthebible.tab = dbGetQuery(wsf_con, "SELECT BookID, BookName, BookAbbreviation FROM booksofthebible")

# Get songbook entries table
songbookentries.tab = dbGetQuery(wsf_con, "SELECT SongbookEntryID, SongbookID, SongInstanceID FROM songbookentries")

# Get songbooks table
songbooks.tab = dbGetQuery(wsf_con, "SELECT SongbookID, SongbookName, SongbookAbbreviation FROM songbooks")
Encoding(songbooks.tab$SongbookName) = "UTF-8"
Encoding(songbooks.tab$SongbookAbbreviation) = "UTF-8"

# Get worship slots table
worshipslots.tab = dbGetQuery(wsf_con, "SELECT WorshipSlotID, WorshipSlot, WorshipSlotOrder FROM worshipslots") %>%
  arrange(WorshipSlotOrder)

# Get special requests tables
specialrequesters.tab = dbGetQuery(wsf_con, "SELECT SpecialRequesterID, FirstName, LastName FROM specialrequesters")
specialrequests.tab = dbGetQuery(wsf_con, "SELECT SongID, SpecialRequesterID FROM specialrequests")

# Get bridge tables
songs.topics.tab = dbGetQuery(wsf_con, "SELECT SongID, TopicID FROM songs_topics")
songinstances.lyrics.tab = dbGetQuery(wsf_con, "SELECT SongInstanceID, LyricsID FROM songinstances_lyrics")
lyrics.artists.tab = dbGetQuery(wsf_con, "SELECT LyricsID, ArtistID FROM lyrics_artists")
lyrics.meters.tab = dbGetQuery(wsf_con, "SELECT LyricsID, MeterID FROM lyrics_meters")
lyrics.scripturereferences.tab = dbGetQuery(wsf_con, "SELECT LyricsID, ScriptureReferenceID FROM lyrics_scripturereferences")
lyrics.copyrightholders.tab = dbGetQuery(wsf_con, "SELECT LyricsID, CopyrightHolderID FROM lyrics_copyrightholders")
songinstances.tunes.tab = dbGetQuery(wsf_con, "SELECT SongInstanceID, TuneID FROM songinstances_tunes")
tunes.artists.tab = dbGetQuery(wsf_con, "SELECT TuneID, ArtistID FROM tunes_artists")
tunes.meters.tab = dbGetQuery(wsf_con, "SELECT TuneID, MeterID FROM tunes_meters")
arrangements.artists.tab = dbGetQuery(wsf_con, "SELECT ArrangementID, ArtistID FROM arrangements_artists")
arrangements.arrangementtypes.tab = dbGetQuery(wsf_con, "SELECT ArrangementID, ArrangementTypeID FROM arrangements_arrangementtypes")
songinstances.timesignatures.tab = dbGetQuery(wsf_con, "SELECT SongInstanceID, TimeSignatureID FROM songinstances_timesignatures")
songinstances.keysignatures.tab = dbGetQuery(wsf_con, "SELECT SongInstanceID, KeySignatureID FROM songinstances_keysignatures")

#########################################
# Collect song info into pretty formats #
#########################################

# Get info for all songs
allSongInfoQuery = "SELECT SongID, Title, Topics, Year FROM song_info"
allSongInfo = dbGetQuery(wsf_con, allSongInfoQuery)
Encoding(allSongInfo$Title) = "UTF-8"
allSongInfo$Decade = floor(allSongInfo$Year / 10) * 10

# Get info for all song instances
allSongInstanceInfoQuery = "SELECT SongID, SongInstanceID, Title, Year, SongbookEntries, ArrangementTypes, KeySignatures, TimeSignatures, ScriptureReferences, Lyricists, Composers, Arrangers, LyricsCopyright, TuneCopyright, ArrangementCopyright FROM songinstance_info ORDER BY NumSongbookEntries DESC, TitleForSort"
allSongInstanceInfo = dbGetQuery(wsf_con, allSongInstanceInfoQuery)
Encoding(allSongInstanceInfo$Title) = "UTF-8"
Encoding(allSongInstanceInfo$KeySignatures) = "UTF-8"
Encoding(allSongInstanceInfo$Lyricists) = "UTF-8"
Encoding(allSongInstanceInfo$Composers) = "UTF-8"
Encoding(allSongInstanceInfo$Arrangers) = "UTF-8"
Encoding(allSongInstanceInfo$LyricsCopyright) = "UTF-8"
Encoding(allSongInstanceInfo$TuneCopyright) = "UTF-8"
Encoding(allSongInstanceInfo$ArrangementCopyright) = "UTF-8"
allSongInstanceInfo$Decade = floor(allSongInstanceInfo$Year / 10) * 10

# Get lyrics first lines for all song instances
allSongInstanceLyrics = inner_join(songinstances.lyrics.tab, lyrics.tab) %>%
  select(SongInstanceID, FirstLine, RefrainFirstLine) %>%
  gather(Source, LyricsLine, -SongInstanceID)

########################
# Get lyrics full text #
########################

# lyrics.full.text = readtext("song_database_lyrics/*.xml",
#                             text_field = "(//verse | //refrain | //prerefrain | //bridge | //soprano | //alto | //tenor | //bass)",
#                             verbosity = 1)
# lyrics.full.text$text = stri_replace_all_regex(lyrics.full.text$text, "^\n|\n$", "")
# lyrics.full.text$text = stri_replace_all_regex(lyrics.full.text$text, "\n\n", "\n")
# lyrics.full.text = corpus(lyrics.full.text)
# docvars(lyrics.full.text, "language") = NA
# docvars(lyrics.full.text, "year") = NA
# for(i in 1:nrow(lyrics.full.text$documents)) {
#   lyrics.file = docvars(lyrics.full.text)$doc_id[i]
#   #print(lyrics.file)
#   lyrics.entry = lyrics.tab[lyrics.tab$FileName == lyrics.file & !is.na(lyrics.tab$FileName),]
#   #print(lyrics.entry)
#   docnames(lyrics.full.text)[i] = paste("LyricsID", lyrics.entry$LyricsID, sep = "")
#   docvars(lyrics.full.text)$language[i] = languages.tab$LanguageName[languages.tab$LanguageID == lyrics.entry$LanguageID]
#   docvars(lyrics.full.text)$year[i] = lyrics.entry$CopyrightYear
#   #print("")
# }
# rm(i, lyrics.file, lyrics.entry)
# lyrics.full.text.english = corpus_subset(lyrics.full.text, language == "English")
# english.stopwords = stopwords("english")
# english.stopwords = c(english.stopwords, c("o", "us", "shall", "thee", "can", "thy", "thou", "may", "ye", "thine", "art", "hath", "hast", "doth", "dost", "doeth", "doest", "wouldst", "wouldest", "shouldst", "shouldest", "couldst", "couldest", "oughtst", "oughtest", "whence", "willst", "wilt"))
# lyrics.full.text.english.dfm = dfm(lyrics.full.text.english, remove_punct = T, remove = english.stopwords, stem = T)
