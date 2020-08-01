#####################################
# Download tables from the database #
#####################################

# Get songs table
songs.df = dbGetQuery(wsf.con,
                      "SELECT SongID, SongName, SongNameForSort
                       FROM songs") %>%
  dplyr::select(song.id = SongID, song.name = SongName,
                song.name.sort = SongNameForSort)
Encoding(songs.df$song.name) = "UTF-8"
Encoding(songs.df$song.name.sort) = "UTF-8"

# Get topics table
topics.df = dbGetQuery(wsf.con,
                       "SELECT TopicID, TopicName
                         FROM topics") %>%
  dplyr::select(topic.id = TopicID, topic.name = TopicName)

# Get song instances table
song.instances.df = dbGetQuery(wsf.con,
                               "SELECT SongInstanceID, SongInstance,
                                       ArrangementID, SongID
                                FROM songinstances") %>%
  dplyr::select(song.instance.id = SongInstanceID, song.instance = SongInstance,
                arrangement.id = ArrangementID, song.id = SongID)
Encoding(song.instances.df$song.instance) = "UTF-8"

# Get lyrics table
lyrics.df = dbGetQuery(wsf.con,
                       "SELECT LyricsID, FirstLine, RefrainFirstLine,
                               CopyrightYear, LanguageID
                        FROM lyrics") %>%
  dplyr::select(lyrics.id = LyricsID, first.line = FirstLine,
                refrain.first.line = RefrainFirstLine,
                copyright.year = CopyrightYear, language.id = LanguageID)
Encoding(lyrics.df$first.line) = "UTF-8"
Encoding(lyrics.df$refrain.first.line) = "UTF-8"

# Get tunes table
tunes.df = dbGetQuery(wsf.con,
                      "SELECT TuneID, TuneName, CopyrightYear
                       FROM tunes") %>%
  dplyr::select(tune.id = TuneID, tune.name = TuneName,
                copyright.year = CopyrightYear)
Encoding(tunes.df$tune.name) = "UTF-8"

# Get arrangements table
arrangements.df = dbGetQuery(wsf.con,
                             "SELECT ArrangementID, ArrangementName,
                                     CopyrightYear
                              FROM arrangements") %>%
  dplyr::select(arrangement.id = ArrangementID,
                arrangement.name = ArrangementName,
                copyright.year = CopyrightYear)

# Get artists table
artists.df = dbGetQuery(wsf.con,
                        "SELECT ArtistID, LastName, FirstName, GenderName
                         FROM artists
                              JOIN genders
                              ON artists.GenderID = genders.GenderID") %>%
  mutate(artist.name = case_when(is.na(FirstName) ~ LastName,
                                 T ~ paste(FirstName, LastName,
                                           sep = " "))) %>%
  dplyr::select(artist.id = ArtistID, last.name = LastName,
                first.name = FirstName, artist.name, gender = GenderName)
Encoding(artists.df$last.name) = "UTF-8"
Encoding(artists.df$first.name) = "UTF-8"

# Get genders table
genders.df = dbGetQuery(wsf.con,
                        "SELECT GenderID, GenderName
                         FROM genders") %>%
  dplyr::select(gender.id = GenderID, gender.name = GenderName)

# Get languages table
languages.df = dbGetQuery(wsf.con,
                          "SELECT LanguageID, LanguageName
                           FROM languages") %>%
  dplyr::select(language.id = LanguageID, language.name = LanguageName)

# Get meters table
meters.df = dbGetQuery(wsf.con,
                       "SELECT MeterID, Meter
                        FROM meters") %>%
  dplyr::select(meter.id = MeterID, meter = Meter)

# Get arrangement types table
arrangement.types.df = dbGetQuery(wsf.con,
                                  "SELECT ArrangementTypeID, ArrangementType
                                   FROM arrangementtypes") %>%
  dplyr::select(arrangement.type.id = ArrangementTypeID,
                arrangement.type = ArrangementType)

# Get time signatures table
time.signatures.df = dbGetQuery(wsf.con,
                                "SELECT TimeSignatureID, TimeSignatureBeat,
                                        TimeSignatureMeasure
                                 FROM timesignatures") %>%
  mutate(time.signature.string = paste(TimeSignatureBeat, TimeSignatureMeasure,
                                       sep = "/")) %>%
  dplyr::select(time.signature.id = TimeSignatureID,
                time.signature.beat = TimeSignatureBeat,
                time.signature.measure = TimeSignatureMeasure,
                time.signature.string)

# Get pitches table
pitches.df = dbGetQuery(wsf.con,
                        "SELECT PitchID, PitchName
                         FROM pitches") %>%
  dplyr::select(pitch.id = PitchID, pitch.name = PitchName)

# Get accidentals table
accidentals.df = dbGetQuery(wsf.con,
                            "SELECT AccidentalID, AccidentalSymbol
                             FROM accidentals") %>%
  dplyr::select(accidental.id = AccidentalID,
                accidental.symbol = AccidentalSymbol)
# It's ridiculous that the actual symbols aren't coming through; I don't have
# the energy to figure out what's going on with the character encoding
accidentals.df = accidentals.df %>%
  mutate(accidental.symbol = case_when(accidental.id == 1 ~ "♯",
                                       accidental.id == 2 ~ "♭",
                                       accidental.id == 3 ~ "♮"))
Encoding(accidentals.df$accidental.symbol) = "UTF-8"

# Get modes table
modes.df = dbGetQuery(wsf.con,
                      "SELECT ModeID, ModeName
                       FROM modes") %>%
  dplyr::select(mode.id = ModeID, mode.name = ModeName)

# Get key signatures table
key.signatures.df = dbGetQuery(wsf.con,
                               "SELECT KeySignatureID, PitchID, AccidentalID,
                               ModeID
                               FROM keysignatures") %>%
  left_join(pitches.df, by = c("PitchID" = "pitch.id")) %>%
  left_join(accidentals.df, by = c("AccidentalID" = "accidental.id")) %>%
  left_join(modes.df, by = c("ModeID" = "mode.id")) %>%
  mutate(key.signature.string = paste(pitch.name,
                                      ifelse(AccidentalID == 3, "",
                                             accidental.symbol),
                                      case_when(ModeID == 1 ~ "",
                                                ModeID == 2 ~ "m",
                                                T ~ paste(" ",
                                                          mode.name,
                                                          sep = "")),
                                      sep = "")) %>%
  dplyr::select(key.signature.id = KeySignatureID, pitch.id = PitchID,
                pitch.name, accidental.id = AccidentalID, mode.id = ModeID,
                key.signature.string)

# Get scripture references table
scripture.references.df = dbGetQuery(wsf.con,
                                     "SELECT ScriptureReferenceID, BookID,
                                             Chapter, Verse
                                      FROM scripturereferences") %>%
  dplyr::select(scripture.reference.id = ScriptureReferenceID, book.id = BookID,
                chapter = Chapter, verse = Verse)

# Get books of the bible table
bible.books.df = dbGetQuery(wsf.con,
                            "SELECT BookID, BookName, BookAbbreviation
                             FROM booksofthebible") %>%
  dplyr::select(book.id = BookID, book.name = BookName,
                book.abbreviation = BookAbbreviation) %>%
  arrange(book.id)

# Get songbook entries table
songbook.entries.df = dbGetQuery(wsf.con,
                                 "SELECT SongbookEntryID, SongbookID,
                                         SongbookVolumeID, EntryNumber,
                                         SongInstanceID
                                  FROM songbookentries") %>%
  dplyr::select(songbook.entry.id = SongbookEntryID, songbook.id = SongbookID,
                songbook.volume.id = SongbookVolumeID,
                entry.number = EntryNumber, song.instance.id = SongInstanceID)

# Get songbooks table
songbooks.df = dbGetQuery(wsf.con,
                          "SELECT SongbookID, SongbookName, SongbookAbbreviation
                           FROM songbooks") %>%
  dplyr::select(songbook.id = SongbookID, songbook.name = SongbookName,
                songbook.abbreviation = SongbookAbbreviation)
Encoding(songbooks.df$songbook.name) = "UTF-8"
Encoding(songbooks.df$songbook.abbreviation) = "UTF-8"

# Get songbook volumes table
songbook.volumes.df = dbGetQuery(wsf.con,
                                 "SELECT SongbookVolumeID, SongbookVolume
                                  FROM songbookvolumes") %>%
  dplyr::select(songbook.volume.id = SongbookVolumeID,
                songbook.volume = SongbookVolume)

# Get copyright holders table
copyright.holders.df = dbGetQuery(wsf.con,
                                  "SELECT CopyrightHolderID,
                                          CopyrightHolderName
                                   FROM copyrightholders") %>%
  dplyr::select(copyright.holder.id = CopyrightHolderID,
                copyright.holder.name = CopyrightHolderName)
Encoding(copyright.holders.df$copyright.holder.name) = "UTF-8"

# Get worship slots table
worship.slots.df = dbGetQuery(wsf.con,
                              "SELECT WorshipSlotID, WorshipSlot,
                                      WorshipSlotOrder
                               FROM worshipslots") %>%
  dplyr::select(worship.slot.id = WorshipSlotID, worship.slot = WorshipSlot,
                worship.slot.order = WorshipSlotOrder) %>%
  arrange(worship.slot.order)

# Get bridge tables
songs.topics.df = dbGetQuery(wsf.con,
                             "SELECT SongID, TopicID
                              FROM songs_topics") %>%
  dplyr::select(song.id = SongID, topic.id = TopicID)
song.instances.lyrics.df = dbGetQuery(wsf.con,
                                      "SELECT SongInstanceID, LyricsID
                                       FROM songinstances_lyrics") %>%
  dplyr::select(song.instance.id = SongInstanceID, lyrics.id = LyricsID)
lyrics.artists.df = dbGetQuery(wsf.con,
                               "SELECT LyricsID, ArtistID
                                FROM lyrics_artists") %>%
  dplyr::select(lyrics.id = LyricsID, artist.id = ArtistID)
lyrics.meters.df = dbGetQuery(wsf.con,
                              "SELECT LyricsID, MeterID
                               FROM lyrics_meters") %>%
  dplyr::select(lyrics.id = LyricsID, meter.id = MeterID)
lyrics.scripture.references.df = dbGetQuery(wsf.con,
                                            "SELECT LyricsID,
                                                    ScriptureReferenceID
                                             FROM lyrics_scripturereferences") %>%
  dplyr::select(lyrics.id = LyricsID,
                scripture.reference.id = ScriptureReferenceID)
lyrics.copyright.holders.df = dbGetQuery(wsf.con,
                                         "SELECT LyricsID, CopyrightHolderID
                                          FROM lyrics_copyrightholders") %>%
  dplyr::select(lyrics.id = LyricsID, copyright.holder.id = CopyrightHolderID)
song.instances.tunes.df = dbGetQuery(wsf.con,
                                     "SELECT SongInstanceID, TuneID
                                      FROM songinstances_tunes") %>%
  dplyr::select(song.instance.id = SongInstanceID, tune.id = TuneID)
tunes.artists.df = dbGetQuery(wsf.con,
                              "SELECT TuneID, ArtistID
                               FROM tunes_artists") %>%
  dplyr::select(tune.id = TuneID, artist.id = ArtistID)
tunes.meters.df = dbGetQuery(wsf.con,
                             "SELECT TuneID, MeterID
                              FROM tunes_meters") %>%
  dplyr::select(tune.id = TuneID, meter.id = MeterID)
tunes.copyright.holders.df = dbGetQuery(wsf.con,
                                        "SELECT TuneID, CopyrightHolderID
                                         FROM tunes_copyrightholders") %>%
  dplyr::select(tune.id = TuneID, copyright.holder.id = CopyrightHolderID)
arrangements.artists.df = dbGetQuery(wsf.con,
                                     "SELECT ArrangementID, ArtistID
                                      FROM arrangements_artists") %>%
  dplyr::select(arrangement.id = ArrangementID, artist.id = ArtistID)
arrangements.arrangement.types.df = dbGetQuery(wsf.con,
                                               "SELECT ArrangementID,
                                                       ArrangementTypeID
                                                FROM arrangements_arrangementtypes") %>%
  dplyr::select(arrangement.id = ArrangementID,
                arrangement.type.id = ArrangementTypeID)
arrangements.copyright.holders.df = dbGetQuery(wsf.con,
                                               "SELECT ArrangementID,
                                                       CopyrightHolderID
                                                FROM arrangements_copyrightholders") %>%
  dplyr::select(arrangement.id = ArrangementID,
                copyright.holder.id = CopyrightHolderID)
song.instances.time.signatures.df = dbGetQuery(wsf.con,
                                               "SELECT SongInstanceID,
                                                       TimeSignatureID
                                                FROM songinstances_timesignatures") %>%
  dplyr::select(song.instance.id = SongInstanceID,
                time.signature.id = TimeSignatureID)
song.instances.key.signatures.df = dbGetQuery(wsf.con,
                                              "SELECT SongInstanceID,
                                                      KeySignatureID
                                               FROM songinstances_keysignatures") %>%
  dplyr::select(song.instance.id = SongInstanceID,
                key.signature.id = KeySignatureID)

#########################################
# Collect song info into pretty formats #
#########################################

# Function that turns a list of integers into a string with ranges.
ints.to.range = function(ints) {
  if(length(ints) == 0) {
    return("")
  }
  s = sort(ints)
  r = ""
  in.range = F
  for(i in 1:length(s)) {
    if(i == 1) {
      r = as.character(s[i])
    } else if(s[i] == s[i - 1] + 1) {
      if(!in.range) {
        r = paste(r, "-", sep = "")
      }
      in.range = T
      if(i == length(s)) {
        r = paste(r, s[i], sep = "")
      } else if(s[i] != s[i + 1] - 1) {
        r = paste(r, s[i], sep = "")
      }
    } else {
      in.range = F
      r = paste(r, s[i], sep = ", ")
    }
  }
  return(r)
}

# Get info for all songs
song.info.df = songs.df %>%
  left_join(songs.topics.df %>%
              inner_join(topics.df, by = c("topic.id")) %>%
              group_by(song.id) %>%
              arrange(topic.name) %>%
              summarise(topics = paste(topic.name, collapse = ", ")) %>%
              ungroup(),
            by = c("song.id")) %>%
  left_join(song.instances.df %>%
              inner_join(song.instances.lyrics.df,
                         by = c("song.instance.id")) %>%
              inner_join(lyrics.df,
                         by = c("lyrics.id")) %>%
              filter(!is.na(copyright.year)) %>%
              group_by(song.id) %>%
              summarise(last.lyrics.year = max(copyright.year)),
            by = c("song.id")) %>%
  left_join(song.instances.df %>%
              inner_join(song.instances.tunes.df,
                         by = c("song.instance.id")) %>%
              inner_join(tunes.df,
                         by = c("tune.id")) %>%
              filter(!is.na(copyright.year)) %>%
              group_by(song.id) %>%
              summarise(last.tune.year = max(copyright.year)),
            by = c("song.id")) %>%
  mutate(year = ifelse(is.na(last.lyrics.year) & is.na(last.tune.year),
                       NA, pmax(last.lyrics.year, last.tune.year)),
         decade = floor(year / 10) * 10) %>%
  dplyr::select(song.id, title = song.name, topics, year, decade)
Encoding(song.info.df$title) = "UTF-8"

# Get info for all song instances
song.instance.info.df = song.instances.df %>%
  left_join(song.instances.lyrics.df %>%
              inner_join(lyrics.df,
                         by = c("lyrics.id")) %>%
              filter(!is.na(copyright.year)) %>%
              group_by(song.instance.id) %>%
              summarise(last.lyrics.year = max(copyright.year)),
            by = c("song.instance.id")) %>%
  left_join(song.instances.tunes.df %>%
              inner_join(tunes.df,
                         by = c("tune.id")) %>%
              filter(!is.na(copyright.year)) %>%
              group_by(song.instance.id) %>%
              summarise(last.tune.year = max(copyright.year)),
            by = c("song.instance.id")) %>%
  left_join(songbook.entries.df %>%
              inner_join(songbooks.df, by = c("songbook.id")) %>%
              left_join(songbook.volumes.df, by = c("songbook.volume.id")) %>%
              mutate(entry.string = paste(songbook.abbreviation,
                                          case_when(is.na(songbook.volume) ~ "",
                                                    songbook.id == 6 ~ "",
                                                    T ~ paste(" ",
                                                              songbook.volume,
                                                              sep = "")),
                                          case_when(songbook.id == 12 ~ "",
                                                    is.na(entry.number) ~ "",
                                                    T ~ paste(" ", entry.number,
                                                              sep = "")),
                                          sep = "")) %>%
              group_by(song.instance.id) %>%
              arrange(songbook.abbreviation, entry.number) %>%
              summarise(songbook.entries = paste(entry.string,
                                                 collapse = ", ")) %>%
              ungroup(),
            by = c("song.instance.id")) %>%
  left_join(arrangements.arrangement.types.df %>%
              inner_join(arrangement.types.df,
                         by = c("arrangement.type.id")) %>%
              group_by(arrangement.id) %>%
              arrange(arrangement.type) %>%
              summarise(arrangement.types = paste(arrangement.type,
                                                  collapse = ", ")),
            by = c("arrangement.id")) %>%
  left_join(song.instances.key.signatures.df %>%
              inner_join(key.signatures.df,
                         by = c("key.signature.id")) %>%
              group_by(song.instance.id) %>%
              arrange(pitch.id, accidental.id, mode.id) %>%
              summarise(key.signatures = paste(key.signature.string,
                                               collapse = ", ")),
            by = c("song.instance.id")) %>%
  left_join(song.instances.time.signatures.df %>%
              inner_join(time.signatures.df,
                         by = c("time.signature.id")) %>%
              group_by(song.instance.id) %>%
              arrange(time.signature.measure, time.signature.beat) %>%
              summarise(time.signatures = paste(time.signature.string,
                                                collapse = ", ")),
            by = c("song.instance.id")) %>%
  left_join(song.instances.lyrics.df %>%
              inner_join(lyrics.scripture.references.df,
                         by = c("lyrics.id")) %>%
              inner_join(scripture.references.df,
                         by = c("scripture.reference.id")) %>%
              inner_join(bible.books.df,
                         by = c("book.id")) %>%
              dplyr::select(song.instance.id, book.id, book.abbreviation,
                            chapter, verse) %>%
              distinct() %>%
              group_by(song.instance.id, book.id, book.abbreviation, chapter) %>%
              summarise(verse.string = ints.to.range(verse)) %>%
              ungroup() %>%
              mutate(chapter.string = paste(chapter, verse.string, sep = ":")) %>%
              group_by(song.instance.id, book.id, book.abbreviation) %>%
              summarise(chapter.string = paste(chapter.string,
                                               collapse = ", ")) %>%
              ungroup() %>%
              mutate(book.string = paste(book.abbreviation, chapter.string,
                                         sep = " ")) %>%
              group_by(song.instance.id) %>%
              summarise(scripture.references = paste(book.string, collapse = "; ")),
            by = c("song.instance.id")) %>%
  left_join(song.instances.lyrics.df %>%
              inner_join(lyrics.artists.df,
                         by = c("lyrics.id")) %>%
              inner_join(artists.df,
                         by = c("artist.id")) %>%
              dplyr::select(song.instance.id, last.name, first.name,
                            artist.name) %>%
              distinct() %>%
              group_by(song.instance.id) %>%
              arrange(last.name, first.name) %>%
              summarise(lyricists = paste(artist.name, collapse = ", ")),
            by = c("song.instance.id")) %>%
  left_join(song.instances.tunes.df %>%
              inner_join(tunes.artists.df,
                         by = c("tune.id")) %>%
              inner_join(artists.df,
                         by = c("artist.id")) %>%
              dplyr::select(song.instance.id, last.name, first.name,
                            artist.name) %>%
              distinct() %>%
              group_by(song.instance.id) %>%
              arrange(last.name, first.name) %>%
              summarise(composers = paste(artist.name, collapse = ", ")),
            by = c("song.instance.id")) %>%
  left_join(arrangements.artists.df %>%
              inner_join(artists.df,
                         by = c("artist.id")) %>%
              dplyr::select(arrangement.id, last.name, first.name,
                            artist.name) %>%
              distinct() %>%
              group_by(arrangement.id) %>%
              arrange(last.name, first.name) %>%
              summarise(arrangers = paste(artist.name, collapse = ", ")),
            by = c("arrangement.id")) %>%
  left_join(song.instances.lyrics.df %>%
              inner_join(lyrics.df,
                         by = c("lyrics.id")) %>%
              inner_join(lyrics.copyright.holders.df,
                         by = c("lyrics.id")) %>%
              inner_join(copyright.holders.df,
                         by = c("copyright.holder.id")) %>%
              group_by(song.instance.id) %>%
              summarise(year.string = paste(unique(copyright.year),
                                            collapse = ", "),
                        holder.string = paste(copyright.holder.name,
                                              collapse = ", ")) %>%
              ungroup() %>%
              mutate(lyrics.copyright = paste(ifelse(holder.string == "public domain",
                                                     "", "© "),
                                              ifelse(year.string == "NA", "",
                                                     paste(year.string, " ",
                                                           sep = "")),
                                              holder.string,
                                              sep = "")),
            by = c("song.instance.id")) %>%
  left_join(song.instances.tunes.df %>%
              inner_join(tunes.df,
                         by = c("tune.id")) %>%
              inner_join(tunes.copyright.holders.df,
                         by = c("tune.id")) %>%
              inner_join(copyright.holders.df,
                         by = c("copyright.holder.id")) %>%
              group_by(song.instance.id) %>%
              summarise(year.string = paste(unique(copyright.year),
                                            collapse = ", "),
                        holder.string = paste(copyright.holder.name,
                                              collapse = ", ")) %>%
              ungroup() %>%
              mutate(tune.copyright = paste(ifelse(holder.string == "public domain",
                                                   "", "© "),
                                            ifelse(year.string == "NA", "",
                                                   paste(year.string, " ",
                                                         sep = "")),
                                            holder.string,
                                            sep = "")),
            by = c("song.instance.id")) %>%
  left_join(arrangements.df %>%
              inner_join(arrangements.copyright.holders.df,
                         by = c("arrangement.id")) %>%
              inner_join(copyright.holders.df,
                         by = c("copyright.holder.id")) %>%
              group_by(arrangement.id) %>%
              summarise(year.string = paste(unique(copyright.year),
                                            collapse = ", "),
                        holder.string = paste(copyright.holder.name,
                                              collapse = ", ")) %>%
              ungroup() %>%
              mutate(arrangement.copyright = paste(ifelse(holder.string == "public domain",
                                                         "", "© "),
                                                  ifelse(year.string == "NA",
                                                         "",
                                                         paste(year.string,
                                                               " ",
                                                               sep = "")),
                                                  holder.string,
                                                  sep = "")),
            by = c("arrangement.id")) %>%
  left_join(songbook.entries.df %>%
              group_by(song.instance.id) %>%
              summarise(num.entries = n()),
            by = c("song.instance.id")) %>%
  mutate(year = ifelse(is.na(last.lyrics.year) & is.na(last.tune.year),
                       NA, pmax(last.lyrics.year, last.tune.year)),
         decade = floor(year / 10) * 10) %>%
  dplyr::select(song.id, song.instance.id, title = song.instance, year, decade,
                songbook.entries, arrangement.types, key.signatures,
                time.signatures, scripture.references, lyricists, composers,
                arrangers, lyrics.copyright, tune.copyright,
                arrangement.copyright, num.entries)
Encoding(song.instance.info.df$title) = "UTF-8"
Encoding(song.instance.info.df$key.signatures) = "UTF-8"
Encoding(song.instance.info.df$lyricists) = "UTF-8"
Encoding(song.instance.info.df$composers) = "UTF-8"
Encoding(song.instance.info.df$arrangers) = "UTF-8"
Encoding(song.instance.info.df$lyrics.copyright) = "UTF-8"
Encoding(song.instance.info.df$tune.copyright) = "UTF-8"
Encoding(song.instance.info.df$arrangement.copyright) = "UTF-8"

# Get lyrics first lines for all song instances
all.song.instance.lyrics.df = song.instances.lyrics.df %>%
  inner_join(lyrics.df, by = c("lyrics.id")) %>%
  dplyr::select(song.instance.id, first.line, refrain.first.line) %>%
  gather(source, lyrics.line, -song.instance.id)
