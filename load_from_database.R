#### Download tables from the database ####

# Get table of songs
songs.sql = "SELECT SongID, SongName
             FROM songs"
songs.df = dbGetQuery(wsf.con, songs.sql) %>%
  dplyr::select(song.id = SongID, song.name = SongName) %>%
  mutate(song.name.sort = gsub("^['\"¡¿]", "", song.name))
Encoding(songs.df$song.name) = "UTF-8"
Encoding(songs.df$song.name.sort) = "UTF-8"

# Get table of song instances
song.instances.sql = "SELECT songinstances.SongInstanceID, SongInstance,
                             songinstances.ArrangementID, SongID,
                             LastLyricsYear, LyricsCopyright, LastTuneYear,
                             TuneCopyright, ArrangementCopyright
                      FROM songinstances
                           LEFT JOIN (SELECT SongInstanceID,
                                             MAX(CopyrightYear) AS LastLyricsYear,
                                             GROUP_CONCAT(CONCAT('(C) ',
                                                                 CopyrightYear,
                                                                 ' ',
                                                                 CopyrightHolderNames)
								                                          ORDER BY CopyrightYear
                                                          SEPARATOR '; ') AS LyricsCopyright
                                      FROM songinstances_lyrics
                                           JOIN (SELECT lyrics.LyricsID,
                                                        CopyrightYear,
                                                        GROUP_CONCAT(CASE WHEN copyrightholders.CopyrightHolderID <> 1
                                                                               THEN CopyrightHolderName
                                                                     END
                                                                     ORDER BY CopyrightHolderName
                                                                     SEPARATOR ', ') AS CopyrightHolderNames
                                                 FROM lyrics
                                                      JOIN lyrics_copyrightholders
                                                      ON lyrics.LyricsID = lyrics_copyrightholders.LyricsID
                                                      JOIN copyrightholders
                                                      ON lyrics_copyrightholders.CopyrightHolderID = copyrightholders.CopyrightHolderID
                                                 GROUP BY lyrics.LyricsID,
                                                          CopyrightYear) lyrics
                                           ON songinstances_lyrics.LyricsID = lyrics.LyricsID
                                      GROUP BY SongInstanceID) lyrics
                           ON songinstances.SongInstanceID = lyrics.SongInstanceID
                           LEFT JOIN (SELECT SongInstanceID,
                                             MAX(CopyrightYear) AS LastTuneYear,
                                             GROUP_CONCAT(CONCAT('(C) ',
                                                                 CopyrightYear,
                                                                 ' ',
                                                                 CopyrightHolderNames)
                                                          ORDER BY CopyrightYear
                                                          SEPARATOR '; ') AS TuneCopyright
                                      FROM songinstances_tunes
                                           JOIN (SELECT tunes.TuneID,
                                                        CopyrightYear,
                                                        GROUP_CONCAT(CASE WHEN copyrightholders.CopyrightHolderID <> 1
                                                                               THEN CopyrightHolderName
                                                                     END
                                                                     ORDER BY CopyrightHolderName
                                                                     SEPARATOR ', ') AS CopyrightHolderNames
                                                 FROM tunes
                                                      JOIN tunes_copyrightholders
                                                      ON tunes.TuneID = tunes_copyrightholders.TuneID
                                                      JOIN copyrightholders
                                                      ON tunes_copyrightholders.CopyrightHolderID = copyrightholders.CopyrightHolderID
                                                 GROUP BY tunes.TuneID,
                                                          CopyrightYear) tunes
                                           ON songinstances_tunes.TuneID = tunes.TuneID
                                      GROUP BY SongInstanceID) tunes
                           ON songinstances.SongInstanceID = tunes.SongInstanceID
                           LEFT JOIN (SELECT ArrangementID,
                                             GROUP_CONCAT(CONCAT('(C) ',
                                                                 CopyrightYear,
                                                                 ' ',
                                                                 CopyrightHolderNames)
                                                          ORDER BY CopyrightYear
                                                          SEPARATOR '; ') AS ArrangementCopyright
                                      FROM (SELECT arrangements.ArrangementID,
                                                   CopyrightYear,
                                                   GROUP_CONCAT(CASE WHEN copyrightholders.CopyrightHolderID <> 1
                                                                          THEN CopyrightHolderName
                                                                END
                                                                ORDER BY CopyrightHolderName
                                                                SEPARATOR ', ') AS CopyrightHolderNames
                                            FROM arrangements
                                                 JOIN arrangements_copyrightholders
                                                 ON arrangements.ArrangementID = arrangements_copyrightholders.ArrangementID
                                                 JOIN copyrightholders
                                                 ON arrangements_copyrightholders.CopyrightHolderID = copyrightholders.CopyrightHolderID
                                            GROUP BY arrangements.ArrangementID,
                                                     CopyrightYear) a
                                      GROUP BY ArrangementID) arrangements
                           ON songinstances.ArrangementID = arrangements.ArrangementID"
song.instances.df = dbGetQuery(wsf.con, song.instances.sql) %>%
  mutate(lyrics.copyright = gsub("\\(C\\)", "©", LyricsCopyright),
         tune.copyright = gsub("\\(C\\)", "©", TuneCopyright),
         arrangement.copyright = gsub("\\(C\\)", "©", ArrangementCopyright)) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.instance = SongInstance,
                arrangement.id = ArrangementID, song.id = SongID,
                last.lyrics.year = LastLyricsYear, lyrics.copyright,
                last.tune.year = LastTuneYear, tune.copyright,
                arrangement.copyright)
Encoding(song.instances.df$song.instance) = "UTF-8"
Encoding(song.instances.df$lyrics.copyright) = "UTF-8"
Encoding(song.instances.df$tune.copyright) = "UTF-8"
Encoding(song.instances.df$arrangement.copyright) = "UTF-8"

# Get table of artists
artists.sql = "SELECT ArtistID, LastName, FirstName, GenderName
               FROM artists
                    LEFT JOIN genders
                    ON artists.GenderID = genders.GenderID"
artists.df = dbGetQuery(wsf.con, artists.sql) %>%
  mutate(artist.name = case_when(is.na(FirstName) ~ LastName,
                                 FirstName == "" ~ LastName,
                                 T ~ paste(FirstName, LastName,
                                           sep = " "))) %>%
  dplyr::select(artist.id = ArtistID, last.name = LastName,
                first.name = FirstName, artist.name, gender = GenderName)
Encoding(artists.df$last.name) = "UTF-8"
Encoding(artists.df$first.name) = "UTF-8"

# Get table that connects song instances and artists
song.instances.artists.sql = "SELECT DISTINCT songinstances.SongInstanceID,
                                     SongID, ArtistID, 'lyricist' AS Role
                              FROM songinstances
                                   JOIN songinstances_lyrics
                                   ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
	                                 JOIN lyrics_artists
	                                 ON songinstances_lyrics.LyricsID = lyrics_artists.LyricsID
                              UNION ALL
                              SELECT DISTINCT songinstances.SongInstanceID,
                                     SongID, ArtistID, 'composer' AS Role
                              FROM songinstances
                                   JOIN songinstances_tunes
                                   ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID
	                                 JOIN tunes_artists
	                                 ON songinstances_tunes.TuneID = tunes_artists.TuneID
                              UNION ALL
                              SELECT DISTINCT songinstances.SongInstanceID,
                                     SongID, ArtistID, 'arranger' AS Role
                              FROM songinstances
                                   JOIN arrangements_artists
                                   ON songinstances.ArrangementID = arrangements_artists.ArrangementID"
song.instances.artists.df = dbGetQuery(wsf.con, song.instances.artists.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                artist.id = ArtistID, role = Role)

# Get table of topics
topics.df = dbGetQuery(wsf.con,
                       "SELECT TopicID, TopicName
                        FROM topics") %>%
  dplyr::select(topic.id = TopicID, topic.name = TopicName)

# Get table that connects songs and topics
songs.topics.sql = "SELECT SongID, TopicID
                    FROM songs_topics"
songs.topics.df = dbGetQuery(wsf.con, songs.topics.sql) %>%
  dplyr::select(song.id = SongID, topic.id = TopicID)

# Get table of books of the Bible
bible.books.sql = "SELECT BookID, BookName, BookAbbreviation
                   FROM booksofthebible"
bible.books.df = dbGetQuery(wsf.con, bible.books.sql) %>%
  dplyr::select(book.id = BookID, book.name = BookName,
                book.abbreviation = BookAbbreviation)

# Get table of scripture references
scripture.references.sql = "SELECT ScriptureReferenceID, booksofthebible.BookID,
                                   BookName, BookAbbreviation, Chapter, Verse
                            FROM scripturereferences
                                 JOIN booksofthebible
                                 ON scripturereferences.BookID = booksofthebible.BookID"
scripture.references.df = dbGetQuery(wsf.con, scripture.references.sql) %>%
  dplyr::select(scripture.reference.id = ScriptureReferenceID, book.id = BookID,
                book.name = BookName, book.abbreviation = BookAbbreviation,
                chapter = Chapter, verse = Verse)

# Get table that connects song instances and scripture references
song.instances.scripture.references.sql = "SELECT DISTINCT songinstances.SongInstanceID,
                                                  SongID, ScriptureReferenceID
                                           FROM songinstances
                                                JOIN songinstances_lyrics
                                                ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
                                                JOIN lyrics_scripturereferences
                                                ON songinstances_lyrics.LyricsID = lyrics_scripturereferences.LyricsID"
song.instances.scripture.references.df = dbGetQuery(wsf.con,
                                                    song.instances.scripture.references.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                scripture.reference.id = ScriptureReferenceID)

# Get table of languages
languages.sql = "SELECT LanguageID, LanguageName
                 FROM languages"
languages.df = dbGetQuery(wsf.con, languages.sql) %>%
  dplyr::select(language.id = LanguageID, language.name = LanguageName)

# Get table that connects song instances and languages
song.instances.languages.sql = "SELECT DISTINCT songinstances.SongInstanceID,
                                       SongID, LanguageID
                                FROM songinstances
                                     JOIN songinstances_lyrics
                                     ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
                                     JOIN lyrics
                                     ON songinstances_lyrics.LyricsID = lyrics.LyricsID"
song.instances.languages.df = dbGetQuery(wsf.con,
                                         song.instances.languages.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                language.id = LanguageID)

# Get table of songbooks
songbooks.sql = "SELECT SongbookID, SongbookName, SongbookAbbreviation
                 FROM songbooks"
songbooks.df = dbGetQuery(wsf.con, songbooks.sql) %>%
  dplyr::select(songbook.id = SongbookID, songbook.name = SongbookName,
                songbook.abbreviation = SongbookAbbreviation)
Encoding(songbooks.df$songbook.name) = "UTF-8"
Encoding(songbooks.df$songbook.abbreviation) = "UTF-8"

# Get table that connects song instances and songbooks
song.instances.songbooks.sql = "SELECT songinstances.SongInstanceID, SongID,
                                       songbooks.SongbookID, SongbookName,
                                       SongbookAbbreviation,
                                       songbookvolumes.SongbookVolumeID,
                                       SongbookVolume, EntryNumber
                                FROM songinstances
                                     JOIN songbookentries
                                     ON songinstances.SongInstanceID = songbookentries.SongInstanceID
                                     JOIN songbooks
                                     ON songbookentries.SongbookID = songbooks.SongbookID
                                     LEFT JOIN songbookvolumes
                                     ON songbookentries.SongbookVolumeID = songbookvolumes.SongbookVolumeID"
song.instances.songbooks.df = dbGetQuery(wsf.con, song.instances.songbooks.sql) %>%
  mutate(entry.string = paste(SongbookAbbreviation,
                              case_when(is.na(SongbookVolume) ~ "",
                                        SongbookID %in% c(0, 6) ~ "",
                                        T ~ paste(" ",
                                                  SongbookVolume,
                                                  sep = "")),
                              case_when(SongbookID == 12 ~ "",
                                        is.na(EntryNumber) ~ "",
                                        EntryNumber == "" ~ "",
                                        T ~ paste(" ", EntryNumber,
                                                  sep = "")),
                              sep = "")) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                songbook.id = SongbookID, songbook.name = SongbookName,
                songbook.abbreviation = SongbookAbbreviation,
                entry.number = EntryNumber, entry.string)
Encoding(song.instances.songbooks.df$songbook.name) = "UTF-8"
Encoding(song.instances.songbooks.df$songbook.abbreviation) = "UTF-8"

# Get table of arrangement types
arrangement.types.sql = "SELECT ArrangementTypeID, ArrangementType
                         FROM arrangementtypes"
arrangement.types.df = dbGetQuery(wsf.con, arrangement.types.sql) %>%
  dplyr::select(arrangement.type.id = ArrangementTypeID,
                arrangement.type = ArrangementType)

# Get table that connects song instances and arrangement types
song.instances.arrangement.types.sql = "SELECT DISTINCT SongInstanceID, SongID,
                                               ArrangementTypeID
                                        FROM songinstances
                                             JOIN arrangements_arrangementtypes
                                             ON songinstances.ArrangementID = arrangements_arrangementtypes.ArrangementID"
song.instances.arrangement.types.df = dbGetQuery(wsf.con,
                                                 song.instances.arrangement.types.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                arrangement.type.id = ArrangementTypeID)

# Get table of key signatures
key.signatures.sql = "SELECT KeySignatureID, PitchName,
                             accidentals.AccidentalID, AccidentalSymbol,
                             modes.ModeID, ModeName
                      FROM keysignatures
                           JOIN pitches
                           ON keysignatures.PitchID = pitches.PitchID
                           JOIN accidentals
                           ON keysignatures.AccidentalID = accidentals.AccidentalID
                           JOIN modes
                           ON keysignatures.ModeID = modes.ModeID"
key.signatures.df = dbGetQuery(wsf.con, key.signatures.sql) %>%
  mutate(key.signature.string = paste(PitchName,
                                      ifelse(AccidentalID == 3, "",
                                             AccidentalSymbol),
                                      case_when(ModeID == 1 ~ "",
                                                ModeID == 2 ~ "m",
                                                T ~ paste(" ",
                                                          ModeName,
                                                          sep = "")),
                                      sep = "")) %>%
  dplyr::select(key.signature.id = KeySignatureID, pitch.name = PitchName,
                accidental.id = AccidentalID,
                accidental.symbol = AccidentalSymbol, mode.id = ModeID,
                mode.name = ModeName, key.signature.string)
Encoding(key.signatures.df$accidental.symbol) = "UTF-8"
Encoding(key.signatures.df$key.signature.string) = "UTF-8"

# Get table that connects song instances and key signatures
song.instances.key.signatures.sql = "SELECT songinstances.SongInstanceID,
                                            SongID, KeySignatureID
                                     FROM songinstances
                                          JOIN songinstances_keysignatures
                                          ON songinstances.SongInstanceID = songinstances_keysignatures.SongInstanceID"
song.instances.key.signatures.df = dbGetQuery(wsf.con,
                                              song.instances.key.signatures.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                key.signature.id = KeySignatureID)

# Get table of time signatures
time.signatures.sql = "SELECT TimeSignatureID, TimeSignatureBeat,
                              TimeSignatureMeasure
                       FROM timesignatures"
time.signatures.df = dbGetQuery(wsf.con, time.signatures.sql) %>%
  mutate(time.signature.string = paste(TimeSignatureBeat, TimeSignatureMeasure,
                                       sep = "/")) %>%
  dplyr::select(time.signature.id = TimeSignatureID,
                time.signature.beat = TimeSignatureBeat,
                time.signature.measure = TimeSignatureMeasure,
                time.signature.string)

# Get table that connects song instances and time signatures
song.instances.time.signatures.sql = "SELECT songinstances.SongInstanceID,
                                             SongID, TimeSignatureID
                                      FROM songinstances
                                           JOIN songinstances_timesignatures
                                           ON songinstances.SongInstanceID = songinstances_timesignatures.SongInstanceID"
song.instances.time.signatures.df = dbGetQuery(wsf.con,
                                               song.instances.time.signatures.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                time.signature.id = TimeSignatureID)

# Get table of meters
meters.sql = "SELECT MeterID, Meter
              FROM meters"
meters.df = dbGetQuery(wsf.con, meters.sql) %>%
  dplyr::select(meter.id = MeterID, meter = Meter)

# Get table that connects song instances and meters
song.instances.meters.sql = "SELECT songinstances.SongInstanceID, SongID, MeterID
                             FROM songinstances
                                  JOIN songinstances_lyrics
                                  ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
                                  JOIN lyrics_meters
                                  ON songinstances_lyrics.LyricsID = lyrics_meters.LyricsID
                             UNION DISTINCT
                             SELECT songinstances.SongInstanceID, SongID, MeterID
                             FROM  songinstances
                                   JOIN songinstances_tunes
                                   ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID
                                   JOIN tunes_meters
                                   ON songinstances_tunes.TuneID = tunes_meters.TuneID"
song.instances.meters.df = dbGetQuery(wsf.con, song.instances.meters.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                meter.id = MeterID)

# Get lyrics first lines for all song instances
lyrics.first.lines.sql = "SELECT SongInstanceID, FirstLine, RefrainFirstLine
                          FROM songinstances_lyrics
                               JOIN lyrics
                               ON songinstances_lyrics.LyricsID = lyrics.LyricsID"
lyrics.first.lines.df = dbGetQuery(wsf.con, lyrics.first.lines.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, first.line = FirstLine,
                refrain.first.line = RefrainFirstLine) %>%
  pivot_longer(cols = c(first.line, refrain.first.line),
               names_to = "source",
               values_to = "lyrics.line")
Encoding(lyrics.first.lines.df$lyrics.line) = "UTF-8"

# Get table of worship slots
if(version == "ctcc") {
  worship.slots.sql = "SELECT WorshipSlotID, WorshipSlot, WorshipSlotOrder
                       FROM worshipslots"
  worship.slots.df = dbGetQuery(wsf.con, worship.slots.sql) %>%
    dplyr::select(worship.slot.id = WorshipSlotID, worship.slot = WorshipSlot,
                  worship.slot.order = WorshipSlotOrder)
}

#### Collect song info into pretty formats ####

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

# Get info for all song instances
song.instance.info.df = song.instances.df %>%
  left_join(song.instances.songbooks.df %>%
              group_by(song.instance.id) %>%
              arrange(songbook.abbreviation, entry.number) %>%
              summarise(songbook.entries = paste(entry.string,
                                                 collapse = ", ")) %>%
              ungroup(),
            by = c("song.instance.id")) %>%
  left_join(song.instances.arrangement.types.df %>%
              inner_join(arrangement.types.df, by = "arrangement.type.id") %>%
              group_by(song.instance.id) %>%
              arrange(arrangement.type) %>%
              summarise(arrangement.types = paste(arrangement.type,
                                                  collapse = ", ")),
            by = "song.instance.id") %>%
  left_join(song.instances.key.signatures.df %>%
              inner_join(key.signatures.df, by = "key.signature.id") %>%
              group_by(song.instance.id) %>%
              arrange(pitch.name, accidental.id, mode.id) %>%
              summarise(key.signatures = paste(key.signature.string,
                                               collapse = ", ")),
            by = "song.instance.id") %>%
  left_join(song.instances.time.signatures.df %>%
              inner_join(time.signatures.df, by = "time.signature.id") %>%
              group_by(song.instance.id) %>%
              arrange(time.signature.measure, time.signature.beat) %>%
              summarise(time.signatures = paste(time.signature.string,
                                                collapse = ", ")),
            by = "song.instance.id") %>%
  left_join(song.instances.scripture.references.df %>%
              inner_join(scripture.references.df,
                         by = "scripture.reference.id") %>%
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
            by = "song.instance.id") %>%
  left_join(song.instances.artists.df %>%
              filter(role != "lyricist") %>%
              inner_join(artists.df, by = "artist.id") %>%
              dplyr::select(song.instance.id, role, last.name, first.name,
                            artist.name) %>%
              distinct() %>%
              group_by(song.instance.id, role) %>%
              arrange(last.name, first.name) %>%
              summarise(artists = paste(artist.name, collapse = ", ")) %>%
              ungroup() %>%
              pivot_wider(names_from = role, values_from = artists) %>%
              dplyr::select(song.instance.id, composers = composer,
                            arrangers = arranger),
            by = "song.instance.id") %>%
  left_join(song.instances.songbooks.df %>%
              group_by(song.instance.id) %>%
              summarise(num.entries = n()),
            by = c("song.instance.id")) %>%
  mutate(year = ifelse(is.na(last.lyrics.year) & is.na(last.tune.year),
                       NA, pmax(last.lyrics.year, last.tune.year)),
         decade = floor(year / 10) * 10) %>%
  dplyr::select(song.id, song.instance.id, title = song.instance, year, decade,
                songbook.entries, arrangement.types, key.signatures,
                time.signatures, scripture.references, composers, arrangers,
                lyrics.copyright, tune.copyright, arrangement.copyright,
                num.entries)
Encoding(song.instance.info.df$title) = "UTF-8"
Encoding(song.instance.info.df$songbook.entries) = "UTF-8"
Encoding(song.instance.info.df$key.signatures) = "UTF-8"
Encoding(song.instance.info.df$composers) = "UTF-8"
Encoding(song.instance.info.df$arrangers) = "UTF-8"
Encoding(song.instance.info.df$lyrics.copyright) = "UTF-8"
Encoding(song.instance.info.df$tune.copyright) = "UTF-8"
Encoding(song.instance.info.df$arrangement.copyright) = "UTF-8"

# Add translators and alterers to lyrics artists
song.instances.lyrics.sql = "SELECT SongInstanceID, LyricsID
                             FROM songinstances_lyrics"
song.instances.lyrics.df = dbGetQuery(wsf.con, song.instances.lyrics.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, lyrics.id = LyricsID)
lyrics.translations.sql = "SELECT lyrics.LyricsID, TranslatedFromID,
                                  CASE WHEN lyrics.LanguageID = sources.LanguageID
                                            THEN 'alt'
                                       ELSE 'tr'
                                  END AS Type
                           FROM lyrics
                                JOIN lyrics_translations
                                ON lyrics.LyricsID = lyrics_translations.LyricsID
                                JOIN lyrics sources
                                ON lyrics_translations.TranslatedFromID = sources.LyricsID"
lyrics.translations.df = dbGetQuery(wsf.con, lyrics.translations.sql) %>%
  dplyr::select(lyrics.id = LyricsID, translated.from.id = TranslatedFromID,
                type = Type)
lyrics.artists.sql = "SELECT lyrics.LyricsID, ArtistID
                      FROM lyrics
                           LEFT JOIN lyrics_artists
                           ON lyrics.LyricsID = lyrics_artists.LyricsID"
lyrics.artists.df = dbGetQuery(wsf.con, lyrics.artists.sql) %>%
  dplyr::select(lyrics.id = LyricsID, artist.id = ArtistID) %>%
  left_join(artists.df, by = "artist.id") %>%
  group_by(lyrics.id) %>%
  arrange(last.name, first.name) %>%
  summarise(artist.string = paste(artist.name, collapse = ", ")) %>%
  ungroup() %>%
  mutate(artist.string = ifelse(artist.string == "NA", NA, artist.string)) %>%
  left_join(lyrics.translations.df, by = "lyrics.id")
while(sum(!is.na(lyrics.artists.df$translated.from.id)) > 0) {
  lyrics.artists.df = lyrics.artists.df %>%
    left_join(lyrics.artists.df %>%
                dplyr::select(lyrics.id, new.source.id = translated.from.id,
                              new.type = type,
                              source.artist.string = artist.string),
              by = c("translated.from.id" = "lyrics.id")) %>%
    mutate(artist.string = case_when(is.na(source.artist.string) ~ artist.string,
                                     is.na(artist.string) ~ source.artist.string,
                                     artist.string == source.artist.string ~ artist.string,
                                     T ~ paste(source.artist.string,
                                               ", ",
                                               type,
                                               ". ",
                                               artist.string,
                                               sep = ""))) %>%
    dplyr::select(lyrics.id, artist.string, translated.from.id = new.source.id,
                  type = new.type)
}
song.instance.info.df = song.instance.info.df %>%
  left_join(song.instances.lyrics.df %>%
              inner_join(lyrics.artists.df, by = "lyrics.id") %>%
              group_by(song.instance.id) %>%
              summarize(lyricists = paste(artist.string, collapse = "; ")),
            by = "song.instance.id")
Encoding(song.instance.info.df$lyricists) = "UTF-8"

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
              group_by(song.id) %>%
              summarise(last.lyrics.year = max(last.lyrics.year),
                        last.tune.year = max(last.tune.year)),
            by = c("song.id")) %>%
  mutate(year = ifelse(is.na(last.lyrics.year) & is.na(last.tune.year),
                       NA, pmax(last.lyrics.year, last.tune.year)),
         decade = floor(year / 10) * 10) %>%
  dplyr::select(song.id, title = song.name, topics, year, decade)
Encoding(song.info.df$title) = "UTF-8"
