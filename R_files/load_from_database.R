#### Download tables from the database ####

# Get table of songs
songs.sql = "SELECT SongID, SongName
             FROM wsf_shiny.songs"
songs.df = dbGetQuery(wsf.shiny.con, songs.sql) %>%
  dplyr::select(song.id = SongID, song.name = SongName) %>%
  mutate(song.name.sort = gsub("^['\"¡¿]", "", song.name))
Encoding(songs.df$song.name) = "UTF-8"
Encoding(songs.df$song.name.sort) = "UTF-8"

# Get table of song instances
song.instances.sql = "SELECT SongInstanceID, SongInstance, ArrangementID,
                             SongID, LastLyricsYear, LyricsCopyright,
                             LastTuneYear, TuneCopyright, ArrangementCopyright
                      FROM wsf_shiny.songinstances"
song.instances.df = dbGetQuery(wsf.shiny.con, song.instances.sql) %>%
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
               FROM wsf_shiny.artists"
artists.df = dbGetQuery(wsf.shiny.con, artists.sql) %>%
  mutate(artist.name = case_when(is.na(FirstName) ~ LastName,
                                 FirstName == "" ~ LastName,
                                 T ~ paste(FirstName, LastName,
                                           sep = " "))) %>%
  dplyr::select(artist.id = ArtistID, last.name = LastName,
                first.name = FirstName, artist.name, gender = GenderName)
Encoding(artists.df$last.name) = "UTF-8"
Encoding(artists.df$first.name) = "UTF-8"
Encoding(artists.df$artist.name) = "UTF-8"
Encoding(artists.df$gender) = "UTF-8"

# Get table that connects song instances and artists
song.instances.artists.sql = "SELECT SongInstanceID, SongID, ArtistID, Role
                              FROM wsf_shiny.songinstances_artists"
song.instances.artists.df = dbGetQuery(wsf.shiny.con,
                                       song.instances.artists.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                artist.id = ArtistID, role = Role)

# Get table of topics
topics.sql = "SELECT TopicID, TopicName
              FROM wsf_shiny.topics"
topics.df = dbGetQuery(wsf.shiny.con, topics.sql) %>%
  dplyr::select(topic.id = TopicID, topic.name = TopicName)
Encoding(topics.df$topic.name) = "UTF-8"

# Get table that connects songs and topics
songs.topics.sql = "SELECT SongID, TopicID
                    FROM wsf_shiny.songs_topics"
songs.topics.df = dbGetQuery(wsf.shiny.con, songs.topics.sql) %>%
  dplyr::select(song.id = SongID, topic.id = TopicID)

# Get table of books of the Bible
bible.books.sql = "SELECT BookID, BookName, BookAbbreviation
                   FROM wsf_shiny.bible_books"
bible.books.df = dbGetQuery(wsf.shiny.con, bible.books.sql) %>%
  dplyr::select(book.id = BookID, book.name = BookName,
                book.abbreviation = BookAbbreviation)
Encoding(bible.books.df$book.name) = "UTF-8"
Encoding(bible.books.df$book.abbreviation) = "UTF-8"

# Get table of scripture references
scripture.references.sql = "SELECT ScriptureReferenceID, BookID, BookName,
                                   BookAbbreviation, Chapter, Verse
                            FROM wsf_shiny.scripturereferences"
scripture.references.df = dbGetQuery(wsf.shiny.con,
                                     scripture.references.sql) %>%
  dplyr::select(scripture.reference.id = ScriptureReferenceID, book.id = BookID,
                book.name = BookName, book.abbreviation = BookAbbreviation,
                chapter = Chapter, verse = Verse)
Encoding(scripture.references.df$book.name) = "UTF-8"
Encoding(scripture.references.df$book.abbreviation) = "UTF-8"

# Get table that connects song instances and scripture references
song.instances.scripture.references.sql = "SELECT SongInstanceID, SongID,
                                                  ScriptureReferenceID
                                           FROM wsf_shiny.songinstances_scripturereferences"
song.instances.scripture.references.df = dbGetQuery(wsf.shiny.con,
                                                    song.instances.scripture.references.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                scripture.reference.id = ScriptureReferenceID)

# Get table of languages
languages.sql = "SELECT LanguageID, LanguageName
                 FROM wsf_shiny.languages"
languages.df = dbGetQuery(wsf.shiny.con, languages.sql) %>%
  dplyr::select(language.id = LanguageID, language.name = LanguageName)
Encoding(languages.df$language.name) = "UTF-8"

# Get table that connects song instances and languages
song.instances.languages.sql = "SELECT SongInstanceID, SongID, LanguageID
                                FROM wsf_shiny.songinstances_languages"
song.instances.languages.df = dbGetQuery(wsf.shiny.con,
                                         song.instances.languages.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                language.id = LanguageID)

# Get table of songbooks
songbooks.sql = "SELECT SongbookID, SongbookName, SongbookAbbreviation,
                        IncludeInSearch
                 FROM wsf_shiny.songbooks"
songbooks.df = dbGetQuery(wsf.shiny.con, songbooks.sql) %>%
  mutate(include.in.search = IncludeInSearch == "Y") %>%
  dplyr::select(songbook.id = SongbookID, songbook.name = SongbookName,
                songbook.abbreviation = SongbookAbbreviation, include.in.search)
Encoding(songbooks.df$songbook.name) = "UTF-8"
Encoding(songbooks.df$songbook.abbreviation) = "UTF-8"

# Get table that connects song instances and songbooks
song.instances.songbooks.sql = "SELECT SongInstanceID, SongID, SongbookID,
                                       SongbookName, SongbookAbbreviation,
                                       IncludeInSearch, SongbookVolumeID,
                                       SongbookVolume, EntryNumber
                                FROM wsf_shiny.songinstances_songbooks"
song.instances.songbooks.df = dbGetQuery(wsf.shiny.con,
                                         song.instances.songbooks.sql) %>%
  filter(SongbookID != 20 | !is.na(SongbookVolume)) %>%
  mutate(include.in.search = IncludeInSearch == "Y",
         entry.string.no.name = paste(case_when(is.na(SongbookVolume) ~ "",
                                                SongbookID %in% c(0, 6) ~ "",
                                                T ~ SongbookVolume),
                                      case_when(SongbookID == 12 ~ "",
                                                is.na(EntryNumber) ~ "",
                                                EntryNumber == "" ~ "",
                                                T ~ paste(" ", EntryNumber,
                                                          sep = "")),
                                      sep = ""),
         entry.string = paste(SongbookAbbreviation,
                              case_when(is.na(SongbookVolume) ~ "",
                                        SongbookID %in% c(0, 6) ~ "",
                                        T ~ " "),
                              entry.string.no.name,
                              sep = ""),
         entry.string.no.name = ifelse(SongbookID == 12,
                                       paste(entry.string.no.name,
                                             EntryNumber,
                                             sep = " "),
                                       entry.string.no.name)) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                songbook.id = SongbookID, songbook.name = SongbookName,
                songbook.abbreviation = SongbookAbbreviation, include.in.search,
                entry.number = EntryNumber, entry.string.no.name, entry.string)
Encoding(song.instances.songbooks.df$songbook.name) = "UTF-8"
Encoding(song.instances.songbooks.df$songbook.abbreviation) = "UTF-8"

# Get table of songbook overlap
songbook.overlap.sql = "SELECT SongbookID1, SongbookName1, IncludeInSearch1,
                               SongbookID2, SongbookName2, IncludeInSearch2,
                               SongID
                        FROM wsf_shiny.songbook_overlap"
songbook.overlap.df = dbGetQuery(wsf.shiny.con, songbook.overlap.sql) %>%
  mutate(include.in.search.1 = IncludeInSearch1 == "Y",
         include.in.search.2 = IncludeInSearch2 == "Y") %>%
  dplyr::select(songbook.id.1 = SongbookID1, songbook.name.1 = SongbookName1,
                include.in.search.1, songbook.id.2 = SongbookID2,
                songbook.name.2 = SongbookName2, include.in.search.2,
                song.id = SongID)
Encoding(songbook.overlap.df$songbook.name.1) = "UTF-8"
Encoding(songbook.overlap.df$songbook.name.2) = "UTF-8"

# Get table of arrangement types
arrangement.types.sql = "SELECT ArrangementTypeID, ArrangementType
                         FROM wsf_shiny.arrangementtypes"
arrangement.types.df = dbGetQuery(wsf.shiny.con, arrangement.types.sql) %>%
  dplyr::select(arrangement.type.id = ArrangementTypeID,
                arrangement.type = ArrangementType)
Encoding(arrangement.types.df$arrangement.type) = "UTF-8"

# Get table that connects song instances and arrangement types
song.instances.arrangement.types.sql = "SELECT SongInstanceID, SongID,
                                               ArrangementTypeID
                                        FROM wsf_shiny.songinstances_arrangementtypes"
song.instances.arrangement.types.df = dbGetQuery(wsf.shiny.con,
                                                 song.instances.arrangement.types.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                arrangement.type.id = ArrangementTypeID)

# Get table of key signatures
key.signatures.sql = "SELECT KeySignatureID, PitchName, AccidentalID,
                             AccidentalSymbol, ModeID, ModeName
                      FROM wsf_shiny.keysignatures"
key.signatures.df = dbGetQuery(wsf.shiny.con, key.signatures.sql) %>%
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
Encoding(key.signatures.df$pitch.name) = "UTF-8"
Encoding(key.signatures.df$accidental.symbol) = "UTF-8"
Encoding(key.signatures.df$mode.name) = "UTF-8"
Encoding(key.signatures.df$key.signature.string) = "UTF-8"

# Get table that connects song instances and key signatures
song.instances.key.signatures.sql = "SELECT SongInstanceID, SongID,
                                            KeySignatureID
                                     FROM wsf_shiny.songinstances_keysignatures"
song.instances.key.signatures.df = dbGetQuery(wsf.shiny.con,
                                              song.instances.key.signatures.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                key.signature.id = KeySignatureID)

# Get table of time signatures
time.signatures.sql = "SELECT TimeSignatureID, TimeSignatureBeat,
                              TimeSignatureMeasure
                       FROM wsf_shiny.timesignatures"
time.signatures.df = dbGetQuery(wsf.shiny.con, time.signatures.sql) %>%
  mutate(time.signature.string = paste(TimeSignatureBeat, TimeSignatureMeasure,
                                       sep = "/")) %>%
  dplyr::select(time.signature.id = TimeSignatureID,
                time.signature.beat = TimeSignatureBeat,
                time.signature.measure = TimeSignatureMeasure,
                time.signature.string)

# Get table that connects song instances and time signatures
song.instances.time.signatures.sql = "SELECT SongInstanceID, SongID,
                                             TimeSignatureID
                                      FROM wsf_shiny.songinstances_timesignatures"
song.instances.time.signatures.df = dbGetQuery(wsf.shiny.con,
                                               song.instances.time.signatures.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                time.signature.id = TimeSignatureID)

# Get table of meters
meters.sql = "SELECT MeterID, Meter, Multiplier
              FROM wsf_shiny.meters"
meters.df = dbGetQuery(wsf.shiny.con, meters.sql) %>%
  mutate(meter.string = paste(Meter, Multiplier)) %>%
  dplyr::select(meter.id = MeterID, meter = Meter, multiplier = Multiplier,
                meter.string)
Encoding(meters.df$meter.string) = "UTF-8"

# Get table that connects song instances and meters
song.instances.meters.sql = "SELECT SongInstanceID, SongID, MeterID
                             FROM wsf_shiny.songinstances_meters"
song.instances.meters.df = dbGetQuery(wsf.shiny.con,
                                      song.instances.meters.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                meter.id = MeterID)

# Get lyrics first lines for all song instances
lyrics.first.lines.sql = "SELECT SongInstanceID, FirstLine, RefrainFirstLine
                          FROM wsf_shiny.lyrics_first_lines"
lyrics.first.lines.df = dbGetQuery(wsf.shiny.con, lyrics.first.lines.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, first.line = FirstLine,
                refrain.first.line = RefrainFirstLine) %>%
  pivot_longer(cols = c(first.line, refrain.first.line),
               names_to = "source",
               values_to = "lyrics.line") %>%
  filter(!is.na(lyrics.line),
         lyrics.line != "")
Encoding(lyrics.first.lines.df$source) = "UTF-8"
Encoding(lyrics.first.lines.df$lyrics.line) = "UTF-8"

# Get table of worship slots
if(version == "ctcc") {
  worship.slots.sql = "SELECT WorshipSlotID, WorshipSlot, WorshipSlotOrder
                       FROM wsf_shiny.worshipslots"
  worship.slots.df = dbGetQuery(wsf.shiny.con, worship.slots.sql) %>%
    dplyr::select(worship.slot.id = WorshipSlotID, worship.slot = WorshipSlot,
                  worship.slot.order = WorshipSlotOrder)
  Encoding(worship.slots.df$worship.slot) = "UTF-8"
}

# Get table of worship history
if(version == "ctcc") {
  worship.history.sql = "SELECT WorshipHistoryID, SongInstanceID,
                                WorshipHistoryDate, WorshipSlotID
                         FROM wsf_shiny.worshiphistory"
  worship.history.df = dbGetQuery(wsf.shiny.con, worship.history.sql) %>%
    dplyr::select(worship.history.id = WorshipHistoryID,
                  song.instance.id = SongInstanceID,
                  worship.history.date = WorshipHistoryDate,
                  worship.slot.id = WorshipSlotID)
  worship.history.df$worship.history.date = as.Date(worship.history.df$worship.history.date)
  worship.history.df = filter(worship.history.df,
                              format(worship.history.df$worship.history.date, "%Y") >= 2017)
}

# Get table of psalm songs
psalm.songs.sql = "SELECT PsalmSongID, PsalmNumber, SongID, PsalmSongTypeID,
                          PsalmSongType, PsalmSongTitle, PrettyScriptureList
                   FROM wsf_shiny.psalmsongs"
psalm.songs.df = dbGetQuery(wsf.shiny.con, psalm.songs.sql) %>%
  mutate(pretty.scripture.list = gsub("^Ps [0-9]+:", "", PrettyScriptureList)) %>%
  dplyr::select(psalm.song.id = PsalmSongID, psalm.number = PsalmNumber,
                song.id = SongID, psalm.song.type.id = PsalmSongTypeID,
                psalm.song.type = PsalmSongType,
                psalm.song.title = PsalmSongTitle, pretty.scripture.list)
Encoding(psalm.songs.df$psalm.song.type) = "UTF-8"
Encoding(psalm.songs.df$psalm.song.title) = "UTF-8"

# Get table of full lyrics
full.lyrics.sql = "SELECT LyricsID, FullLyrics
                   FROM wsf.all_lyrics"
full.lyrics.df = dbGetQuery(wsf.shiny.con, full.lyrics.sql) %>%
  dplyr::select(lyrics.id = LyricsID, full.lyrics = FullLyrics)
Encoding(full.lyrics.df$full.lyrics) = "UTF-8"

# Get table that connecs psalm songs and lyrics
psalm.songs.lyrics.sql = "SELECT PsalmSongID, LyricsID, FirstLine, LanguageID,
                                 PublicDomain, LyricsOrder
                          FROM wsf_shiny.psalmsongs_lyrics"
psalm.songs.lyrics.df = dbGetQuery(wsf.shiny.con, psalm.songs.lyrics.sql) %>%
  dplyr::select(psalm.song.id = PsalmSongID, lyrics.id = LyricsID,
                first.line = FirstLine, language.id = LanguageID,
                public.domain = PublicDomain, lyrics.order = LyricsOrder)
Encoding(psalm.songs.lyrics.df$first.line) = "UTF-8"

# Get table of psalm song types
psalm.song.types.sql = "SELECT PsalmSongTypeID, PsalmSongType
                        FROM wsf_shiny.psalmsongtypes"
psalm.song.types.df = dbGetQuery(wsf.shiny.con, psalm.song.types.sql) %>%
  dplyr::select(psalm.song.type.id = PsalmSongTypeID,
                psalm.song.type = PsalmSongType)
Encoding(psalm.song.types.df$psalm.song.type) = "UTF-8"

# Get table of alternative tunes
alternative.tunes.sql = "SELECT PsalmSongID, TuneID, TuneDisplayName, Notes
                         FROM wsf_shiny.psalmsongs_alternativetunes"
alternative.tunes.df = dbGetQuery(wsf.shiny.con, alternative.tunes.sql) %>%
  dplyr::select(psalm.song.id = PsalmSongID, tune.id = TuneID,
                tune.display.name = TuneDisplayName, notes = Notes)
Encoding(alternative.tunes.df$tune.display.name) = "UTF-8"
Encoding(alternative.tunes.df$notes) = "UTF-8"

# Get table that connects tunes and canonical songs
tunes.canonical.songs.sql = "SELECT TuneID, SongID
                             FROM wsf_shiny.tunes_canonicalsongs"
tunes.canonical.songs.df = dbGetQuery(wsf.shiny.con,
                                      tunes.canonical.songs.sql) %>%
  dplyr::select(tune.id = TuneID, song.id = SongID)

#### Remove songbooks we don't want to show ####

if(version == "general") {
  # Songbooks
  songbooks.df = songbooks.df %>%
    filter(include.in.search)
  # Songbook entries
  song.instances.songbooks.df = song.instances.songbooks.df %>%
    filter(include.in.search)
  # Songbook overlap
  songbook.overlap.df = songbook.overlap.df %>%
    filter(include.in.search.1, include.in.search.2)
  # Song instances
  song.instances.df = song.instances.df %>%
    semi_join(song.instances.songbooks.df %>%
                dplyr::select(song.instance.id) %>%
                distinct(),
              by = "song.instance.id")
  # Songs
  songs.df = songs.df %>%
    semi_join(song.instances.df %>%
                dplyr::select(song.id) %>%
                distinct(),
              by = "song.id")
  # Artists
  artists.df = artists.df %>%
    semi_join(song.instances.artists.df %>%
                semi_join(song.instances.df, by = "song.instance.id"),
              by = "artist.id")
  # Topics
  topics.df = topics.df %>%
    semi_join(songs.topics.df %>%
                semi_join(songs.df, by = "song.id"),
              by = "topic.id")
  # Scripture references
  scripture.references.df = scripture.references.df %>%
    semi_join(song.instances.scripture.references.df %>%
                semi_join(song.instances.df, by = "song.instance.id"),
              by = "scripture.reference.id")
  # Languages
  languages.df = languages.df %>%
    semi_join(song.instances.languages.df %>%
                semi_join(song.instances.df, by = "song.instance.id"),
              by = "language.id")
  # Arrangement types
  arrangement.types.df = arrangement.types.df %>%
    semi_join(song.instances.arrangement.types.df %>%
                semi_join(song.instances.df, by = "song.instance.id"),
              by = "arrangement.type.id")
  # Key signatures
  key.signatures.df = key.signatures.df %>%
    semi_join(song.instances.key.signatures.df %>%
                semi_join(song.instances.df, by = "song.instance.id"),
              by = "key.signature.id")
  # Time signatures
  time.signatures.df = time.signatures.df %>%
    semi_join(song.instances.time.signatures.df %>%
                semi_join(song.instances.df, by = "song.instance.id"),
              by = "time.signature.id")
  # Meters
  meters.df = meters.df %>%
    semi_join(song.instances.meters.df %>%
                semi_join(song.instances.df, by = "song.instance.id"),
              by = "meter.id")
  # Psalm songs
  psalm.songs.df = psalm.songs.df %>%
    filter(song.id %in% song.instances.df$song.id ||
             grepl("^MP", psalm.song.id))
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
  arrange(desc(num.entries)) %>%
  dplyr::select(song.id, song.instance.id, title = song.instance, year, decade,
                songbook.entries, arrangement.types, key.signatures,
                time.signatures, scripture.references, composers, arrangers,
                lyrics.copyright, tune.copyright, arrangement.copyright,
                num.entries)

# Add translators and alterers to lyrics artists
song.instances.lyrics.sql = "SELECT SongInstanceID, LyricsID
                             FROM songinstances_lyrics"
song.instances.lyrics.df = dbGetQuery(wsf.shiny.con,
                                      song.instances.lyrics.sql) %>%
  dplyr::select(song.instance.id = SongInstanceID, lyrics.id = LyricsID)
lyrics.translations.sql = "SELECT LyricsID, TranslatedFromID, Type
                           FROM lyrics_translations"
lyrics.translations.df = dbGetQuery(wsf.shiny.con, lyrics.translations.sql) %>%
  dplyr::select(lyrics.id = LyricsID, translated.from.id = TranslatedFromID,
                type = Type)
lyrics.artists.sql = "SELECT LyricsID, ArtistID
                      FROM lyrics_artists"
lyrics.artists.df = dbGetQuery(wsf.shiny.con, lyrics.artists.sql) %>%
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
  left_join(song.instances.songbooks.df %>%
              group_by(song.id) %>%
              arrange(songbook.abbreviation, entry.number) %>%
              summarise(songbook.entries = paste(entry.string,
                                                 collapse = ", ")) %>%
              ungroup(),
            by = c("song.id")) %>%
  mutate(year = ifelse(is.na(last.lyrics.year) & is.na(last.tune.year),
                       NA, pmax(last.lyrics.year, last.tune.year)),
         decade = floor(year / 10) * 10) %>%
  dplyr::select(song.id, title = song.name, topics, year, decade,
                songbook.entries)

# Create nodes and edges for songbook overlap graph
songbook.group.colors = brewer.pal(5, "Set3")[c(5, 3, 1, 2, 4)]
names(songbook.group.colors) = c("Newer songbooks", "Older songbooks",
                                 "Series", "Specialized collections",
                                 "Uncategorized")
songbook.overlap.nodes.df = data.frame(songbook.id = unique(c(songbook.overlap.df$songbook.id.1,
                                                              songbook.overlap.df$songbook.id.2))) %>%
  inner_join(songbooks.df, by = "songbook.id") %>%
  mutate(label = str_wrap(songbook.name, 15),
         group = case_when(songbook.id %in% c(1, 6, 8, 11, 17) ~ 1,
                           songbook.id %in% c(2, 3, 4, 5, 9) ~ 2,
                           songbook.id %in% c(10, 14, 15, 20) ~ 3,
                           songbook.id %in% c(13, 18, 19) ~ 4,
                           T ~ 5),
         rgb = case_when(songbook.id == 12 ~ "#888888",
                         T ~ songbook.group.colors[group]),
         rgb = apply(col2rgb(rgb), 2, paste, collapse = ", "),
         color = paste("rgba(", rgb, ", 0.6)", sep = "")) %>%
  dplyr::select(id = songbook.id, label, group, rgb, color)
songbook.overlap.edges.df = songbook.overlap.df %>%
  mutate(id = paste(songbook.id.1, songbook.id.2, sep = "-"),
         overlap.weight = 1) %>%
  dplyr::select(id, from = songbook.id.1, to = songbook.id.2, songbook.name.1,
                songbook.name.2, song.id, overlap.weight) %>%
  group_by(id, from, to, songbook.name.1, songbook.name.2) %>%
  summarize(n = n(),
            weight = sum(overlap.weight)) %>%
  ungroup() %>%
  mutate(title = paste(songbook.name.1, "<br/>", songbook.name.2, "<br/><i>",
                       n, " song", ifelse(n == 1, "", "s"), " in common</i>",
                       sep = ""),
         value = weight)

# Add songbook entries to alternative tunes
alternative.tunes.df = alternative.tunes.df %>%
  left_join(tunes.canonical.songs.df, by = "tune.id") %>%
  left_join(song.instances.songbooks.df, by = "song.id") %>%
  group_by(psalm.song.id, tune.id, tune.display.name, notes, song.id) %>%
  arrange(songbook.abbreviation, entry.number) %>%
  summarise(entry.string = paste(entry.string, collapse = ", ")) %>%
  ungroup()
