#### General filter ####

# Filter out material from song(book)s we don't want to show
if(version == "general") {
  filter.sql = "WHERE IncludeInSearch = 'Y'"
} else {
  filter.sql = ""
}

#### Song instance info ####

# Get table of song instances
song.instances.sql = "SELECT SongInstanceID, SongInstance, SongInstanceLower,
                             ArrangementID, SongID, LastLyricsYear,
                             LyricsCopyright, LastTuneYear, TuneCopyright,
                             ArrangementCopyright, ArrangementTypes, Lyricists,
                             Composers, Arrangers, SongbookEntries, NumEntries,
                             KeySignatures, TimeSignatures, ScriptureReferences,
                             HTML
                      FROM wsf_shiny.songinstances"
song.instances.df = dbGetQuery(wsf.shiny.con,
                               paste(song.instances.sql,
                                     filter.sql)) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.instance = SongInstance,
                song.instance.lower = SongInstanceLower,
                arrangement.id = ArrangementID, song.id = SongID,
                last.lyrics.year = LastLyricsYear,
                lyrics.copyright = LyricsCopyright,
                last.tune.year = LastTuneYear, tune.copyright = TuneCopyright,
                arrangement.copyright = ArrangementCopyright,
                arrangement.types = ArrangementTypes, lyricists = Lyricists,
                composers = Composers, arrangers = Arrangers,
                songbook.entries = SongbookEntries, num.entries = NumEntries,
                key.signatures = KeySignatures,
                time.signatures = TimeSignatures,
                scripture.references = ScriptureReferences, html = HTML)
Encoding(song.instances.df$song.instance) = "UTF-8"
Encoding(song.instances.df$song.instance.lower) = "UTF-8"
Encoding(song.instances.df$lyrics.copyright) = "UTF-8"
Encoding(song.instances.df$tune.copyright) = "UTF-8"
Encoding(song.instances.df$arrangement.copyright) = "UTF-8"
Encoding(song.instances.df$arrangement.types) = "UTF-8"
Encoding(song.instances.df$lyricists) = "UTF-8"
Encoding(song.instances.df$composers) = "UTF-8"
Encoding(song.instances.df$arrangers) = "UTF-8"
Encoding(song.instances.df$songbook.entries) = "UTF-8"
Encoding(song.instances.df$key.signatures) = "UTF-8"
Encoding(song.instances.df$time.signatures) = "UTF-8"
Encoding(song.instances.df$scripture.references) = "UTF-8"
Encoding(song.instances.df$html) = "UTF-8"

# Get table of artists
artists.sql = "SELECT ArtistID, LastName, FirstName, ArtistName, GenderName
               FROM wsf_shiny.artists"
artists.df = dbGetQuery(wsf.shiny.con, paste(artists.sql, filter.sql)) %>%
  dplyr::select(artist.id = ArtistID, last.name = LastName,
                first.name = FirstName, artist.name = ArtistName,
                gender = GenderName)
Encoding(artists.df$last.name) = "UTF-8"
Encoding(artists.df$first.name) = "UTF-8"
Encoding(artists.df$artist.name) = "UTF-8"
Encoding(artists.df$gender) = "UTF-8"

# Get table that connects song instances and artists
song.instances.artists.sql = "SELECT SongInstanceID, SongID, ArtistID, Role
                              FROM wsf_shiny.songinstances_artists"
song.instances.artists.df = dbGetQuery(wsf.shiny.con,
                                       paste(song.instances.artists.sql,
                                             filter.sql)) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                artist.id = ArtistID, role = Role)

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
                                     paste(scripture.references.sql,
                                           filter.sql)) %>%
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
                                                    paste(song.instances.scripture.references.sql,
                                                          filter.sql)) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                scripture.reference.id = ScriptureReferenceID)

# Get table of languages
languages.sql = "SELECT LanguageID, LanguageName
                 FROM wsf_shiny.languages"
languages.df = dbGetQuery(wsf.shiny.con, paste(languages.sql, filter.sql)) %>%
  dplyr::select(language.id = LanguageID, language.name = LanguageName)
Encoding(languages.df$language.name) = "UTF-8"

# Get table that connects song instances and languages
song.instances.languages.sql = "SELECT SongInstanceID, SongID, LanguageID
                                FROM wsf_shiny.songinstances_languages"
song.instances.languages.df = dbGetQuery(wsf.shiny.con,
                                         paste(song.instances.languages.sql,
                                               filter.sql)) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                language.id = LanguageID)

# Get table of songbooks
songbooks.sql = "SELECT SongbookID, SongbookName, SongbookAbbreviation
                 FROM wsf_shiny.songbooks"
songbooks.df = dbGetQuery(wsf.shiny.con,
                          paste(songbooks.sql, filter.sql)) %>%
  dplyr::select(songbook.id = SongbookID, songbook.name = SongbookName,
                songbook.abbreviation = SongbookAbbreviation)
Encoding(songbooks.df$songbook.name) = "UTF-8"
Encoding(songbooks.df$songbook.abbreviation) = "UTF-8"

# Get table that connects song instances and songbooks
song.instances.songbooks.sql = "SELECT SongInstanceID, SongID, SongbookID,
                                       SongbookName, SongbookAbbreviation,
                                       IncludeInSearch, SongbookVolumeID,
                                       SongbookVolume, EntryNumber,
                                       EntryStringNoName, EntryString
                                FROM wsf_shiny.songinstances_songbooks"
song.instances.songbooks.df = dbGetQuery(wsf.shiny.con,
                                         paste(song.instances.songbooks.sql,
                                               ifelse(filter.sql == "",
                                                      "WHERE 1 = 1 ",
                                                      filter.sql),
                                               " AND (SongbookID <> 20
                                                      OR SongbookVolume IS NOT NULL)")) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                songbook.id = SongbookID, songbook.name = SongbookName,
                songbook.abbreviation = SongbookAbbreviation,
                entry.number = EntryNumber,
                entry.string.no.name = EntryStringNoName,
                entry.string = EntryString)
Encoding(song.instances.songbooks.df$songbook.name) = "UTF-8"
Encoding(song.instances.songbooks.df$songbook.abbreviation) = "UTF-8"
Encoding(song.instances.songbooks.df$entry.string.no.name) = "UTF-8"
Encoding(song.instances.songbooks.df$entry.string) = "UTF-8"

# Get table of arrangement types
arrangement.types.sql = "SELECT ArrangementTypeID, ArrangementType
                         FROM wsf_shiny.arrangementtypes"
arrangement.types.df = dbGetQuery(wsf.shiny.con,
                                  paste(arrangement.types.sql, filter.sql)) %>%
  dplyr::select(arrangement.type.id = ArrangementTypeID,
                arrangement.type = ArrangementType)
Encoding(arrangement.types.df$arrangement.type) = "UTF-8"

# Get table that connects song instances and arrangement types
song.instances.arrangement.types.sql = "SELECT SongInstanceID, SongID,
                                               ArrangementTypeID
                                        FROM wsf_shiny.songinstances_arrangementtypes"
song.instances.arrangement.types.df = dbGetQuery(wsf.shiny.con,
                                                 paste(song.instances.arrangement.types.sql,
                                                       filter.sql)) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                arrangement.type.id = ArrangementTypeID)

# Get table of key signatures
key.signatures.sql = "SELECT KeySignatureID, PitchName, AccidentalID,
AccidentalSymbol, ModeID, ModeName,
KeySignatureString
FROM wsf_shiny.keysignatures"
key.signatures.df = dbGetQuery(wsf.shiny.con,
                               paste(key.signatures.sql, filter.sql)) %>%
  dplyr::select(key.signature.id = KeySignatureID, pitch.name = PitchName,
                accidental.id = AccidentalID,
                accidental.symbol = AccidentalSymbol, mode.id = ModeID,
                mode.name = ModeName, key.signature.string = KeySignatureString)
Encoding(key.signatures.df$pitch.name) = "UTF-8"
Encoding(key.signatures.df$accidental.symbol) = "UTF-8"
Encoding(key.signatures.df$mode.name) = "UTF-8"
Encoding(key.signatures.df$key.signature.string) = "UTF-8"

# Get table that connects song instances and key signatures
song.instances.key.signatures.sql = "SELECT SongInstanceID, SongID,
KeySignatureID
FROM wsf_shiny.songinstances_keysignatures"
song.instances.key.signatures.df = dbGetQuery(wsf.shiny.con,
                                              paste(song.instances.key.signatures.sql,
                                                    filter.sql)) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                key.signature.id = KeySignatureID)

# Get table of time signatures
time.signatures.sql = "SELECT TimeSignatureID, TimeSignatureBeat,
                              TimeSignatureMeasure, TimeSignatureString
                       FROM wsf_shiny.timesignatures"
time.signatures.df = dbGetQuery(wsf.shiny.con,
                                paste(time.signatures.sql,
                                      filter.sql)) %>%
  dplyr::select(time.signature.id = TimeSignatureID,
                time.signature.beat = TimeSignatureBeat,
                time.signature.measure = TimeSignatureMeasure,
                time.signature.string = TimeSignatureString)
Encoding(time.signatures.df$time.signature.string) = "UTF-8"

# Get table that connects song instances and time signatures
song.instances.time.signatures.sql = "SELECT SongInstanceID, SongID,
                                             TimeSignatureID
                                      FROM wsf_shiny.songinstances_timesignatures"
song.instances.time.signatures.df = dbGetQuery(wsf.shiny.con,
                                               paste(song.instances.time.signatures.sql,
                                                     filter.sql)) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                time.signature.id = TimeSignatureID)

# Get table of meters
meters.sql = "SELECT MeterID, Meter, Multiplier, MeterString
              FROM wsf_shiny.meters"
meters.df = dbGetQuery(wsf.shiny.con, paste(meters.sql, filter.sql)) %>%
  dplyr::select(meter.id = MeterID, meter = Meter, multiplier = Multiplier,
                meter.string = MeterString)
Encoding(meters.df$meter.string) = "UTF-8"

# Get table that connects song instances and meters
song.instances.meters.sql = "SELECT SongInstanceID, SongID, MeterID
                             FROM wsf_shiny.songinstances_meters"
song.instances.meters.df = dbGetQuery(wsf.shiny.con,
                                      paste(song.instances.meters.sql,
                                            filter.sql)) %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID,
                meter.id = MeterID)

# Get table of full lyrics
full.lyrics.sql = "SELECT LyricsID, FullLyrics
FROM wsf_shiny.full_lyrics"
full.lyrics.df = dbGetQuery(wsf.shiny.con, full.lyrics.sql) %>%
  dplyr::select(lyrics.id = LyricsID, full.lyrics = FullLyrics)
Encoding(full.lyrics.df$full.lyrics) = "UTF-8"

#### Song info ####

# Get table of songs
songs.sql = "SELECT SongID, SongName, SongNameLower, SongNameSort, Topics
FROM wsf_shiny.songs"
songs.df = dbGetQuery(wsf.shiny.con,
                      paste(songs.sql, filter.sql)) %>%
  dplyr::select(song.id = SongID, song.name = SongName,
                song.name.lower = SongNameLower,
                song.name.sort = SongNameSort, topics = Topics)
Encoding(songs.df$song.name) = "UTF-8"
Encoding(songs.df$song.name.lower) = "UTF-8"
Encoding(songs.df$song.name.sort) = "UTF-8"
Encoding(songs.df$topics) = "UTF-8"

# Get table of topics
topics.sql = "SELECT TopicID, TopicName
FROM wsf_shiny.topics"
topics.df = dbGetQuery(wsf.shiny.con, paste(topics.sql, filter.sql)) %>%
  dplyr::select(topic.id = TopicID, topic.name = TopicName)
Encoding(topics.df$topic.name) = "UTF-8"

# Get table that connects songs and topics
songs.topics.sql = "SELECT SongID, TopicID
FROM wsf_shiny.songs_topics"
songs.topics.df = dbGetQuery(wsf.shiny.con,
                             paste(songs.topics.sql, filter.sql)) %>%
  dplyr::select(song.id = SongID, topic.id = TopicID)

#### Songbook overlap info ####

# Get table of songbook overlap
songbook.overlap.sql = "SELECT SongbookID1, SongbookName1, SongbookID2,
                               SongbookName2, SongID
                        FROM wsf_shiny.songbook_overlap"
songbook.overlap.df = dbGetQuery(wsf.shiny.con,
                                 paste(songbook.overlap.sql,
                                       filter.sql)) %>%
  dplyr::select(songbook.id.1 = SongbookID1, songbook.name.1 = SongbookName1,
                songbook.id.2 = SongbookID2, songbook.name.2 = SongbookName2,
                song.id = SongID)
Encoding(songbook.overlap.df$songbook.name.1) = "UTF-8"
Encoding(songbook.overlap.df$songbook.name.2) = "UTF-8"

#### Worship history info ####

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
  worship.history.df$worship.history.date = as.Date(worship.history.df$worship.history.date,
                                                    format = "%m/%d/%Y")
  worship.history.df = filter(worship.history.df,
                              format(worship.history.df$worship.history.date, "%Y") >= 2017)
}

#### Psalm song info ####

# Get table of psalm songs
psalm.songs.sql = "SELECT PsalmSongID, PsalmNumber, SongID, PsalmSongTypeID,
                          PsalmSongType, PsalmSongTitle,
                          PrettyScriptureList, Artists
                   FROM wsf_shiny.psalmsongs"
psalm.songs.df = dbGetQuery(wsf.shiny.con,
                            paste(psalm.songs.sql, filter.sql)) %>%
  dplyr::select(psalm.song.id = PsalmSongID, psalm.number = PsalmNumber,
                song.id = SongID, psalm.song.type.id = PsalmSongTypeID,
                psalm.song.type = PsalmSongType,
                psalm.song.title = PsalmSongTitle,
                pretty.scripture.list = PrettyScriptureList, artists = Artists)
Encoding(psalm.songs.df$psalm.song.type) = "UTF-8"
Encoding(psalm.songs.df$psalm.song.title) = "UTF-8"
Encoding(psalm.songs.df$pretty.scripture.list) = "UTF-8"
Encoding(psalm.songs.df$artists) = "UTF-8"

# Get table that connecs psalm songs and lyrics
psalm.songs.lyrics.sql = "SELECT PsalmSongID, LyricsID, FirstLine, LanguageID,
                                 PublicDomain, LyricsOrder
                          FROM wsf_shiny.psalmsongs_lyrics"
psalm.songs.lyrics.df = dbGetQuery(wsf.shiny.con,
                                   paste(psalm.songs.lyrics.sql,
                                         filter.sql)) %>%
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
alternative.tunes.df = dbGetQuery(wsf.shiny.con,
                                  paste(alternative.tunes.sql,
                                        filter.sql)) %>%
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

#### Collect song info into pretty formats ####

# Get info for all song instances
song.instance.info.df = song.instances.df %>%
  mutate(year = ifelse(is.na(last.lyrics.year) & is.na(last.tune.year),
                       NA, pmax(last.lyrics.year, last.tune.year)),
         decade = floor(year / 10) * 10) %>%
  arrange(desc(num.entries)) %>%
  dplyr::select(song.id, song.instance.id, title = song.instance,
                title.lower = song.instance.lower, year, decade,
                songbook.entries, arrangement.types, key.signatures,
                time.signatures, scripture.references, lyricists, composers,
                arrangers, lyrics.copyright, tune.copyright,
                arrangement.copyright, num.entries)

# Get info for all songs
song.info.df = songs.df %>%
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
              ungroup() %>%
              mutate(songbook.entries = na_if(songbook.entries, "NA")),
            by = c("song.id")) %>%
  mutate(year = ifelse(is.na(last.lyrics.year) & is.na(last.tune.year),
                       NA, pmax(last.lyrics.year, last.tune.year)),
         decade = floor(year / 10) * 10) %>%
  dplyr::select(song.id, title = song.name, title.lower = song.name.lower,
                topics, year, decade, songbook.entries)

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
  ungroup() %>%
  mutate(entry.string = na_if(entry.string,"NA"))
