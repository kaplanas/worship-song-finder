# Filter by song title
if(input$songTitle != "") {
  if(input$songTitleOptions == "String") {
    parts = c(input$songTitle)
  }
  else {
    parts = unlist(strsplit(input$songTitle, " "))
    if(input$songTitleOptions == "Whole words") {
      parts = paste("\\b", parts, "\\b", sep = "")
    }
  }
  for(part in parts) {
    results.df = inner_join(results.df, song.instances.df,
                            by = "song.id") %>%
      filter(grepl(part, song.instance, ignore.case = T)) %>%
      select(song.id, song.name) %>%
      distinct
  }
}

# Filter by artist
if(input$artistName != "") {
  results.lyrics.df = data.frame(song.id = integer(), song.name = character(),
                                 artist.id = integer())
  if(is.element(input$artistNameOptions, c("Lyrics", "Any"))) {
    results.lyrics.df = inner_join(results.df, song.instances.df,
                                   by = "song.id") %>%
      inner_join(song.instances.lyrics.df, by = "song.instance.id") %>%
      inner_join(lyrics.artists.df, by = "lyrics.id") %>%
      select(song.id, song.name, artist.id) %>%
      distinct
  }
  results.tune.df = data.frame(song.id = integer(), song.name = character(),
                               artist.id = integer())
  if(is.element(input$artistNameOptions, c("Music", "Any"))) {
    results.tune.df = inner_join(results.df, song.instances.df,
                                 by = "song.id") %>%
      inner_join(song.instances.tunes.df, by = "song.instance.id") %>%
      inner_join(tunes.artists.df, by = "tune.id") %>%
      select(song.id, song.name, artist.id) %>%
      distinct
  }
  results.arrangement.df = data.frame(song.id = integer(),
                                      song.name = character(),
                                      artist.id = integer())
  if(is.element(input$artistNameOptions, c("Arrangement", "Any"))) {
    results.arrangement.df = inner_join(results.df, song.instances.df,
                                        by = "song.id") %>%
      inner_join(arrangements.artists.df, by = "arrangement.id") %>%
      select(song.id, song.name, artist.id) %>%
      distinct
  }
  parts = unlist(strsplit(input$artistName, " "))
  parts = parts[nchar(parts) > 0]
  parts = paste(parts, collapse = "|")
  results.df = union(results.lyrics.df, results.tune.df) %>%
    union(results.arrangement.df) %>%
    inner_join(artists.df, by = "artist.id") %>%
    select(song.id, song.name, first.name, last.name) %>%
    filter(grepl(parts, first.name, ignore.case = T) |
             grepl(parts, last.name, ignore.case = T)) %>%
    select(song.id, song.name) %>%
    distinct
}

# Filter by topic
if(length(input$topicChoices) >= 1) {
  if(input$topicOptions == "Match all") {
    for(topicChoiceID in input$topicChoices) {
      results.df = inner_join(results.df, songs.topics.df, by = "song.id") %>%
        filter(topic.id == topicChoiceID) %>%
        select(song.id, song.name) %>%
        distinct
    }
  }
  else {
    results.df = inner_join(results.df, songs.topics.df, by = "song.id") %>%
      filter(is.element(topic.id, input$topicChoices)) %>%
      select(song.id, song.name) %>%
      distinct
  }
}

# Filter by scripture reference
if(input$scriptureBook != 0) {
  results.df = inner_join(results.df, song.instances.df, by = "song.id") %>%
    inner_join(song.instances.lyrics.df) %>%
    inner_join(lyrics.scripture.references.df, by = "lyrics.id") %>%
    inner_join(scripture.references.df, by = "scripture.reference.id")
  if(input$scriptureOptions == "Single verse") {
    results.df = filter(results.df,
                        book.id == input$scriptureBook &
                          chapter == input$scriptureChapterStart &
                          verse == input$scriptureVerseStart) %>%
      select(song.id, song.name) %>%
      distinct
  }
  else {
    results.df = filter(results.df,
                        book.id == input$scriptureBook &
                          (chapter > input$scriptureChapterStart |
                             (chapter == input$scriptureChapterStart &
                                verse >= input$scriptureVerseStart)) &
                          (chapter < input$scriptureChapterEnd |
                             (chapter == input$scriptureChapterEnd &
                                verse <= input$scriptureVerseEnd))) %>%
      select(song.id, song.name) %>%
      distinct
  }
}

# Filter by language
if(length(input$languageChoices) >= 1) {
  if(input$languageOptions == "Match all") {
    for(languageChoiceID in input$languageChoices) {
      results.df = inner_join(results.df, song.instances.df, by = "song.id") %>%
        inner_join(song.instances.lyrics.df, by = "song.instance.id") %>%
        inner_join(lyrics.df, by = "lyrics.id") %>%
        filter(language.id == languageChoiceID) %>%
        select(song.id, song.name) %>%
        distinct
    }
  }
  else {
    results.df = inner_join(results.df, song.instances.df, by = "song.id") %>%
      inner_join(song.instances.lyrics.df, by = "song.instance.id") %>%
      inner_join(lyrics.df, by = "lyrics.id") %>%
      filter(is.element(language.id, input$languageChoices)) %>%
      select(song.id, song.name) %>%
      distinct
  }
}

# Filter by songbook
if(length(input$songbookChoices) >= 1) {
  if(input$songbookOptions == "Match all") {
    for(songbookChoiceID in input$songbookChoices) {
      results.df = inner_join(results.df, song.instances.df, by = "song.id") %>%
        inner_join(songbook.entries.df, by = "song.instance.id") %>%
        filter(songbook.id == songbookChoiceID) %>%
        select(song.id, song.name) %>%
        distinct
    }
  }
  else {
    results.df = inner_join(results.df, song.instances.df, by = "song.id") %>%
      inner_join(songbook.entries.df, by = "song.instance.id") %>%
      filter(is.element(songbook.id, input$songbookChoices)) %>%
      select(song.id, song.name) %>%
      distinct
  }
}

# Filter by arrangement type
if(length(input$arrangementChoices) >= 1) {
  if(input$arrangementOptions == "Include") {
    results.df = inner_join(results.df, song.instances.df, by = "song.id") %>%
      inner_join(arrangements.arrangement.types.df, by = "arrangement.id") %>%
      filter(is.element(arrangement.type.id, input$arrangementChoices)) %>%
      select(song.id, song.name) %>%
      distinct
  }
  else {
    song.instances.exclude.df = inner_join(song.instances.df,
                                           arrangements.arrangement.types.df,
                                           by = "arrangement.id") %>%
      filter(is.element(arrangement.type.id, input$arrangementChoices)) %>%
      select(song.instance.id, song.id) %>%
      distinct
    song.instances.include.df = select(song.instances.df, song.instance.id,
                                       song.id) %>%
      setdiff(song.instances.exclude.df)
    results.df = inner_join(results.df, song.instances.include.df,
                            by = "song.id") %>%
      select(song.id, song.name) %>%
      distinct
  }
}

# Filter by key signature
if(length(input$keyChoices) >= 1) {
  if(input$keyOptions == "Match all") {
    song.instances.include.df = select(song.instances.df, song.instance.id)
    for(keyChoiceID in input$keyChoices) {
      song.instances.include.df = inner_join(song.instances.include.df,
                                             song.instances.key.signatures.df,
                                             by = "song.instance.id") %>%
        filter(key.signature.id == keyChoiceID) %>%
        select(song.instance.id) %>%
        distinct
    }
    song.instances.include.df = inner_join(song.instances.df,
                                           song.instances.include.df,
                                           by = "song.instance.id")
    results.df = inner_join(results.df, song.instances.include.df,
                            by = "song.id") %>%
      select(song.id, song.name) %>%
      distinct
  }
  else {
    results.df = inner_join(results.df, song.instances.df, by = "song.id") %>%
      inner_join(song.instances.key.signatures.df, by = "song.instance.id") %>%
      filter(is.element(key.signature.id, input$keyChoices)) %>%
      select(song.id, song.name) %>%
      distinct
  }
}

# Filter by time signature
if(length(input$timeChoices) >= 1) {
  if(input$timeOptions == "Match all") {
    song.instances.include.df = select(song.instances.df, song.instance.id)
    for(timeChoiceID in input$timeChoices) {
      song.instances.include.df = inner_join(song.instances.include.df,
                                             song.instances.time.signatures.df,
                                             by = "song.instance.id") %>%
        filter(time.signature.id == timeChoiceID) %>%
        select(song.instance.id) %>%
        distinct
    }
    song.instances.include.df = inner_join(song.instances.df,
                                           song.instances.include.df,
                                           by = "song.instance.id")
    results.df = inner_join(results.df, song.instances.include.df,
                            by = "song.id") %>%
      select(song.id, song.name) %>%
      distinct
  }
  else {
    results.df = inner_join(results.df, song.instances.df, by = "song.id") %>%
      inner_join(song.instances.time.signatures.df, by = "song.instance.id") %>%
      filter(is.element(time.signature.id, input$timeChoices)) %>%
      select(song.id, song.name) %>%
      distinct
  }
}

# Filter by meter
if(length(input$meterChoices) >= 1) {
  if(input$meterOptions == "Match all") {
    song.instances.include.df = select(song.instances.df, song.instance.id)
    for(meterChoiceID in input$meterChoices) {
      song.instances.include.df = inner_join(song.instances.include.df,
                                             song.instances.lyrics.df,
                                             by = "song.instance.id") %>%
        inner_join(lyrics.meters.df, by = "lyrics.id") %>%
        inner_join(song.instances.tunes.df, by = "song.instance.id") %>%
        inner_join(tunes.meters.df, by = "tune.id") %>%
        select(song.instance.id, meter.id.x, meter.id.y) %>%
        gather(meter.source, meter.id, -song.instance.id) %>%
        select(song.instance.id, meter.id) %>%
        filter(meter.id == meterChoiceID) %>%
        select(song.instance.id) %>%
        distinct
    }
    song.instances.include.df = inner_join(song.instances.df,
                                           song.instances.include.df,
                                           by = "song.instance.id")
    results.df = inner_join(results.df, song.instances.include.df,
                            by = "song.id") %>%
      select(song.id, song.name) %>%
      distinct
  }
  else {
    results.df = inner_join(results.df, song.instances.df, by = "song.id") %>%
      inner_join(song.instances.lyrics.df, by = "song.instance.id") %>%
      inner_join(lyrics.meters.df, by = "lyrics.id") %>%
      inner_join(song.instances.tunes.df, by = "song.instance.id") %>%
      inner_join(tunes.meters.df, by = "tune.id") %>%
      filter(is.element(meter.id.x, input$meterChoices) | is.element(meter.id.y, input$meterChoices)) %>%
      select(song.id, song.name) %>%
      distinct
  }
}