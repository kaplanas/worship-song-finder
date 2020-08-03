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
    results.df = results.df %>%
      inner_join(song.instances.df, by = "song.id") %>%
      filter(grepl(part, song.instance, ignore.case = T)) %>%
      dplyr::select(song.id, song.name) %>%
      distinct()
  }
}

# Filter by artist
if(input$artistName != "") {
  parts = unlist(strsplit(input$artistName, " "))
  parts = parts[nchar(parts) > 0]
  parts = paste(parts, collapse = "|")
  results.df = song.instances.artists.df %>%
    inner_join(artists.df, by = "artist.id") %>%
    filter(input$artistNameOptions == "Any" |
             (input$artistNameOptions == "Lyrics" & role == "lyricist") |
             (input$artistNameOptions == "Music" & role == "composer") |
             (input$artistNameOptions == "Arrangement" & role == "arranger"),
           grepl(parts, first.name, ignore.case = T) |
             grepl(parts, last.name, ignore.case = T)) %>%
    inner_join(results.df, by = "song.id") %>%
    dplyr::select(song.id, song.name) %>%
    distinct()
}

# Filter by topic
if(length(input$topicChoices) >= 1) {
  if(input$topicOptions == "Match all") {
    for(topic.choice.id in input$topicChoices) {
      results.df = results.df %>%
        inner_join(songs.topics.df, by = "song.id") %>%
        filter(topic.id == topic.choice.id) %>%
        dplyr::select(song.id, song.name) %>%
        distinct
    }
  }
  else {
    results.df = results.df %>%
      inner_join(songs.topics.df, by = "song.id") %>%
      filter(topic.id %in% input$topicChoices) %>%
      dplyr::select(song.id, song.name) %>%
      distinct
  }
}

# Filter by scripture reference
if(input$scriptureBook != 0) {
  if(input$scriptureOptions == "Single verse") {
    results.df = song.instances.scripture.references.df %>%
      inner_join(scripture.references.df, by = "scripture.reference.id") %>%
      filter(book.id == input$scriptureBook,
             chapter == input$scriptureChapterStart,
             verse == input$scriptureVerseStart) %>%
      inner_join(results.df, by = "song.id") %>%
      dplyr::select(song.id, song.name) %>%
      distinct()
  }
  else {
    results.df = song.instances.scripture.references.df %>%
      inner_join(scripture.references.df, by = "scripture.reference.id") %>%
      filter(book.id == input$scriptureBook,
             chapter > input$scriptureChapterStart |
               (chapter == input$scriptureChapterStart &
                  verse >= input$scriptureVerseStart),
             chapter < input$scriptureChapterEnd |
               (chapter == input$scriptureChapterEnd &
                  verse <= input$scriptureVerseEnd)) %>%
      inner_join(results.df, by = "song.id") %>%
      dplyr::select(song.id, song.name) %>%
      distinct()
  }
}

# Filter by language
if(length(input$languageChoices) >= 1) {
  if(input$languageOptions == "Match all") {
    for(language.choice.id in input$languageChoices) {
      results.df = song.instances.languages.df %>%
        filter(language.id == language.choice.id) %>%
        inner_join(results.df, by = "song.id") %>%
        dplyr::select(song.id, song.name) %>%
        distinct()
    }
  }
  else {
    results.df = song.instances.languages.df %>%
      filter(is.element(language.id, input$languageChoices)) %>%
      inner_join(results.df, by = "song.id") %>%
      dplyr::select(song.id, song.name) %>%
      distinct()
  }
}

# Filter by songbook
if(length(input$songbookChoices) >= 1) {
  if(input$songbookOptions == "Match all") {
    for(songbook.choice.id in input$songbookChoices) {
      results.df = song.instances.songbooks.df %>%
        filter(songbook.id == songbook.choice.id) %>%
        inner_join(results.df, by = "song.id")
        select(song.id, song.name) %>%
        distinct()
    }
  }
  else {
    results.df = song.instances.songbooks.df %>%
      filter(is.element(songbook.id, input$songbookChoices)) %>%
      inner_join(results.df, by = "song.id") %>%
      select(song.id, song.name) %>%
      distinct
  }
}

# Filter by arrangement type
if(length(input$arrangementChoices) >= 1) {
  if(input$arrangementOptions == "Include") {
    results.df = song.instances.arrangement.types.df %>%
      inner_join(arrangement.types.df, by = "arrangement.type.id") %>%
      filter(arrangement.type.id %in% input$arrangementChoices) %>%
      inner_join(results.df, by = "song.id") %>%
      dplyr::select(song.id, song.name) %>%
      distinct()
  }
  else {
    song.instances.exclude.df = song.instances.arrangement.types.df %>%
      inner_join(arrangement.types.df, by = "arrangement.type.id") %>%
      filter(arrangement.type.id %in% input$arrangementChoices) %>%
      dplyr::select(song.instance.id, song.id) %>%
      distinct()
    song.instances.include.df = song.instances.df %>%
      dplyr::select(song.instance.id, song.id) %>%
      setdiff(song.instances.exclude.df)
    results.df = song.instances.include.df %>%
      inner_join(results.df, by = "song.id") %>%
      dplyr::select(song.id, song.name) %>%
      distinct()
  }
}

# Filter by key signature
if(length(input$keyChoices) >= 1) {
  if(input$keyOptions == "Match all") {
    song.instances.include.df = song.instances.df %>%
      dplyr::select(song.instance.id)
    for(key.choice.id in input$keyChoices) {
      song.instances.include.df = song.instances.include.df %>%
        inner_join(song.instances.key.signatures.df,
                   by = "song.instance.id") %>%
        filter(key.signature.id == keyChoiceID) %>%
        dplyr::select(song.instance.id) %>%
        distinct()
    }
    song.instances.include.df = song.instances.df %>%
      inner_join(song.instances.include.df, by = "song.instance.id")
    results.df = results.df %>%
      inner_join(song.instances.include.df, by = "song.id") %>%
      dplyr::select(song.id, song.name) %>%
      distinct()
  }
  else {
    results.df = song.instances.key.signatures.df %>%
      filter(key.signature.id %in% input$keyChoices) %>%
      inner_join(results.df, by = "song.id") %>%
      dplyr::select(song.id, song.name) %>%
      distinct()
  }
}

# Filter by time signature
if(length(input$timeChoices) >= 1) {
  if(input$timeOptions == "Match all") {
    song.instances.include.df = song.instances.df %>%
      dplyr::select(song.instance.id)
    for(time.choice.id in input$timeChoices) {
      song.instances.include.df = song.instances.time.signatures.df %>%
        inner_join(song.instances.include.df, by = "song.instance.id") %>%
        filter(time.signature.id == time.choice.id) %>%
        dplyr::select(song.instance.id) %>%
        distinct()
    }
    song.instances.include.df = song.instances.df %>%
      inner_join(song.instances.include.df, by = "song.instance.id")
    results.df = song.instances.include.df %>%
      inner_join(results.df, by = "song.id") %>%
      dplyr::select(song.id, song.name) %>%
      distinct()
  }
  else {
    results.df = song.instances.time.signatures.df %>%
      filter(time.signature.id %in% input$timeChoices) %>%
      inner_join(results.df) %>%
      dplyr::select(song.id, song.name) %>%
      distinct()
  }
}

# Filter by meter
if(length(input$meterChoices) >= 1) {
  if(input$meterOptions == "Match all") {
    song.instances.include.df = song.instances.df %>%
      dplyr::select(song.instance.id)
    for(meter.choice.id in input$meterChoices) {
      song.instances.include.df = song.instances.include.df %>%
        inner_join(song.instances.meters.df, by = "song.instance.id") %>%
        filter(meter.id == meter.choice.id) %>%
        dplyr::select(song.instance.id) %>%
        distinct()
    }
    song.instances.include.df = song.instances.df %>%
      inner_join(song.instances.include.df, by = "song.instance.id")
    results.df = song.instancecs.include.df %>%
      inner_join(results.df, by = "song.id") %>%
      dplyr::select(song.id, song.name) %>%
      distinct()
  }
  else {
    results.df = song.instances.meters.df %>%
      filter(meter.id %in% input$meterChoices) %>%
      inner_join(results.df, by = "song.id") %>%
      dplyr::select(song.id, song.name) %>%
      distinct()
  }
}