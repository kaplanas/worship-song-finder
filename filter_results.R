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
    songListResults = inner_join(songListResults, songinstances.tab) %>%
      filter(grepl(part, SongInstance, ignore.case = T)) %>%
      select(SongID, SongName) %>%
      distinct
  }
}

# Filter by artist
if(input$artistName != "") {
  songListResultsLyrics = data.frame(SongID = integer(), SongName = character(), ArtistID = integer())
  if(is.element(input$artistNameOptions, c("Lyrics", "Any"))) {
    songListResultsLyrics = inner_join(songListResults, songinstances.tab) %>%
      inner_join(songinstances.lyrics.tab) %>%
      inner_join(lyrics.artists.tab) %>%
      select(SongID, SongName, ArtistID) %>%
      distinct
  }
  songListResultsTune = data.frame(SongID = integer(), SongName = character(), ArtistID = integer())
  if(is.element(input$artistNameOptions, c("Music", "Any"))) {
    songListResultsTune = inner_join(songListResults, songinstances.tab) %>%
      inner_join(songinstances.tunes.tab) %>%
      inner_join(tunes.artists.tab) %>%
      select(SongID, SongName, ArtistID) %>%
      distinct
  }
  songListResultsArrangement = data.frame(SongID = integer(), SongName = character(), ArtistID = integer())
  if(is.element(input$artistNameOptions, c("Arrangement", "Any"))) {
    songListResultsArrangement = inner_join(songListResults, songinstances.tab) %>%
      inner_join(arrangements.artists.tab) %>%
      select(SongID, SongName, ArtistID) %>%
      distinct
  }
  parts = unlist(strsplit(input$artistName, " "))
  parts = parts[nchar(parts) > 0]
  parts = paste(parts, collapse = "|")
  songListResults = union(songListResultsLyrics, songListResultsTune) %>%
    union(songListResultsArrangement) %>%
    inner_join(artists.tab) %>%
    select(SongID, SongName, FirstName, LastName) %>%
    filter(grepl(parts, FirstName, ignore.case = T) | grepl(parts, LastName, ignore.case = T)) %>%
    select(SongID, SongName) %>%
    distinct
}

# Filter by topic
if(length(input$topicChoices) >= 1) {
  if(input$topicOptions == "Match all") {
    for(topicChoiceID in input$topicChoices) {
      songListResults = inner_join(songListResults, songs.topics.tab) %>%
        filter(TopicID == topicChoiceID) %>%
        select(SongID, SongName) %>%
        distinct
    }
  }
  else {
    songListResults = inner_join(songListResults, songs.topics.tab) %>%
      filter(is.element(TopicID, input$topicChoices)) %>%
      select(SongID, SongName) %>%
      distinct
  }
}

# Filter by scripture reference
if(input$scriptureBook != 0) {
  songListResults = inner_join(songListResults, songinstances.tab) %>%
    inner_join(songinstances.lyrics.tab) %>%
    inner_join(lyrics.scripturereferences.tab) %>%
    inner_join(scripturereferences.tab)
  if(input$scriptureOptions == "Single verse") {
    songListResults = filter(songListResults,
                             BookID == input$scriptureBook &
                             Chapter == input$scriptureChapterStart &
                             Verse == input$scriptureVerseStart) %>%
      select(SongID, SongName) %>%
      distinct
  }
  else {
    songListResults = filter(songListResults,
                             BookID == input$scriptureBook &
                             (Chapter > input$scriptureChapterStart |
                              (Chapter == input$scriptureChapterStart &
                               Verse >= input$scriptureVerseStart)) &
                             (Chapter < input$scriptureChapterEnd |
                              (Chapter == input$scriptureChapterEnd &
                               Verse <= input$scriptureVerseEnd))) %>%
      select(SongID, SongName) %>%
      distinct
  }
}

# Filter by language
if(length(input$languageChoices) >= 1) {
  if(input$languageOptions == "Match all") {
    for(languageChoiceID in input$languageChoices) {
      songListResults = inner_join(songListResults, songinstances.tab) %>%
        inner_join(songinstances.lyrics.tab) %>%
        inner_join(lyrics.tab) %>%
        filter(LanguageID == languageChoiceID) %>%
        select(SongID, SongName) %>%
        distinct
    }
  }
  else {
    songListResults = inner_join(songListResults, songinstances.tab) %>%
      inner_join(songinstances.lyrics.tab) %>%
      inner_join(lyrics.tab) %>%
      filter(is.element(LanguageID, input$languageChoices)) %>%
      select(SongID, SongName) %>%
      distinct
  }
}

# Filter by songbook
if(length(input$songbookChoices) >= 1) {
  if(input$songbookOptions == "Match all") {
    for(songbookChoiceID in input$songbookChoices) {
      songListResults = inner_join(songListResults, songinstances.tab) %>%
        inner_join(songbookentries.tab) %>%
        filter(SongbookID == songbookChoiceID) %>%
        select(SongID, SongName) %>%
        distinct
    }
  }
  else {
    songListResults = inner_join(songListResults, songinstances.tab) %>%
      inner_join(songbookentries.tab) %>%
      filter(is.element(SongbookID, input$songbookChoices)) %>%
      select(SongID, SongName) %>%
      distinct
  }
}

# Filter by arrangement type
if(length(input$arrangementChoices) >= 1) {
  if(input$arrangementOptions == "Include") {
    songListResults = inner_join(songListResults, songinstances.tab) %>%
      inner_join(arrangements.arrangementtypes.tab) %>%
      filter(is.element(ArrangementTypeID, input$arrangementChoices)) %>%
      select(SongID, SongName) %>%
      distinct
  }
  else {
    songInstancesToExclude = inner_join(songinstances.tab, arrangements.arrangementtypes.tab) %>%
      filter(is.element(ArrangementTypeID, input$arrangementChoices)) %>%
      select(SongInstanceID, SongID) %>%
      distinct
    songInstancesToInclude = select(songinstances.tab, SongInstanceID, SongID) %>%
      setdiff(songInstancesToExclude)
    songListResults = inner_join(songListResults, songInstancesToInclude) %>%
      select(SongID, SongName) %>%
      distinct
  }
}

# Filter by key signature
if(length(input$keyChoices) >= 1) {
  if(input$keyOptions == "Match all") {
    songInstancesToInclude = select(songinstances.tab, SongInstanceID)
    for(keyChoiceID in input$keyChoices) {
      songInstancesToInclude = inner_join(songInstancesToInclude, songinstances.keysignatures.tab) %>%
        filter(KeySignatureID == keyChoiceID) %>%
        select(SongInstanceID) %>%
        distinct
    }
    songInstancesToInclude = inner_join(songinstances.tab, songInstancesToInclude)
    songListResults = inner_join(songListResults, songInstancesToInclude) %>%
      select(SongID, SongName) %>%
      distinct
  }
  else {
    songListResults = inner_join(songListResults, songinstances.tab) %>%
      inner_join(songinstances.keysignatures.tab) %>%
      filter(is.element(KeySignatureID, input$keyChoices)) %>%
      select(SongID, SongName) %>%
      distinct
  }
}

# Filter by time signature
if(length(input$timeChoices) >= 1) {
  if(input$timeOptions == "Match all") {
    songInstancesToInclude = select(songinstances.tab, SongInstanceID)
    for(timeChoiceID in input$timeChoices) {
      songInstancesToInclude = inner_join(songInstancesToInclude, songinstances.timesignatures.tab) %>%
        filter(TimeSignatureID == timeChoiceID) %>%
        select(SongInstanceID) %>%
        distinct
    }
    songInstancesToInclude = inner_join(songinstances.tab, songInstancesToInclude)
    songListResults = inner_join(songListResults, songInstancesToInclude) %>%
      select(SongID, SongName) %>%
      distinct
  }
  else {
    songListResults = inner_join(songListResults, songinstances.tab) %>%
      inner_join(songinstances.timesignatures.tab) %>%
      filter(is.element(TimeSignatureID, input$timeChoices)) %>%
      select(SongID, SongName) %>%
      distinct
  }
}

# Filter by meter
if(length(input$meterChoices) >= 1) {
  if(input$meterOptions == "Match all") {
    songInstancesToInclude = select(songinstances.tab, SongInstanceID)
    for(meterChoiceID in input$meterChoices) {
      songInstancesToInclude = inner_join(songInstancesToInclude, songinstances.lyrics.tab) %>%
        inner_join(lyrics.meters.tab) %>%
        inner_join(songinstances.tunes.tab) %>%
        inner_join(tunes.meters.tab, by = "TuneID") %>%
        select(SongInstanceID, MeterID.x, MeterID.y) %>%
        gather(MeterSource, MeterID, -SongInstanceID) %>%
        select(SongInstanceID, MeterID) %>%
        filter(MeterID == meterChoiceID) %>%
        select(SongInstanceID) %>%
        distinct
    }
    songInstancesToInclude = inner_join(songinstances.tab, songInstancesToInclude)
    songListResults = inner_join(songListResults, songInstancesToInclude) %>%
      select(SongID, SongName) %>%
      distinct
  }
  else {
    songListResults = inner_join(songListResults, songinstances.tab) %>%
      inner_join(songinstances.lyrics.tab) %>%
      inner_join(lyrics.meters.tab) %>%
      inner_join(songinstances.tunes.tab) %>%
      inner_join(tunes.meters.tab, by = "TuneID") %>%
      filter(is.element(MeterID.x, input$meterChoices) | is.element(MeterID.y, input$meterChoices)) %>%
      select(SongID, SongName) %>%
      distinct
  }
}

# Filter by date last sung
if(input$filterByDate) {
  songsSungSinceCutoff = inner_join(songs.tab, songinstances.tab) %>%
    inner_join(worshiphistory.tab) %>%
    filter(WorshipHistoryDate >= input$notSungSinceDate) %>%
    select(SongID, SongName)
  songListResults = setdiff(songListResults, songsSungSinceCutoff)
}

# Filter by survey and special requests
if(length(input$requestChoices) >= 1) {
  if(is.element("request", input$requestChoices)) {
    songListResults = inner_join(songListResults, specialrequests.tab) %>%
      select(SongID, SongName) %>%
      distinct
  }
}