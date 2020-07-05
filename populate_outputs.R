# Breaks for histograms by song year
songYearBreaks = seq(floor(min(allSongInfo$Year, na.rm = T) / 10) * 10, ceiling(max(allSongInfo$Year, na.rm = T) / 10) * 10, 10)

####################################
# Populate worship history outputs #
####################################

# Populate the list of recent songs
output$recentSongList <- renderTable({
  worshipHistory = inner_join(worshiphistory.tab, songinstances.tab) %>%
    inner_join(worshipslots.tab) %>%
    filter(WorshipHistoryDate >= input$recentSongsDateRange[1] & WorshipHistoryDate <= input$recentSongsDateRange[2]) %>%
    mutate(Date = format(WorshipHistoryDate, dateOutputFormat), Song = SongInstance, Position = WorshipSlot) %>%
    arrange(desc(WorshipHistoryDate), WorshipHistoryID) %>%
    select(Date, Song, Position)
})

# Populate the list of frequent songs
output$frequentSongList <- renderTable({
  worshipHistory = inner_join(worshiphistory.tab, songinstances.tab) %>%
    inner_join(songs.tab) %>%
    arrange(desc(WorshipHistoryDate), WorshipHistoryID) %>%
    filter(WorshipHistoryDate >= input$frequentSongsDateRange[1] & WorshipHistoryDate <= input$frequentSongsDateRange[2]) %>%
    select(SongName, WorshipHistoryDate) %>%
    group_by(SongName) %>%
    summarize(NumTimes = n_distinct(WorshipHistoryDate), LastSung = max(WorshipHistoryDate)) %>%
    filter(NumTimes >= input$frequentSongsCutoff) %>%
    arrange(desc(NumTimes), desc(LastSung), SongName) %>%
    mutate(Song = SongName, "Number of times sung" = NumTimes, "Last sung" = format(LastSung, "%B %e, %Y")) %>%
    select(Song, "Number of times sung", "Last sung")
}, digits = 0, align = 'lcl')

# Create the plot of frequent topics
output$frequentTopicPlot <- renderPlot({
  songTopicFrequency = inner_join(worshiphistory.tab, songinstances.tab) %>%
    inner_join(songs.topics.tab) %>%
    inner_join(topics.tab) %>%
    mutate(NewWorshipSlotID = ifelse(is.element(WorshipSlotID, c(4, 5)), 3, WorshipSlotID)) %>%
    inner_join(worshipslots.tab, by = c("NewWorshipSlotID" = "WorshipSlotID")) %>%
    filter(WorshipHistoryDate >= input$songYearDateRange[1] & WorshipHistoryDate <= input$songYearDateRange[2]) %>%
    select(SongID, TopicName, WorshipHistoryDate, WorshipSlotID, WorshipSlot) %>%
    distinct
  songTopicFrequency$WorshipSlot = factor(songTopicFrequency$WorshipSlot,
                                          levels = worshipslots.tab$WorshipSlot)
  if(length(input$recentTopicSlots) > 0) {
    songTopicFrequency = songTopicFrequency %>%
      filter(is.element(WorshipSlot, input$recentTopicSlots))
  }
  g = ggplot(songTopicFrequency, aes(x = reorder(TopicName, table(TopicName)[TopicName]))) +
    geom_bar() +
    coord_flip() +
    xlab("Topic") +
    ylab("Number of songs") +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          axis.ticks.x = element_blank())
  if(length(input$recentTopicSlots) > 0) {
    g = g +
      facet_grid(. ~ WorshipSlot) +
      theme(axis.text.x = element_blank())
  }
  g
})

# Create the plot of songs by year
output$songsByYear <- renderPlot({
  songYearFrequency = inner_join(allSongInstanceInfo, worshiphistory.tab, by = "SongInstanceID") %>%
    filter(WorshipHistoryDate >= input$songYearDateRange[1] & WorshipHistoryDate <= input$songYearDateRange[2] & !is.na(Year)) %>%
    inner_join(songinstances.lyrics.tab, by = c("SongInstanceID")) %>%
    inner_join(lyrics.tab, by = c("LyricsID")) %>%
    filter(LanguageID == 1) %>%
    select(Year, Title, WorshipHistoryDate) %>%
    distinct
  ggplot(songYearFrequency, aes(x = Year)) +
    geom_histogram(breaks = songYearBreaks) +
    xlab("Year written") +
    ylab("Number of songs") +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 14))
})

##################################
# Populate song analysis outputs #
##################################

# Create the bar plot of song year by songbook
output$yearBySongbook <- renderPlot({
  songbookAndYear = merge(allSongInstanceInfo, songinstances.tab, by = "SongInstanceID") %>%
    inner_join(songbookentries.tab) %>%
    inner_join(songbooks.tab) %>%
    select(SongInstanceID, Title, Year, SongbookID, SongbookName) %>%
    filter(is.element(SongbookName, input$songbooksToAnalyze) & !is.na(Year))
  distinct
  g = ggplot(songbookAndYear, aes(x = Year, col = SongbookName, fill = SongbookName)) +
    labs(fill = "Songbook", color = "Songbook") +
    xlab("Year written") +
    ylab("Number of songs") +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 14))
  if(input$songbookYearOptions == "Histogram (raw counts)") {
    g = g +
      geom_histogram(breaks = songYearBreaks, alpha = 0.1, position = "identity")
  }
  else if(input$songbookYearOptions == "Density (smoothed proportions)") {
    g = g +
      geom_density(breaks = songYearBreaks, alpha = 0.1) +
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank())
  }
  g
})

# Create the bar plot of topic by songbook
output$topicBySongbook <- renderPlot({
  if(length(input$songbooksToAnalyze) > 0) {
    songbookAndTopic = inner_join(songbookentries.tab, songinstances.tab) %>%
      inner_join(songs.tab) %>%
      inner_join(songs.topics.tab) %>%
      inner_join(topics.tab) %>%
      inner_join(songbooks.tab) %>%
      select(SongID, SongName, TopicID, TopicName, SongbookID, SongbookName) %>%
      filter(is.element(SongbookName, input$songbooksToAnalyze)) %>%
      distinct
    songbookAndTopicSum = group_by(songbookAndTopic, SongbookName, TopicName) %>%
      summarize(NumSongs = n_distinct(SongName)) %>%
      ungroup %>%
      complete(SongbookName, TopicName, fill = list(NumSongs = 0)) %>%
      inner_join(inner_join(songbookentries.tab, songinstances.tab) %>% inner_join(songbooks.tab) %>% group_by(SongbookName) %>% summarize(SongbookTotal = n_distinct(SongID))) %>%
      mutate(PropSongs = NumSongs / SongbookTotal)
    songbookAndTopicSum = songbookAndTopicSum %>%
      inner_join(group_by(songbookAndTopicSum, TopicName) %>% summarize(MeanProp = mean(PropSongs))) %>%
      mutate(Diff = PropSongs - MeanProp)
    if(input$songbookTopicOptions == "Number of songs") {
      songbookAndTopicSum = arrange(songbookAndTopicSum, SongbookName, NumSongs)
    }
    else if(input$songbookTopicOptions == "Topic") {
      songbookAndTopicSum = arrange(songbookAndTopicSum, SongbookName, desc(TopicName))
    }
    else if(input$songbookTopicOptions == "Relative number of songs") {
      songbookAndTopicSum = arrange(songbookAndTopicSum, SongbookName, Diff)
    }
    songbookAndTopicSum = mutate(songbookAndTopicSum, order = row_number())
    ggplot(songbookAndTopicSum, aes(x = order, y = NumSongs, fill = Diff)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient2(name = "",
                           midpoint = 0) +
      scale_x_continuous(breaks = songbookAndTopicSum$order,
                         labels = songbookAndTopicSum$TopicName,
                         expand = c(0, 0)) +
      coord_flip() +
      facet_wrap(~ SongbookName, scales = "free") +
      xlab("Topic") +
      ylab("Number of songs") +
      theme(legend.position = "none",
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14))
  }
})

# Create the bar plot of arrangement type by songbook
output$arrangementTypeBySongbook <- renderPlot({
  if(length(input$songbooksToAnalyze) > 0) {
    songbookAndArrangementType = inner_join(songbookentries.tab, songinstances.tab) %>%
      inner_join(songs.tab) %>%
      inner_join(arrangements.arrangementtypes.tab) %>%
      inner_join(arrangementtypes.tab) %>%
      inner_join(songbooks.tab) %>%
      select(SongID, SongName, ArrangementTypeID, ArrangementType, SongbookID, SongbookName) %>%
      filter(is.element(SongbookName, input$songbooksToAnalyze)) %>%
      distinct
    songbookAndArrangementTypeSum = group_by(songbookAndArrangementType, SongbookName, ArrangementType) %>%
      summarize(NumSongs = n_distinct(SongName)) %>%
      ungroup %>%
      complete(SongbookName, ArrangementType, fill = list(NumSongs = 0)) %>%
      inner_join(inner_join(songbookentries.tab, songinstances.tab) %>% inner_join(songbooks.tab) %>% group_by(SongbookName) %>% summarize(SongbookTotal = n_distinct(SongID))) %>%
      mutate(PropSongs = NumSongs / SongbookTotal)
    songbookAndArrangementTypeSum = songbookAndArrangementTypeSum %>%
      inner_join(group_by(songbookAndArrangementTypeSum, ArrangementType) %>% summarize(MeanProp = mean(PropSongs))) %>%
      mutate(Diff = PropSongs - MeanProp)
    if(input$songbookArrangementTypeOptions == "Number of songs") {
      songbookAndArrangementTypeSum = arrange(songbookAndArrangementTypeSum, SongbookName, NumSongs)
    }
    else if(input$songbookArrangementTypeOptions == "Arrangement type") {
      songbookAndArrangementTypeSum = arrange(songbookAndArrangementTypeSum, SongbookName, desc(ArrangementType))
    }
    else if(input$songbookArrangementTypeOptions == "Relative number of songs") {
      songbookAndArrangementTypeSum = arrange(songbookAndArrangementTypeSum, SongbookName, Diff)
    }
    songbookAndArrangementTypeSum = mutate(songbookAndArrangementTypeSum, order = row_number())
    ggplot(songbookAndArrangementTypeSum, aes(x = order, y = NumSongs, fill = Diff)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient2(name = "",
                           midpoint = 0) +
      scale_x_continuous(breaks = songbookAndArrangementTypeSum$order,
                         labels = songbookAndArrangementTypeSum$ArrangementType,
                         expand = c(0, 0)) +
      coord_flip() +
      facet_wrap(~ SongbookName, scales = "free") +
      xlab("Topic") +
      ylab("Number of songs") +
      theme(legend.position = "none",
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14))
  }
})

# Create the line graph of topic frequency over time
output$topicsOverTime <- renderPlot({
  if(length(input$topicsOverTimeTopics) > 0) {
    topicByYear = inner_join(allSongInfo, songs.topics.tab) %>%
      inner_join(topics.tab) %>%
      filter(is.element(TopicName, input$topicsOverTimeTopics)) %>%
      group_by(TopicName, Decade) %>%
      summarize(NumSongs = n_distinct(SongID)) %>%
      ungroup %>%
      complete(TopicName, Decade, fill = list(NumSongs = 0)) %>%
      inner_join(group_by(allSongInfo, Decade) %>% summarize(DecadeTotal = n())) %>%
      mutate(Prop = NumSongs / DecadeTotal,
             Lower = binom.confint(NumSongs, DecadeTotal, 0.95, "exact")$lower,
             Upper = binom.confint(NumSongs, DecadeTotal, 0.95, "exact")$upper)
    ggplot(topicByYear, aes(x = Decade, y = Prop)) +
      geom_line() +
      geom_ribbon(aes(x = Decade, ymin = Lower, ymax = Upper), alpha = 0.3, color = NA) +
      facet_wrap(~ TopicName) +
      scale_x_continuous("Decade",
                         limits = c(1800, max(songYearBreaks))) +
      ylab("Proportion of songs")
  }
})

# Create the line graph of references to books of the Bible over time
output$scriptureReferencesOverTime <- renderPlot({
  if(length(input$scriptureReferencesOverTimeBooks) > 0) {
    scriptureByYear = inner_join(allSongInfo, songinstances.tab) %>%
      inner_join(songinstances.lyrics.tab) %>%
      inner_join(lyrics.scripturereferences.tab) %>%
      inner_join(scripturereferences.tab) %>%
      inner_join(booksofthebible.tab) %>%
      filter(is.element(BookName, input$scriptureReferencesOverTimeBooks)) %>%
      group_by(BookName, Decade) %>%
      summarize(NumSongs = n_distinct(SongID)) %>%
      ungroup %>%
      complete(BookName, Decade, fill = list(NumSongs = 0)) %>%
      inner_join(group_by(allSongInfo, Decade) %>% summarize(DecadeTotal = n())) %>%
      mutate(Prop = NumSongs / DecadeTotal,
             Lower = binom.confint(NumSongs, DecadeTotal, 0.95, "exact")$lower,
             Upper = binom.confint(NumSongs, DecadeTotal, 0.95, "exact")$upper)
    scriptureByYear$BookName = factor(scriptureByYear$BookName,
                                      levels = names(bookList)[is.element(names(bookList), scriptureByYear$BookName)])
    ggplot(scriptureByYear, aes(x = Decade, y = Prop)) +
      geom_line() +
      geom_ribbon(aes(x = Decade, ymin = Lower, ymax = Upper), alpha = 0.3, color = NA) +
      facet_wrap(~ BookName) +
      scale_x_continuous("Decade",
                         limits = c(1800, max(songYearBreaks))) +
      ylab("Proportion of songs")
  }
})

# Create the line graph of artist gender over time
output$genderOverTime <- renderPlot({
  if(length(input$genderOverTimeRoles) > 0) {
    genderBySong = inner_join(allSongInfo, songinstances.tab) %>%
      select(SongID, Decade, SongInstanceID)
    genderOverTime = data.frame(SongID = c(), Decade = c(), GenderName = c(), NumArtists = c())
    if(is.element("Lyrics", input$genderOverTimeRoles)) {
      genderBySongLyrics = genderBySong %>%
        inner_join(songinstances.lyrics.tab) %>%
        inner_join(lyrics.artists.tab) %>%
        inner_join(artists.tab) %>%
        inner_join(genders.tab, by = c("Gender" = "GenderID")) %>%
        group_by(SongID, Decade, GenderName) %>%
        summarize(NumArtists = n_distinct(ArtistID))
      genderOverTime = rbind(genderOverTime, data.frame(genderBySongLyrics))
    }
    if(is.element("Music", input$genderOverTimeRoles)) {
      genderBySongMusic = genderBySong %>%
        inner_join(songinstances.tunes.tab) %>%
        inner_join(tunes.artists.tab) %>%
        inner_join(artists.tab) %>%
        inner_join(genders.tab, by = c("Gender" = "GenderID")) %>%
        group_by(SongID, Decade, GenderName) %>%
        summarize(NumArtists = n_distinct(ArtistID))
      genderOverTime = rbind(genderOverTime, data.frame(genderBySongMusic))
    }
    if(is.element("Arrangement", input$genderOverTimeRoles)) {
      genderBySongArrangement = genderBySong %>%
        inner_join(songinstances.tab) %>%
        inner_join(arrangements.artists.tab) %>%
        inner_join(artists.tab) %>%
        inner_join(genders.tab, by = c("Gender" = "GenderID")) %>%
        group_by(SongID, Decade, GenderName) %>%
        summarize(NumArtists = n_distinct(ArtistID))
      genderOverTime = rbind(genderOverTime, data.frame(genderBySongArrangement))
    }
    genderOverTime = genderOverTime %>%
      group_by(SongID, Decade, GenderName) %>%
      summarize(TotalArtists = sum(NumArtists)) %>%
      ungroup %>%
      filter(TotalArtists > 0) %>%
      group_by(GenderName, Decade) %>%
      summarize(NumSongs = n_distinct(SongID)) %>%
      ungroup %>%
      complete(GenderName, Decade, fill = list(NumSongs = 0)) %>%
      inner_join(group_by(allSongInfo, Decade) %>% summarize(DecadeTotal = n())) %>%
      mutate(Prop = NumSongs / DecadeTotal,
             Lower = binom.confint(NumSongs, DecadeTotal, 0.95, "exact")$lower,
             Upper = binom.confint(NumSongs, DecadeTotal, 0.95, "exact")$upper)
    genderOverTime$GenderName = factor(genderOverTime$GenderName, levels = c("Male", "Female"))
    ggplot(genderOverTime, aes(x = Decade, y = Prop, col = GenderName)) +
      geom_line() +
      geom_ribbon(aes(x = Decade, ymin = Lower, ymax = Upper, fill = GenderName), alpha = 0.3, color = NA) +
      scale_x_continuous("Decade",
                         limits = c(1800, max(songYearBreaks))) +
      scale_color_discrete("Artist gender") +
      scale_fill_discrete("Artist gender") +
      ylab("Proportion of songs")
  }
})