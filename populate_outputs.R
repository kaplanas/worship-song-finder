# Breaks for song year axes
song.year.breaks = seq(1000, 3000, 100)

#### Populate worship history outputs ####

if(version == "ctcc") {

  # Populate the list of recent songs
  output$recentSongList <- renderTable({
    recent.songs.df = worship.history.df %>%
      inner_join(song.instances.df, by = "song.instance.id") %>%
      inner_join(worship.slots.df, by = "worship.slot.id") %>%
      filter(worship.history.date >= input$recentSongsDateRange[1],
             worship.history.date <= input$recentSongsDateRange[2]) %>%
      mutate(Date = format(worship.history.date, date.output.format),
             Song = song.instance,
             Position = worship.slot) %>%
      arrange(desc(worship.history.date), worship.history.id) %>%
      dplyr::select(Date, Song, Position)
  })
  
  # Populate the list of frequent songs
  output$frequentSongList <- renderTable({
    frequent.songs.df = worship.history.df %>%
      inner_join(song.instances.df, by = "song.instance.id") %>%
      inner_join(songs.df, by = "song.id") %>%
      filter(worship.history.date >= input$frequentSongsDateRange[1],
             worship.history.date <= input$frequentSongsDateRange[2]) %>%
      dplyr::select(song.name, worship.history.date) %>%
      group_by(song.name) %>%
      summarise(num.times = n_distinct(worship.history.date),
                last.sung = max(worship.history.date)) %>%
      ungroup() %>%
      filter(num.times >= input$frequentSongsCutoff) %>%
      arrange(desc(num.times), desc(last.sung), song.name) %>%
      mutate(Song = song.name,
             "Number of times sung" = num.times,
             "Last sung" = format(last.sung, "%B %e, %Y")) %>%
      dplyr::select(Song, "Number of times sung", "Last sung")
  }, digits = 0, align = 'lcl')
  
  # Create the plot of frequent topics
  output$frequentTopicPlot <- renderPlot({
    frequent.topics.df = worship.history.df %>%
      inner_join(song.instances.df, by = "song.instance.id") %>%
      inner_join(songs.topics.df, by = "song.id") %>%
      inner_join(topics.df, by = "topic.id") %>%
      mutate(new.worship.slot.id = ifelse(is.element(worship.slot.id, c(4, 5)), 3,
                                          worship.slot.id)) %>%
      inner_join(worship.slots.df,
                 by = c("new.worship.slot.id" = "worship.slot.id")) %>%
      filter(worship.history.date >= input$songYearDateRange[1],
             worship.history.date <= input$songYearDateRange[2]) %>%
      dplyr::select(song.id, topic.name, worship.history.date, worship.slot.id,
                    worship.slot) %>%
      distinct()
    frequent.topics.df$worship.slot = factor(frequent.topics.df$worship.slot,
                                             levels = worship.slots.df$worship.slot)
    if(length(input$recentTopicSlots) > 0) {
      frequent.topics.df = frequent.topics.df %>%
        filter(worship.slot %in% input$recentTopicSlots)
    }
    g = ggplot(frequent.topics.df,
               aes(x = reorder(topic.name, table(topic.name)[topic.name]))) +
      geom_bar() +
      coord_flip() +
      xlab("Topic") +
      ylab("Number of songs") +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            axis.ticks.x = element_blank())
    if(length(input$recentTopicSlots) > 0) {
      g = g +
        facet_grid(. ~ worship.slot) +
        theme(axis.text.x = element_blank())
    }
    g
  })
  
  # Create the plot of songs by year
  output$songsByYear <- renderPlot({
    song.years.df = song.instance.info.df %>%
      inner_join(worship.history.df, by = "song.instance.id") %>%
      filter(worship.history.date >= input$songYearDateRange[1],
             worship.history.date <= input$songYearDateRange[2],
             !is.na(year)) %>%
      inner_join(song.instances.languages.df, by = "song.instance.id") %>%
      filter(language.id == 1) %>%
      dplyr::select(year, title, worship.history.date) %>%
      distinct()
    ggplot(song.years.df, aes(x = year)) +
      geom_histogram(breaks = song.year.breaks) +
      xlab("Year written") +
      ylab("Number of songs") +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 14))
  })

}

#### Populate song analysis outputs ####

# Function that creates a bar plot of Xs by songbook
bar.plot.by.songbook = function(data, grouping.col, input.vals,
                                display.options) {
  if(length(input.vals) > 0) {
    groups.by.songbook.df = data
    groups.by.songbook.df$gc = groups.by.songbook.df[,grouping.col]
    groups.by.songbook.df = groups.by.songbook.df %>%
      filter(songbook.name %in% input.vals) %>%
      group_by(songbook.name, gc) %>%
      summarise(num.songs = n_distinct(song.id)) %>%
      ungroup() %>%
      complete(songbook.name, gc, fill = list(num.songs = 0)) %>%
      group_by(songbook.name) %>%
      mutate(songbook.total = sum(num.songs)) %>%
      ungroup() %>%
      mutate(prop.songs = num.songs / songbook.total) %>%
      group_by(gc) %>%
      mutate(mean.prop = mean(prop.songs)) %>%
      ungroup() %>%
      mutate(diff = prop.songs - mean.prop)
    if(display.options == "Number of songs") {
      groups.by.songbook.df = groups.by.songbook.df %>%
        arrange(songbook.name, num.songs)
    } else if(display.options == "Relative number of songs") {
      groups.by.songbook.df = groups.by.songbook.df %>%
        arrange(songbook.name, diff)
    } else {
      groups.by.songbook.df = groups.by.songbook.df %>%
        arrange(songbook.name, desc(gc))
    }
    groups.by.songbook.df = groups.by.songbook.df %>%
      mutate(order = row_number())
    ggplot(groups.by.songbook.df, aes(x = order, y = num.songs, fill = diff)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_gradient2(name = "",
                           midpoint = 0) +
      scale_x_continuous(breaks = groups.by.songbook.df$order,
                         labels = groups.by.songbook.df$gc,
                         expand = c(0, 0)) +
      coord_flip() +
      facet_wrap(~ songbook.name, scales = "free") +
      xlab("Topic") +
      ylab("Number of songs") +
      theme(legend.position = "none",
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14))
  }
}

# Create the bar plot of song year by songbook
output$yearBySongbook <- renderPlot({
  song.instance.info.df %>%
    inner_join(song.instances.songbooks.df, by = "song.instance.id") %>%
    dplyr::select(song.instance.id, title, decade, songbook.id, songbook.name) %>%
    filter(is.element(songbook.name, input$songbooksToAnalyze),
           !is.na(decade)) %>%
    distinct() %>%
    ggplot(aes(x = decade, col = songbook.name, fill = songbook.name)) +
    geom_bar(position = "identity", alpha = 0.1) +
    scale_x_continuous("Year written", breaks = song.year.breaks) +
    scale_y_continuous("Number of songs") +
    scale_color_discrete("Songbook") +
    scale_fill_discrete("Songbook") +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12))
})

# Create the bar plot of topic by songbook
output$topicBySongbook <- renderPlot({
  bar.plot.by.songbook(data = song.instances.songbooks.df %>%
                         inner_join(songs.topics.df, by = "song.id") %>%
                         inner_join(topics.df, by = "topic.id"),
                       grouping.col = "topic.name",
                       input.vals = input$songbooksToAnalyze,
                       display.options = input$songbookTopicOptions)
})

# Create the bar plot of arrangement type by songbook
output$arrangementTypeBySongbook <- renderPlot({
  bar.plot.by.songbook(data = song.instances.songbooks.df %>%
                         dplyr::select(song.instance.id, songbook.name) %>%
                         inner_join(song.instances.arrangement.types.df,
                                    by = "song.instance.id") %>%
                         inner_join(arrangement.types.df,
                                    by = "arrangement.type.id"),
                       grouping.col = "arrangement.type",
                       input.vals = input$songbooksToAnalyze,
                       display.options = input$songbookArrangementTypeOptions)
})

# Create the bar plot of mode by songbook
output$modeBySongbook <- renderPlot({
  bar.plot.by.songbook(data = song.instances.songbooks.df %>%
                         dplyr::select(song.instance.id, songbook.name) %>%
                         inner_join(song.instances.key.signatures.df,
                                    by = "song.instance.id") %>%
                         inner_join(key.signatures.df, by = "key.signature.id"),
                       grouping.col = "mode.name",
                       input.vals = input$songbooksToAnalyze,
                       display.options = input$songbookModeOptions)
})

# Create the bar plot of time signature by songbook
output$timeSignatureBySongbook <- renderPlot({
  bar.plot.by.songbook(data = song.instances.songbooks.df %>%
                         dplyr::select(song.instance.id, songbook.name) %>%
                         inner_join(song.instances.time.signatures.df,
                                    by = "song.instance.id") %>%
                         inner_join(time.signatures.df,
                                    by = "time.signature.id"),
                       grouping.col = "time.signature.string",
                       input.vals = input$songbooksToAnalyze,
                       display.options = input$songbookTimeSignatureOptions)
})

# Create the line graph of topic frequency over time
output$topicsOverTime <- renderPlot({
  if(length(input$topicsOverTimeTopics) > 0) {
    topic.year.df = song.info.df %>%
      inner_join(songs.topics.df, by = "song.id") %>%
      inner_join(topics.df, by = "topic.id") %>%
      filter(topic.name %in% input$topicsOverTimeTopics) %>%
      group_by(topic.name, decade) %>%
      summarise(num.songs = n_distinct(song.id)) %>%
      ungroup %>%
      complete(topic.name, decade, fill = list(num.songs = 0)) %>%
      inner_join(group_by(song.info.df, decade) %>%
                   summarise(decade.total = n()),
                 by = "decade") %>%
      mutate(prop = num.songs / decade.total,
             lower = binom.confint(num.songs, decade.total, 0.95, "exact")$lower,
             upper = binom.confint(num.songs, decade.total, 0.95, "exact")$upper)
    ggplot(topic.year.df, aes(x = decade, y = prop)) +
      geom_line() +
      geom_ribbon(aes(x = decade, ymin = lower, ymax = upper),
                  alpha = 0.3, color = NA) +
      facet_wrap(~ topic.name) +
      scale_x_continuous("Decade",
                         limits = c(1800, max(song.year.breaks))) +
      ylab("Proportion of songs")
  }
})

# Create the line graph of references to books of the Bible over time
output$scriptureReferencesOverTime <- renderPlot({
  if(length(input$scriptureReferencesOverTimeBooks) > 0) {
    scripture.year.df = song.info.df %>%
      inner_join(song.instances.scripture.references.df, by = "song.id") %>%
      inner_join(scripture.references.df, by = "scripture.reference.id") %>%
      filter(book.name %in% input$scriptureReferencesOverTimeBooks) %>%
      group_by(book.name, decade) %>%
      summarise(num.songs = n_distinct(song.id)) %>%
      ungroup() %>%
      complete(book.name, decade, fill = list(num.songs = 0)) %>%
      inner_join(group_by(song.info.df, decade) %>%
                   summarise(decade.total = n()),
                 by = "decade") %>%
      mutate(prop = num.songs / decade.total,
             lower = binom.confint(num.songs, decade.total, 0.95, "exact")$lower,
             upper = binom.confint(num.songs, decade.total, 0.95, "exact")$upper)
    scripture.year.df$book.name = factor(scripture.year.df$book.name,
                                         levels = names(book.list)[is.element(names(book.list),
                                                                              scripture.year.df$book.name)])
    ggplot(scripture.year.df, aes(x = decade, y = prop)) +
      geom_line() +
      geom_ribbon(aes(x = decade, ymin = lower, ymax = upper),
                  alpha = 0.3, color = NA) +
      facet_wrap(~ book.name) +
      scale_x_continuous("Decade",
                         limits = c(1800, max(song.year.breaks))) +
      ylab("Proportion of songs")
  }
})

# Create the line graph of artist gender over time
output$genderOverTime <- renderPlot({
  if(length(input$genderOverTimeRoles) > 0) {
    gender.song.df = song.info.df %>%
      filter(!is.na(decade)) %>%
      dplyr::select(song.id, decade)
    gender.time.df = gender.song.df %>%
      inner_join(song.instances.artists.df %>%
                   filter((role == "lyricist" & "Lyrics" %in% input$genderOverTimeRoles) |
                            (role == "composer" & "Music" %in% input$genderOverTimeRoles) |
                            (role == "arranger" & "Arrangement" %in% input$genderOverTimeRoles)),
                 by = "song.id") %>%
      inner_join(artists.df, by = "artist.id") %>%
      filter(gender != "NA") %>%
      group_by(song.id, decade, gender) %>%
      summarise(num.artists = n_distinct(artist.id))
    gender.time.df = gender.time.df %>%
      group_by(song.id, decade, gender) %>%
      summarise(total.artists = sum(num.artists)) %>%
      ungroup() %>%
      filter(total.artists > 0) %>%
      group_by(gender, decade) %>%
      summarise(num.songs = n_distinct(song.id)) %>%
      ungroup() %>%
      complete(gender, decade, fill = list(num.songs = 0)) %>%
      inner_join(group_by(song.info.df, decade) %>%
                   summarise(decade.total = n_distinct(song.id)),
                 by = "decade") %>%
      mutate(prop = num.songs / decade.total,
             lower = binom.confint(num.songs, decade.total, 0.95, "exact")$lower,
             upper = binom.confint(num.songs, decade.total, 0.95, "exact")$upper)
    gender.time.df$gender = factor(gender.time.df$gender,
                                        levels = c("Male", "Female"))
    ggplot(gender.time.df, aes(x = decade, y = prop, col = gender)) +
      geom_line() +
      geom_ribbon(aes(x = decade, ymin = lower, ymax = upper,
                      fill = gender), alpha = 0.3, color = NA) +
      scale_x_continuous("Decade",
                         limits = c(1800, max(song.year.breaks))) +
      scale_color_discrete("Artist gender") +
      scale_fill_discrete("Artist gender") +
      ylab("Proportion of songs")
  }
})