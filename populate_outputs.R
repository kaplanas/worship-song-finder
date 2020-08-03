# Breaks for histograms by song year
song.year.breaks = seq(floor(min(song.info.df$year, na.rm = T) / 10) * 10,
                       ceiling(max(song.info.df$year, na.rm = T) / 10) * 10, 10)

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
      arrange(desc(worship.history.date), worship.history.id) %>%
      filter(worship.history.date >= input$frequentSongsDateRange[1],
             worship.history.date <= input$frequentSongsDateRange[2]) %>%
      select(song.name, worship.history.date) %>%
      group_by(song.name) %>%
      summarise(num.times = n_distinct(worship.history.date),
                last.sung = max(worship.history.date)) %>%
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

# Create the bar plot of song year by songbook
output$yearBySongbook <- renderPlot({
  songbook.year.df = song.instance.info.df %>%
    inner_join(song.instances.songbooks.df, by = "song.instance.id") %>%
    dplyr::select(song.instance.id, title, year, songbook.id, songbook.name) %>%
    filter(is.element(songbook.name, input$songbooksToAnalyze),
           !is.na(year)) %>%
    distinct()
  g = ggplot(songbook.year.df,
             aes(x = year, col = songbook.name, fill = songbook.name)) +
    labs(fill = "Songbook", color = "Songbook") +
    xlab("Year written") +
    ylab("Number of songs") +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 14))
  if(input$songbookYearOptions == "Histogram (raw counts)") {
    g = g +
      geom_histogram(breaks = song.year.breaks, alpha = 0.1,
                     position = "identity")
  }
  else if(input$songbookYearOptions == "Density (smoothed proportions)") {
    g = g +
      geom_density(breaks = song.year.breaks, alpha = 0.1) +
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank())
  }
  g
})

# Create the bar plot of topic by songbook
output$topicBySongbook <- renderPlot({
  if(length(input$songbooksToAnalyze) > 0) {
    songbook.topic.df = song.instances.df %>%
      inner_join(song.instances.songbooks.df,
                 by = c("song.instance.id", "song.id")) %>%
      inner_join(songs.df, by = "song.id") %>%
      inner_join(songs.topics.df, by = "song.id") %>%
      inner_join(topics.df, by = "topic.id") %>%
      dplyr::select(song.id, song.name, topic.id, topic.name, songbook.id,
                    songbook.name) %>%
      filter(songbook.name %in% input$songbooksToAnalyze) %>%
      distinct()
    songbook.topic.sum.df = songbook.topic.df %>%
      group_by(songbook.name, topic.name) %>%
      summarise(num.songs = n_distinct(song.name)) %>%
      ungroup %>%
      complete(songbook.name, topic.name, fill = list(num.songs = 0)) %>%
      inner_join(song.instances.songbooks.df %>%
                   group_by(songbook.name) %>%
                   summarise(songbook.total = n_distinct(song.id)),
                 by = "songbook.name") %>%
      mutate(prop.songs = num.songs / songbook.total)
    songbook.topic.sum.df = songbook.topic.sum.df %>%
      inner_join(songbook.topic.sum.df %>%
                   group_by(topic.name) %>%
                   summarise(mean.prop = mean(prop.songs)),
                 by = "topic.name") %>%
      mutate(diff = prop.songs - mean.prop)
    if(input$songbookTopicOptions == "Number of songs") {
      songbook.topic.sum.df = songbook.topic.sum.df %>%
        arrange(songbook.name, num.songs)
    }
    else if(input$songbookTopicOptions == "Topic") {
      songbook.topic.sum.df = songbook.topic.sum.df %>%
        arrange(songbook.name, desc(topic.name))
    }
    else if(input$songbookTopicOptions == "Relative number of songs") {
      songbook.topic.sum.df = songbook.topic.sum.df %>%
        arrange(songbook.name, diff)
    }
    songbook.topic.sum.df = songbook.topic.sum.df %>%
      mutate(order = row_number())
    ggplot(songbook.topic.sum.df, aes(x = order, y = num.songs, fill = diff)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient2(name = "",
                           midpoint = 0) +
      scale_x_continuous(breaks = songbook.topic.sum.df$order,
                         labels = songbook.topic.sum.df$topic.name,
                         expand = c(0, 0)) +
      coord_flip() +
      facet_wrap(~ songbook.name, scales = "free") +
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
    songbook.arrangement.type.df = song.instances.songbooks.df %>%
      inner_join(song.instances.arrangement.types.df,
                 by = c("song.instance.id", "song.id")) %>%
      inner_join(songs.df, by = "song.id") %>%
      inner_join(arrangement.types.df, by = "arrangement.type.id") %>%
      dplyr::select(song.id, song.name, arrangement.type.id, arrangement.type,
                    songbook.id, songbook.name) %>%
      filter(songbook.name %in% input$songbooksToAnalyze) %>%
      distinct()
    songbook.arrangement.type.sum.df = songbook.arrangement.type.df %>%
      group_by(songbook.name, arrangement.type) %>%
      summarise(num.songs = n_distinct(song.name)) %>%
      ungroup %>%
      complete(songbook.name, arrangement.type, fill = list(num.songs = 0)) %>%
      inner_join(song.instances.songbooks.df %>%
                   group_by(songbook.name) %>%
                   summarise(songbook.total = n_distinct(song.id)),
                 by = "songbook.name") %>%
      mutate(prop.songs = num.songs / songbook.total)
    songbook.arrangement.type.sum.df = songbook.arrangement.type.sum.df %>%
      inner_join(songbook.arrangement.type.sum.df %>%
                   group_by(arrangement.type) %>%
                   summarise(mean.prop = mean(prop.songs)),
                 by = "arrangement.type") %>%
      mutate(diff = prop.songs - mean.prop)
    if(input$songbookArrangementTypeOptions == "Number of songs") {
      songbook.arrangement.type.sum.df = songbook.arrangement.type.sum.df %>%
        arrange(songbook.name, num.songs)
    }
    else if(input$songbookArrangementTypeOptions == "Arrangement type") {
      songbook.arrangement.type.sum.df = songbook.arrangement.type.sum.df %>%
        arrange(songbook.name, desc(arrangement.type))
    }
    else if(input$songbookArrangementTypeOptions == "Relative number of songs") {
      songbook.arrangement.type.sum.df = songbook.arrangement.type.sum.df %>%
        arrange(songbook.name, diff)
    }
    songbook.arrangement.type.sum.df = songbook.arrangement.type.sum.df %>%
      mutate(order = row_number())
    ggplot(songbook.arrangement.type.sum.df,
           aes(x = order, y = num.songs, fill = diff)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient2(name = "",
                           midpoint = 0) +
      scale_x_continuous(breaks = songbook.arrangement.type.sum.df$order,
                         labels = songbook.arrangement.type.sum.df$arrangement.type,
                         expand = c(0, 0)) +
      coord_flip() +
      facet_wrap(~ songbook.name, scales = "free") +
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