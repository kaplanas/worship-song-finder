#### Populate song search outputs ####

# Populate the results list
get.song.list.results = reactive({
  # Start with a list of all songs
  results.df = songs.df %>%
    select(song.id, song.name)
  # Filter the songs
  source("R_files/filter_results.R", local = T)
  # Order the results
  results.df = inner_join(results.df, songs.df,
                          by = c("song.id", "song.name")) %>%
    arrange(song.name.sort)
  # Return the results
  results.df
})
output$songList = renderUI({
  
  # Create the list of results
  results.df = get.song.list.results()
  if(nrow(results.df) > 0 &
     (input$songTitle != "" |
      input$artistName != "" |
      length(input$topicChoices) > 0 |
      input$scriptureBook != 0 |
      length(input$songbookChoices) > 0 |
      length(input$arrangementChoices) > 0 |
      length(input$languageChoices) > 0 |
      length(input$keyChoices) > 0 |
      length(input$timeChoices) > 0 |
      length(input$meterChoices) > 0 |
      input$tuneName != "") |
     length(input$requestChoices) > 0) {
    song.panels =
      lapply(1:nrow(results.df),
             function(i) {
               return(tabPanel(title = all.song.panel.titles[[as.character(results.df$song.id[i])]],
                               all.song.panels[[as.character(results.df$song.id[i])]]))
             })
    song.panels[["widths"]] = c(5, 7)
    song.panels[["well"]] = F
    song.panels[["id"]] = "songResultsPanel"
    do.call(navlistPanel, song.panels)
  }
})

#### Populate worship history outputs ####

if(version == "ctcc") {

  # Populate the list of recent songs
  output$recentSongList <- renderTable({
    recent.songs.df = worship.history.df %>%
      inner_join(song.instances.df, by = "song.instance.id") %>%
      filter(worship.history.date >= input$recentSongsDateRange[1],
             worship.history.date <= input$recentSongsDateRange[2]) %>%
      mutate(Date = format(worship.history.date, date.output.format),
             Song = song.instance) %>%
      arrange(desc(worship.history.date), worship.history.id) %>%
      dplyr::select(Date, Song)
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
      filter(worship.history.date >= input$songYearDateRange[1],
             worship.history.date <= input$songYearDateRange[2]) %>%
      dplyr::select(song.id, topic.name, worship.history.date) %>%
      distinct()
    g = ggplot(frequent.topics.df,
               aes(x = reorder(topic.name, table(topic.name)[topic.name]))) +
      geom_bar() +
      coord_flip() +
      xlab("Topic") +
      ylab("Number of songs") +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            axis.ticks.x = element_blank())
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
      geom_histogram(breaks = seq(1700, ceiling(max(song.years.df$year / 10)) * 10, 10)) +
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
            axis.text = element_text(size = 14),
            strip.text = element_text(size = 16))
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
    scale_x_continuous("Year written", breaks = seq(1000, 3000, 100)) +
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

# Function that creates a line graph of X over time
line.plot = function(data, grouping.col, input.vals) {
  if(length(input.vals) > 0) {
    print(input.vals)
    groups.by.decade.df = data
    groups.by.decade.df$gc = groups.by.decade.df[,grouping.col]
    groups.by.decade.df %>%
      group_by(decade) %>%
      mutate(decade.total = n_distinct(song.id)) %>%
      ungroup() %>%
      filter(gc %in% input.vals) %>%
      group_by(gc, decade, decade.total) %>%
      summarise(num.songs = n_distinct(song.id)) %>%
      ungroup() %>%
      complete(gc, nesting(decade, decade.total),
               fill = list(num.songs = 0)) %>%
      mutate(prop = num.songs / decade.total,
             lower = binom.confint(num.songs, decade.total, 0.95, "exact")$lower,
             upper = binom.confint(num.songs, decade.total, 0.95, "exact")$upper) %>%
      ggplot(aes(x = decade, y = prop)) +
      geom_line() +
      geom_ribbon(aes(ymin = lower, ymax = upper),
                  alpha = 0.3, color = NA) +
      facet_wrap(~ gc) +
      scale_x_continuous("Decade", breaks = seq(1800, 3000, 50),
                         limits = c(1800, NA)) +
      scale_y_continuous("Percent of songs", limits = c(0, 1),
                         labels = scales::percent_format()) +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 16))
  }
}

# Create the line graph of topic frequency over time
output$topicsOverTime <- renderPlot({
  line.plot(data = song.info.df %>%
              left_join(songs.topics.df, by = "song.id") %>%
              left_join(topics.df, by = "topic.id"),
            grouping.col = "topic.name",
            input.vals = input$topicsOverTimeTopics)
})

# Create the line graph of references to books of the Bible over time
output$scriptureReferencesOverTime <- renderPlot({
  line.plot(data = song.info.df %>%
              left_join(song.instances.scripture.references.df,
                        by = "song.id") %>%
              left_join(scripture.references.df,
                        by = "scripture.reference.id"),
            grouping.col = "book.name",
            input.vals = input$scriptureReferencesOverTimeBooks)
})

# Create the line graph of artist gender over time
output$genderOverTime <- renderPlot({
  if(length(input$genderOverTimeRoles) > 0) {
    gender.time.df = song.info.df %>%
      filter(!is.na(decade)) %>%
      group_by(decade) %>%
      mutate(decade.total = n_distinct(song.id)) %>%
      ungroup() %>%
      inner_join(song.instances.artists.df, by = "song.id") %>%
      filter((role == "lyricist" & "Lyrics" %in% input$genderOverTimeRoles) |
               (role == "composer" & "Music" %in% input$genderOverTimeRoles) |
               (role == "arranger" & "Arrangement" %in% input$genderOverTimeRoles)) %>%
      inner_join(artists.df, by = "artist.id") %>%
      filter(gender != "NA") %>%
      group_by(decade, decade.total, gender) %>%
      summarise(num.songs = n_distinct(song.id)) %>%
      ungroup() %>%
      complete(gender, nesting(decade, decade.total),
               fill = list(num.songs = 0)) %>%
      mutate(prop = num.songs / decade.total,
             lower = binom.confint(num.songs, decade.total, 0.95, "exact")$lower,
             upper = binom.confint(num.songs, decade.total, 0.95, "exact")$upper)
    gender.time.df$gender = factor(gender.time.df$gender,
                                        levels = c("Male", "Female"))
    ggplot(gender.time.df, aes(x = decade, y = prop, col = gender)) +
      geom_line() +
      geom_ribbon(aes(x = decade, ymin = lower, ymax = upper,
                      fill = gender), alpha = 0.3, color = NA) +
      scale_x_continuous("Decade", breaks = seq(1800, 3000, 50),
                         limits = c(1800, NA)) +
      scale_y_continuous("Percent of songs",
                         labels = scales::percent_format()) +
      scale_color_discrete("Artist gender") +
      scale_fill_discrete("Artist gender") +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 12))
  }
})

# Create the graph of songbook overlap
output$songbookOverlap <- renderVisNetwork({
  visNetwork(songbook.overlap.nodes.df, songbook.overlap.edges.df) %>%
    visNodes(shape = "box", font = list(size = 18)) %>%
    visEdges(color = list(color = "rgba(0, 0, 0, 0.1)", highlight = "black")) %>%
    visIgraphLayout(layout = "layout_with_fr",
                    weights = songbook.overlap.edges.df$weight) %>%
    visLegend(addNodes = lapply(1:(length(songbook.group.colors)-1),
                                function(x) {
                                  list(label = names(songbook.group.colors)[x],
                                       color = unname(songbook.group.colors[x]),
                                       shape = "box") }),
      useGroups = F, stepY = 50) %>%
    visEvents(select = "function(data) {
              Shiny.onInputChange('current_nodes_selection', data.nodes);
              Shiny.onInputChange('current_edges_selection', data.edges);
              ;}")
})

# Update the edge colors depending on which node is highlighted
observe({
  if(isTruthy(input$current_nodes_selection)) {
    selected.id = as.numeric(input$current_nodes_selection)
    temp.edges.df = songbook.overlap.edges.df %>%
      mutate(other.id = case_when(from == selected.id ~ to,
                                  to == selected.id ~ from)) %>%
      left_join(songbook.overlap.nodes.df %>%
                  dplyr::select(id, color.highlight = color),
                by = c("other.id" = "id"))
    visNetworkProxy("songbookOverlap") %>%
      visUpdateEdges(temp.edges.df)
  } else {
    visNetworkProxy("songbookOverlap") %>%
      visUpdateEdges(songbook.overlap.edges.df %>%
                       mutate(color.highlight = "black"))
  }
})

# Create the table of songs selected in the overlap graph
output$songbookSongsInCommon = renderDT({
  if(isTruthy(input$current_nodes_selection)) {
    id = as.numeric(input$current_nodes_selection)
    songbook.name = songbooks.df$songbook.name[songbooks.df$songbook.id == id]
    songs.df %>%
      dplyr::select(song.id, Song = song.name) %>%
      inner_join(song.instances.songbooks.df %>%
                   group_by(song.id) %>%
                   filter(n_distinct(songbook.id) == 1) %>%
                   ungroup() %>%
                   filter(songbook.id == id) %>%
                   mutate(!!songbook.name := entry.string.no.name) %>%
                   dplyr::select(song.id, !!songbook.name),
                 by = "song.id") %>%
      dplyr::select(-song.id) %>%
      arrange(Song)
  }
  else if(isTruthy(input$current_edges_selection)) {
    ids.1 = as.numeric(gsub("^([0-9]+)-[0-9]+$", "\\1",
                            input$current_edges_selection, perl = T))
    ids.2 = as.numeric(gsub("^[0-9]+-([0-9]+)$", "\\1",
                            input$current_edges_selection, perl = T))
    temp.df = songs.df %>%
      dplyr::select(song.id, Song = song.name)
    for(id in unique(c(ids.1, ids.2))) {
      songbook.name = songbooks.df$songbook.name[songbooks.df$songbook.id == id]
      temp.df = temp.df %>%
        inner_join(song.instances.songbooks.df %>%
                     filter(songbook.id == id) %>%
                     group_by(song.id) %>%
                     summarise(!!songbook.name := paste0(entry.string.no.name,
                                                         collapse = ", ")) %>%
                     ungroup(),
                   by = "song.id")
    }
    temp.df %>%
      dplyr::select(-song.id) %>%
      arrange(Song)
  }
}, rownames = F, options = list(pageLength = 50))

#### Populate psalm song outputs ####

# Populate the results list
get.psalm.song.list.results = reactive({
  # Start with a list of all psalm songs
  psalm.song.results.df = psalm.songs.df %>%
    select(psalm.song.id, psalm.number, song.id, psalm.song.type.id,
           psalm.song.title)
  # Filter the songs
  psalm.song.results.df = psalm.song.results.df %>%
    filter(psalm.number == input$psalmNumber)
  if(length(input$psalmSongType) > 0) {
    psalm.song.results.df = psalm.song.results.df %>%
      filter(coalesce(psalm.song.type.id, as.integer(1)) %in% input$psalmSongType)
  }
  # Order the results
  psalm.song.results.df = psalm.song.results.df %>%
    arrange(psalm.song.title)
  # Return the results
  psalm.song.results.df
})
output$psalmSongList = renderUI({
  # Create the list of results
  psalm.song.results.df = get.psalm.song.list.results()
  psalm.song.panels =
    lapply(1:nrow(psalm.song.results.df),
           function(i) {
             return(tabPanel(title = all.psalm.song.panel.titles[[as.character(psalm.song.results.df$psalm.song.id[i])]],
                             all.psalm.song.panels[[as.character(psalm.song.results.df$psalm.song.id[i])]]))
           })
  psalm.song.panels[["widths"]] = c(5, 7)
  psalm.song.panels[["well"]] = F
  psalm.song.panels[["id"]] = "psalmSongResultsPanel"
  temp.panels = do.call(navlistPanel, psalm.song.panels)
  # Assign classes based on the psalm song type (so css can assign colors)
  for(i in 1:nrow(psalm.song.results.df)) {
    current.class = NULL
    if("class" %in% names(temp.panels$children[[1]]$children[[1]]$children[[i]]$attribs)) {
      current.class = temp.panels$children[[1]]$children[[1]]$children[[i]]$attribs$class
    }
    new.class = paste("psalmsongtype",
                      coalesce(psalm.song.results.df$psalm.song.type.id[i],
                               as.integer(0)),
                      sep = "")
    if(is.null(current.class)) {
      current.class = new.class
    } else {
      current.class = paste(current.class, new.class, sep = " ")
    }
    temp.panels$children[[1]]$children[[1]]$children[[i]]$attribs$class = current.class
  }
  temp.panels
})
