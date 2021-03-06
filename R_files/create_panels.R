#### Get lists for populating filters ####

# A string for indicating that nothing is selected
none.selected.string = "[None selected]"

# Topics
topic.list = topics.df %>%
  arrange(topic.name)

# Books of the Bible
book.list = c(0,
              scripture.references.df %>%
                dplyr::select(book.id) %>%
                distinct() %>%
                inner_join(bible.books.df, by = c("book.id")) %>%
                pull(book.id))
names(book.list) = c(none.selected.string,
                     bible.books.df$book.name[book.list[-1]])
book.list = as.list(book.list)

# Songbooks
songbook.list = songbooks.df %>%
  arrange(songbook.name) %>%
  select(songbook.id, songbook.name)

# Arrangement types
arrangement.list = arrangement.types.df %>%
  arrange(arrangement.type)

# Languages
language.list = languages.df %>%
  arrange(language.name)

# Key signatures
key.signature.list = key.signatures.df %>%
  arrange(pitch.name, ifelse(accidental.id == 3, 0, accidental.id), mode.id) %>%
  dplyr::select(key.signature.id, key.signature.string)

# Time signatures
time.signature.list = time.signatures.df %>%
  arrange(time.signature.measure, time.signature.beat) %>%
  dplyr::select(time.signature.id, time.signature.string)

# Meters
meter.list = song.instances.meters.df %>%
  dplyr::select(meter.id, song.id) %>%
  group_by(meter.id) %>%
  summarise(num.songs = n()) %>%
  filter(num.songs >= 5,
         meter.id != 1) %>%
  inner_join(meters.df, by = c("meter.id")) %>%
  mutate(meter.for.sort = sapply(meter.string,
                                 function(x) {
                                   paste(paste(str_pad(unlist(strsplit(gsub(" D$",
                                                                            "",
                                                                            x),
                                                                       "[. ]")),
                                                       2,
                                                       side = "left",
                                                       pad = "0"),
                                               collapse = ""),
                                         ifelse(endsWith(x, " D"), " D", ""),
                                         sep = "") })) %>%
  arrange(meter.for.sort) %>%
  select(meter.id, meter = meter.string)

# Psalm numbers
psalm.number.list = 1:150

# Psalm song types
psalm.song.type.list = psalm.song.types.df %>%
  dplyr::select(psalm.song.type, psalm.song.type.id) %>%
  arrange(psalm.song.type) %>%
  deframe()

#### Create filter panels ####

# Song title filter
song.title.filter = tabPanel("Song title",
                             div(id = "songTitleFilter",
                                 radioButtons("songTitleOptions", NULL,
                                              choices = c("Parts", "Whole words", "String")),
                                 textInput("songTitle", NULL))
                             )

# Artist name filter
artist.name.filter = tabPanel("Artist name",
                              radioButtons("artistNameOptions", NULL,
                                           choices = c("Lyrics", "Music", "Arrangement", "Any")),
                              textInput("artistName", NULL)
                              )

# Topic filter
topic.filter = tabPanel("Topic",
                        radioButtons("topicOptions", NULL,
                                     choices = c("Match any", "Match all")),
                        checkboxGroupInput("topicChoices", NULL,
                                           choiceNames = topic.list$topic.name,
                                           choiceValues = topic.list$topic.id)
                        )

# Scripture reference filter
scripture.reference.filter = tabPanel("Scripture reference",
                                      radioButtons("scriptureOptions", NULL,
                                                   choices = c("Single verse", "Range")),
                                      selectInput("scriptureBook", "Book:",
                                                  choices = book.list),
                                      selectInput("scriptureChapterStart", "Chapter:", c()),
                                      selectInput("scriptureVerseStart", "Verse:", c()),
                                      selectInput("scriptureChapterEnd", "Ending chapter:", c()),
                                      selectInput("scriptureVerseEnd", "Ending verse:", c())
                                      )

# Language filter
language.filter = tabPanel("Language",
                           radioButtons("languageOptions", NULL,
                                        choices = c("Match any", "Match all")),
                           checkboxGroupInput("languageChoices", NULL,
                                              choiceNames = language.list$language.name,
                                              choiceValues = language.list$language.id)
                           )

# Songbook filter
songbook.filter= tabPanel("Songbook",
                          radioButtons("songbookOptions", NULL,
                                       choices = c("Match any", "Match all")),
                          checkboxGroupInput("songbookChoices", NULL,
                                             choiceNames = songbook.list$songbook.name,
                                             choiceValues = songbook.list$songbook.id)
                          )

# Arrangement type filter
arrangement.type.filter = tabPanel("Arrangement type",
                                   radioButtons("arrangementOptions", NULL,
                                                choices = c("Include", "Exclude")),
                                   checkboxGroupInput("arrangementChoices", NULL,
                                                      choiceNames = arrangement.list$arrangement.type,
                                                      choiceValues = arrangement.list$arrangement.type.id)
                                   )

# Key signature filter
key.signature.filter = tabPanel("Key signature",
                                radioButtons("keyOptions", NULL,
                                             choices = c("Match any", "Match all")),
                                checkboxGroupInput("keyChoices", NULL,
                                                   choiceNames = key.signature.list$key.signature.string,
                                                   choiceValues = key.signature.list$key.signature.id)
                                )

# Time signature filter
time.signature.filter = tabPanel("Time signature",
                                 radioButtons("timeOptions", NULL,
                                              choices = c("Match any", "Match all")),
                                 checkboxGroupInput("timeChoices", NULL,
                                                    choiceNames = time.signature.list$time.signature.string,
                                                    choiceValues = time.signature.list$time.signature.id)
                                 )

# Meter filter
meter.filter = tabPanel("Meter",
                        radioButtons("meterOptions", NULL,
                                     choices = c("Match any", "Match all")),
                        checkboxGroupInput("meterChoices", NULL,
                                           choiceNames = meter.list$meter,
                                           choiceValues = meter.list$meter.id)
)

# Tune name filter
tune.name.filter = tabPanel("Tune name",
                            div(id = "tuneNameFilter",
                                radioButtons("tuneNameOptions", NULL,
                                             choices = c("Parts", "Whole words", "String")),
                                textInput("tuneName", NULL))
)

# Psalm number filter
psalm.number.filter = selectInput("psalmNumber",
                                  "Psalm number:",
                                  1:150)

# Psalm song type filter
psalm.song.type.filter = pickerInput("psalmSongType",
                                     "Select one or more song types:",
                                     multiple = T,
                                     options = list(`actions-box` = TRUE),
                                     width = "100%",
                                     choices = psalm.song.type.list)

#### Create other input panels ####

if(version == "ctcc") {

  # Recent songs panel
  recent.songs.panel = tabPanel("Recent songs",
                                dateRangeInput("recentSongsDateRange",
                                               "Date range:",
                                               separator = date.input.sep,
                                               format = date.input.format,
                                               min = date.input.min,
                                               start = date.input.start),
                                tableOutput("recentSongList")
                                )
  
  # Frequent songs panel
  frequent.songs.panel = tabPanel("Frequent songs",
                                  fluidRow(column(5,
                                                  dateRangeInput("frequentSongsDateRange",
                                                                 "Date range:",
                                                                 separator = date.input.sep,
                                                                 format = date.input.format,
                                                                 min = date.input.min,
                                                                 start = date.input.start)),
                                           column(5,
                                                  numericInput("frequentSongsCutoff",
                                                               "Songs sung at least this many times:",
                                                               value = 2,
                                                               min = 1))),
                                  tableOutput("frequentSongList")
                                  )
  
  # Frequent topics panel
  frequent.topics.panel = tabPanel("Frequent topics",
                                   fluidRow(column(5,
                                                   dateRangeInput("recentTopicsDateRange",
                                                                  "Date range:",
                                                                  separator = date.input.sep,
                                                                  format = date.input.format,
                                                                  min = date.input.min,
                                                                  start = date.input.start))),
                                   plotOutput("frequentTopicPlot",
                                              height = "1000px")
                                   )
  
  # Songs by year panel
  songs.by.year.panel = tabPanel("Songs by year",
                                 dateRangeInput("songYearDateRange",
                                                "Date range:",
                                                separator = date.input.sep,
                                                format = date.input.format,
                                                min = date.input.min,
                                                start = date.input.start),
                                 plotOutput("songsByYear")
                                 )

}

# Songbook comparison panel
songbook.comparison.panel = tabPanel("Songbook comparison",
                                     pickerInput("songbooksToAnalyze",
                                                 "Select one or more songbooks:",
                                                 multiple = T,
                                                 options = list(`actions-box` = TRUE),
                                                 width = "100%",
                                                 choices = songbook.list$songbook.name),
                                     tabsetPanel(
                                       tabPanel("Song year",
                                                plotOutput("yearBySongbook")),
                                       tabPanel("Topic",
                                                radioButtons("songbookTopicOptions",
                                                             "Sort by:",
                                                             inline = T,
                                                             choices = c("Number of songs", "Topic", "Relative number of songs")),
                                                helpText(tags$p("Color indicates whether the songbook has proportionally ", tags$span("more", style = "color: blue"), " or ", tags$span("fewer", style = "color: red"), " songs on that topic, ", tags$i("relative to the other songbooks you selected."))),
                                                plotOutput("topicBySongbook",
                                                           height = "1000px")),
                                       tabPanel("Arrangement type",
                                                radioButtons("songbookArrangementTypeOptions",
                                                             "Sort by:",
                                                             inline = T,
                                                             choices = c("Number of songs", "Arrangement type", "Relative number of songs")),
                                                helpText(tags$p("Color indicates whether the songbook has proportionally ", tags$span("more", style = "color: blue"), " or ", tags$span("fewer", style = "color: red"), " songs of that arrangement type, ", tags$i("relative to the other songbooks you selected."))),
                                                plotOutput("arrangementTypeBySongbook")
                                       ),
                                       tabPanel("Mode",
                                                radioButtons("songbookModeOptions",
                                                             "Sort by:",
                                                             inline = T,
                                                             choices = c("Number of songs", "Mode", "Relative number of songs")),
                                                helpText(tags$p("Color indicates whether the songbook has proportionally ", tags$span("more", style = "color: blue"), " or ", tags$span("fewer", style = "color: red"), " songs in that mode, ", tags$i("relative to the other songbooks you selected."))),
                                                plotOutput("modeBySongbook")
                                       ),
                                       tabPanel("Time signature",
                                                radioButtons("songbookTimeSignatureOptions",
                                                             "Sort by:",
                                                             inline = T,
                                                             choices = c("Number of songs", "Time signature", "Relative number of songs")),
                                                helpText(tags$p("Color indicates whether the songbook has proportionally ", tags$span("more", style = "color: blue"), " or ", tags$span("fewer", style = "color: red"), " songs in that time signature, ", tags$i("relative to the other songbooks you selected."))),
                                                plotOutput("timeSignatureBySongbook")
                                       )
                                     )
                                     )

# Songbook overlap panel
songbook.overlap.panel = tabPanel("Songbook overlap",
                                  helpText(tags$p(tags$b("Just for fun, a way to explore what different songbooks have in common."))),
                                  helpText(tags$p("Click on a songbook for a list of songs unique to that songbook, with the volume (if applicable) and number of each song.")),
                                  helpText(tags$p("Click on a link for a list of songs the two songbooks have in common.  Thicker links mean the songbooks share more songs.")),
                                  helpText(tags$p("Click and drag songbooks to rearrange the graph.")),
                                  visNetworkOutput("songbookOverlap",
                                                   width = "1100px",
                                                   height = "700px"),
                                  DTOutput("songbookSongsInCommon"))

# Change over time panel
change.over.time.panel = tabPanel("Change over time",
                                  tabsetPanel(
                                    tabPanel("Topics",
                                             pickerInput("topicsOverTimeTopics",
                                                         "Select one or more topics:",
                                                         multiple = T,
                                                         options = list(`actions-box` = TRUE),
                                                         width = "100%",
                                                         choices = topic.list$topic.name),
                                             plotOutput("topicsOverTime",
                                                        height = "600px")),
                                    tabPanel("Scripture references",
                                             pickerInput("scriptureReferencesOverTimeBooks",
                                                         "Select one or more books of the Bible:",
                                                         multiple = T,
                                                         options = list(`actions-box` = TRUE),
                                                         width = "100%",
                                                         choices = names(book.list)[names(book.list) != none.selected.string]),
                                             plotOutput("scriptureReferencesOverTime",
                                                        height = "600px")),
                                    tabPanel("Gender",
                                             pickerInput("genderOverTimeRoles",
                                                         "Select one or more artist roles:",
                                                         multiple = T,
                                                         options = list(`actions-box` = TRUE),
                                                         width = "100%",
                                                         choices = c("Lyrics", "Music", "Arrangement")),
                                             plotOutput("genderOverTime",
                                                        height = "600px"))
                                  )
                                  )

#### Create output panels ####

# Get titles for content panels for all songs
all.song.panel.titles = lapply(
  song.info.df$song.id,
  function(songID) {
    panel.name = song.info.df$panel.name[song.info.df$song.id == songID]
    tags$span(HTML(panel.name), display = "flex", justify = "space-between")
  }
)
names(all.song.panel.titles) = as.character(song.info.df$song.id)

# Get content panels for all songs
all.song.panels = lapply(
  song.info.df$song.id,
  function(songID) {
    song.row = song.info.df[song.info.df$song.id == songID,]
    # Song title
    song.title = h1(song.row$title)
    initial.song.info.to.return = list(song.title)
    # Song topics
    if(!is.na(song.row$topics)) {
      song.topics = p(song.row$topics)
      initial.song.info.to.return[[length(initial.song.info.to.return) + 1]] = song.topics
    }
    # Song instances
    song.info.to.return = c(initial.song.info.to.return, lapply(
      song.instance.info.df$song.instance.id[song.instance.info.df$song.id == songID],
      function(songInstanceID) {
        return(HTML(song.instances.df$html[song.instances.df$song.instance.id == songInstanceID]))
      }
    ))
    return(song.info.to.return)
  }
)
names(all.song.panels) = as.character(song.info.df$song.id)

# Get titles for content panels for all psalm songs
all.psalm.song.panel.titles = lapply(
  psalm.songs.df$psalm.song.id,
  function(psalmSongID) {
    panel.name = psalm.songs.df$panel.name[psalm.songs.df$psalm.song.id == psalmSongID]
    tags$span(HTML(panel.name), display = "flex", justify = "space-between")
  }
)
names(all.psalm.song.panel.titles) = psalm.songs.df$psalm.song.id

# Constant: colors for psalm song types
psalm.song.type.colors = brewer.pal(4, "Set1")[c(3, 2, 4, 1)]

# Get content panels for all psalm songs
all.psalm.song.panels = lapply(
  psalm.songs.df$psalm.song.id,
  function(psalmSongID) {
    psalm.song.row = psalm.songs.df[psalm.songs.df$psalm.song.id == psalmSongID,]
    current.song.id = psalm.song.row$song.id
    current.psalm.number = psalm.song.row$psalm.number
    # Main title
    main.title = h1(psalm.song.row$psalm.song.title)
    # Song info
    song.info.panel = psalm.song.row$html.info
    all.psalm.song.panels = list(tabPanel("Song info",
                                          HTML(song.info.panel)))
    # Lyrics
    current.lyrics.df = psalm.songs.lyrics.df %>%
      filter(psalm.song.id == psalmSongID)
    if(nrow(current.lyrics.df) > 0) {
      for(i in 1:nrow(current.lyrics.df)) {
        all.psalm.song.panels[[length(all.psalm.song.panels) + 1]] =
          tabPanel(current.lyrics.df$tab.name[i],
                   tags$p(),
                   HTML(current.lyrics.df$full.lyrics[i]))
      }
    }
    # Alternative tunes
    alternative.tunes.panel = psalm.song.row$html.alternatives
    if(isTruthy(alternative.tunes.panel)) {
      all.psalm.song.panels[[length(all.psalm.song.panels) + 1]] = 
        tabPanel("Alternative tunes", HTML(alternative.tunes.panel))
    }
    # Combine all info
    info.tabs = do.call(tabsetPanel, all.psalm.song.panels)
    psalm.song.info.to.return = list(main.title, info.tabs)
    return(psalm.song.info.to.return)
  }
)
names(all.psalm.song.panels) = psalm.songs.df$psalm.song.id