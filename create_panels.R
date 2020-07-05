####################################
# Get lists for populating filters #
####################################

# A string for indicating that nothing is selected
none.selected.string = "[None selected]"

# Topics
topicList = topics.tab %>%
  arrange(TopicName)

# Books of the Bible
booksofthebible.tab = arrange(booksofthebible.tab, BookID)
bookList = c(0, booksofthebible.tab$BookID[is.element(booksofthebible.tab$BookID, scripturereferences.tab$BookID)])
names(bookList) = c(none.selected.string, booksofthebible.tab$BookName[is.element(booksofthebible.tab$BookID, scripturereferences.tab$BookID)])
bookList = as.list(bookList)

# Songbooks
songbookList = songbooks.tab %>%
  arrange(SongbookName) %>%
  select(SongbookID, SongbookName)

# Arrangement types
arrangementList = arrangementtypes.tab %>%
  arrange(ArrangementType)

# Languages
languageList = languages.tab %>%
  arrange(LanguageName)

# Key signatures
keySignatureList = inner_join(keysignatures.tab, pitches.tab) %>%
  inner_join(accidentals.tab) %>%
  inner_join(modes.tab) %>%
  mutate(PrettyKeySignatureLabel = paste(PitchName,
                                         ifelse(AccidentalID == 3, "", AccidentalSymbol),
                                         ifelse(ModeID == 1, "", ifelse(ModeID == 2, "m", paste(" ", ModeName, sep = ""))), sep = "")) %>%
  arrange(PitchName, ifelse(AccidentalID == 3, 0, AccidentalID), ModeID) %>%
  select(KeySignatureID, PrettyKeySignatureLabel)

# Time signatures
timeSignatureList = timesignatures.tab %>%
  mutate(PrettyTimeSignatureLabel = paste(TimeSignatureBeat, "/", TimeSignatureMeasure, sep = "")) %>%
  arrange(TimeSignatureBeat, TimeSignatureMeasure) %>%
  select(TimeSignatureID, PrettyTimeSignatureLabel)

# Meters
meterList = inner_join(songinstances.tab, songinstances.lyrics.tab) %>%
  inner_join(lyrics.meters.tab) %>%
  inner_join(songinstances.tunes.tab) %>%
  inner_join(tunes.meters.tab, by = "TuneID") %>%
  select(SongID, MeterID.x, MeterID.y) %>%
  gather(MeterSource, MeterID, -SongID) %>%
  select(SongID, MeterID) %>%
  group_by(MeterID) %>%
  summarize(NumSongs = n()) %>%
  filter(NumSongs >= 3 & MeterID != 1) %>%
  inner_join(meters.tab) %>%
  mutate(MeterForSort = sapply(Meter, function(x) { paste(paste(str_pad(unlist(strsplit(gsub(" D$", "", x), "[. ]")), 2, side = "left", pad = "0"), collapse = ""), ifelse(endsWith(x, " D"), " D", ""), sep = "") })) %>%
  arrange(MeterForSort) %>%
  select(MeterID, Meter)

# Worship slots
worshipSlotList = worshipslots.tab %>%
  arrange(WorshipSlotOrder) %>%
  filter(!is.element(WorshipSlotID, c(4, 5))) %>%
  select(WorshipSlotID, WorshipSlot)

########################
# Create filter panels #
########################

# Song title filter
songTitleFilter = tabPanel("Song title",
                           div(id = "songTitleFilter",
                               radioButtons("songTitleOptions", NA,
                                            choices = c("Parts", "Whole words", "String")),
                               textInput("songTitle", NA))
                          )

# Artist name filter
artistNameFilter = tabPanel("Artist name",
                            radioButtons("artistNameOptions", NA,
                                         choices = c("Lyrics", "Music", "Arrangement", "Any")),
                            textInput("artistName", NA)
)

# Topic filter
topicFilter = tabPanel("Topic",
                       radioButtons("topicOptions", NA,
                                    choices = c("Match any", "Match all")),
                       checkboxGroupInput("topicChoices", NA,
                                          choiceNames = topicList$TopicName,
                                          choiceValues = topicList$TopicID)
                      )

# Scripture reference filter
scriptureReferenceFilter = tabPanel("Scripture reference",
                                    radioButtons("scriptureOptions", NA,
                                                 choices = c("Single verse", "Range")),
                                    selectInput("scriptureBook", "Book:",
                                                choices = bookList),
                                    selectInput("scriptureChapterStart", "Chapter:", c()),
                                    selectInput("scriptureVerseStart", "Verse:", c()),
                                    selectInput("scriptureChapterEnd", "Ending chapter:", c()),
                                    selectInput("scriptureVerseEnd", "Ending verse:", c())
                                   )

# Language filter
languageFilter = tabPanel("Language",
                          radioButtons("languageOptions", NA,
                                       choices = c("Match any", "Match all")),
                          checkboxGroupInput("languageChoices", NA,
                                             choiceNames = languageList$LanguageName,
                                             choiceValues = languageList$LanguageID)
                         )

# Songbook filter
songbookFilter= tabPanel("Songbook",
                         radioButtons("songbookOptions", NA,
                                      choices = c("Match any", "Match all")),
                         checkboxGroupInput("songbookChoices", NA,
                                            choiceNames = songbookList$SongbookName,
                                            choiceValues = songbookList$SongbookID)
                        )

# Arrangement type filter
arrangementTypeFilter = tabPanel("Arrangement type",
                                 radioButtons("arrangementOptions", NA,
                                              choices = c("Include", "Exclude")),
                                 checkboxGroupInput("arrangementChoices", NA,
                                                    choiceNames = arrangementList$ArrangementType,
                                                    choiceValues = arrangementList$ArrangementTypeID)
                                )

# Key signature filter
keySignatureFilter = tabPanel("Key signature",
                              radioButtons("keyOptions", NA,
                                           choices = c("Match any", "Match all")),
                              checkboxGroupInput("keyChoices", NA,
                                                 choiceNames = keySignatureList$PrettyKeySignatureLabel,
                                                 choiceValues = keySignatureList$KeySignatureID)
                             )

# Time signature filter
timeSignatureFilter = tabPanel("Time signature",
                               radioButtons("timeOptions", NA,
                                            choices = c("Match any", "Match all")),
                               checkboxGroupInput("timeChoices", NA,
                                                  choiceNames = timeSignatureList$PrettyTimeSignatureLabel,
                                                  choiceValues = timeSignatureList$TimeSignatureID)
                              )

# Meter filter
meterFilter = tabPanel("Meter",
                       radioButtons("meterOptions", NA,
                                    choices = c("Match any", "Match all")),
                       checkboxGroupInput("meterChoices", NA,
                                          choiceNames = meterList$Meter,
                                          choiceValues = meterList$MeterID)
                      )

# Recency filter
recencyFilter = tabPanel("Not sung recently",
                         checkboxInput("filterByDate",
                                       "Omit songs sung since..."),
                         dateInput("notSungSinceDate", NA,
                                   format = dateInputFormat,
                                   min = dateInputMin)
                        )

# Request filter
requestFilter = tabPanel("Survey and requests",
                         checkboxGroupInput("requestChoices",
                                            "Filter to songs that...",
                                            choiceNames = c("were on the survey", "have been specially requested"),
                                            choiceValues = c("survey", "request")))

#############################
# Create other input panels #
#############################

# Recent songs panel
recentSongsPanel = tabPanel("Recent songs",
                            dateRangeInput("recentSongsDateRange",
                                           "Date range:",
                                           separator = dateInputSep,
                                           format = dateInputFormat,
                                           min = dateInputMin,
                                           start = dateInputStart),
                            tableOutput("recentSongList")
                           )

# Frequent songs panel
frequentSongsPanel = tabPanel("Frequent songs",
                              fluidRow(column(5,
                                              dateRangeInput("frequentSongsDateRange",
                                                             "Date range:",
                                                             separator = dateInputSep,
                                                             format = dateInputFormat,
                                                             min = dateInputMin,
                                                             start = dateInputStart)),
                                       column(5,
                                              numericInput("frequentSongsCutoff",
                                                           "Songs sung at least this many times:",
                                                           value = 2,
                                                           min = 1))),
                              tableOutput("frequentSongList")
                             )

# Frequent topics panel
frequentTopicsPanel = tabPanel("Frequent topics",
                               fluidRow(column(5,
                                               dateRangeInput("recentTopicsDateRange",
                                                              "Date range:",
                                                              separator = dateInputSep,
                                                              format = dateInputFormat,
                                                              min = dateInputMin,
                                                              start = dateInputStart)),
                                        column(5,
                                               pickerInput("recentTopicSlots",
                                                           "Separate by position in service:",
                                                           multiple = T,
                                                           options = list(`actions-box` = TRUE),
                                                           choices = worshipSlotList$WorshipSlot))),
                               plotOutput("frequentTopicPlot",
                                          height = "1000px")
                              )

# Songs by year panel
songsByYearPanel = tabPanel("Songs by year",
                            dateRangeInput("songYearDateRange",
                                           "Date range:",
                                           separator = dateInputSep,
                                           format = dateInputFormat,
                                           min = dateInputMin,
                                           start = dateInputStart),
                            plotOutput("songsByYear")
                           )

# Songbook comparison panel
songbookComparisonPanel = tabPanel("Songbook comparison",
                                   pickerInput("songbooksToAnalyze",
                                               "Select one or more songbooks:",
                                               multiple = T,
                                               options = list(`actions-box` = TRUE),
                                               width = "100%",
                                               choices = songbookList$SongbookName),
                                   tabsetPanel(
                                     tabPanel("Song year",
                                              radioButtons("songbookYearOptions",
                                                           "Plot type:",
                                                           inline = T,
                                                           choices = c("Histogram (raw counts)", "Density (smoothed proportions)")),
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
                                     )
                                   )
                                  )

# Change over time panel
changeOverTimePanel = tabPanel("Change over time",
                               tabsetPanel(
                                 tabPanel("Topics",
                                          pickerInput("topicsOverTimeTopics",
                                                      "Select one or more topics:",
                                                      multiple = T,
                                                      options = list(`actions-box` = TRUE),
                                                      width = "100%",
                                                      choices = topicList$TopicName),
                                          plotOutput("topicsOverTime",
                                                     height = "600px")),
                                 tabPanel("Scripture references",
                                          pickerInput("scriptureReferencesOverTimeBooks",
                                                      "Select one or more books of the Bible:",
                                                      multiple = T,
                                                      options = list(`actions-box` = TRUE),
                                                      width = "100%",
                                                      choices = names(bookList)[names(bookList) != none.selected.string]),
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

########################
# Create output panels #
########################

# Get titles for content panels for all songs
allSongPanelTitles = lapply(
  allSongInfo$SongID,
  function(songID) {
    mainTitle = allSongInfo$Title[allSongInfo$SongID == songID]
    otherTitles = sort(unique(allSongInstanceInfo$Title[allSongInstanceInfo$SongID == songID &
                                                          !startsWith(mainTitle, allSongInstanceInfo$Title)]))
    tags$span(mainTitle,
         lapply(otherTitles,
                function(otherTitle) {
                  list(br(), tags$i(otherTitle))
                }), display = "flex", justify = "space-between")
  }
)
names(allSongPanelTitles) = as.character(allSongInfo$SongID)

# Get content panels for all songs
allSongPanels = lapply(
  allSongInfo$SongID,
  function(songID) {
    songRow = allSongInfo[allSongInfo$SongID == songID,]
    # Song title
    songTitle = h1(songRow$Title)
    initialSongInfoToReturn = list(songTitle)
    # Song topics
    if(!is.na(songRow$Topics)) {
      songTopics = p(songRow$Topics)
      initialSongInfoToReturn[[length(initialSongInfoToReturn) + 1]] = songTopics
    }
    songInfoToReturn = c(initialSongInfoToReturn, lapply(
      allSongInstanceInfo$SongInstanceID[allSongInstanceInfo$SongID == songID],
      function(songInstanceID) {
        songInstanceRow = allSongInstanceInfo[allSongInstanceInfo$SongInstanceID == songInstanceID,]
        # Song instance title
        panelListToReturn = list(hr(), h3(songInstanceRow$Title))
        # Song instance songbook entries
        if(!is.na(songInstanceRow$SongbookEntries)) {
          songbookEntries = p(songInstanceRow$SongbookEntries)
          panelListToReturn[[length(panelListToReturn) + 1]] = songbookEntries
        }
        # Song instance arrangement types
        if(!is.na(songInstanceRow$ArrangementTypes)) {
          arrangementTypes = p(songInstanceRow$ArrangementTypes)
          panelListToReturn[[length(panelListToReturn) + 1]] = arrangementTypes
        }
        # Song instance key and time signatures
        if(!is.na(songInstanceRow$KeySignatures) | !is.na(songInstanceRow$TimeSignatures)) {
          keyTimeSignatures = p(paste(songInstanceRow$KeySignatures,
                                      ifelse(!is.na(songInstanceRow$KeySignatures) & !is.na(songInstanceRow$TimeSignatures), "; ", ""),
                                      songInstanceRow$TimeSignatures, sep = ""))
          panelListToReturn[[length(panelListToReturn) + 1]] = keyTimeSignatures
        }
        # Song instance scripture references
        if(!is.na(songInstanceRow$ScriptureReferences)) {
          scriptureReferences = p(songInstanceRow$ScriptureReferences)
          panelListToReturn[[length(panelListToReturn) + 1]] = scriptureReferences
        }
        # Song instance lyricists and composers
        if(!is.na(songInstanceRow$Lyricists) & !is.na(songInstanceRow$Composers) & songInstanceRow$Lyricists == songInstanceRow$Composers) {
          panelListToReturn[[length(panelListToReturn) + 1]] = p(paste("Lyrics & Music:", songInstanceRow$Lyricists))
        }
        else {
          if(!is.na(songInstanceRow$Lyricists)) {
            panelListToReturn[[length(panelListToReturn) + 1]] = p(paste("Lyrics:", songInstanceRow$Lyricists))
          }
          if(!is.na(songInstanceRow$Composers)) {
            panelListToReturn[[length(panelListToReturn) + 1]] = p(paste("Music:", songInstanceRow$Composers))
          }
        }
        # Song instance arrangers
        if(!is.na(songInstanceRow$Arrangers)) {
          panelListToReturn[[length(panelListToReturn) + 1]] = p(paste("Arrangement:", songInstanceRow$Arrangers))
        }
        # Song instance copyright info
        if(!is.na(songInstanceRow$LyricsCopyright) & !is.na(songInstanceRow$TuneCopyright) & songInstanceRow$LyricsCopyright == songInstanceRow$TuneCopyright) {
          panelListToReturn[[length(panelListToReturn) + 1]] = p(songInstanceRow$LyricsCopyright)
        }
        else {
          if(!is.na(songInstanceRow$LyricsCopyright)) {
            panelListToReturn[[length(panelListToReturn) + 1]] = p(paste("Lyrics", songInstanceRow$LyricsCopyright))
          }
          if(!is.na(songInstanceRow$TuneCopyright)) {
            panelListToReturn[[length(panelListToReturn) + 1]] = p(paste("Music", songInstanceRow$TuneCopyright))
          }
        }
        if(!is.na(songInstanceRow$ArrangementCopyright)) {
          panelListToReturn[[length(panelListToReturn) + 1]] = p(paste("Arrangement", songInstanceRow$ArrangementCopyright))
        }
        # Song instances lyrics first lines
        songInstanceLyrics = allSongInstanceLyrics$LyricsLine[allSongInstanceLyrics$SongInstanceID == songInstanceID &
                                                              !is.na(allSongInstanceLyrics$LyricsLine)]
        # if(length(songInstanceLyrics) > 0 & nrow(inner_join(songinstances.lyrics.tab, lyrics.copyrightholders.tab) %>% filter(SongInstanceID == songInstanceID & CopyrightHolderID == 1)) > 0) {
          # panelListToReturn[[length(panelListToReturn) + 1]] = tags$a("Show full lyrics", id = "showFullLyrics", href = "#")
          # panelListToReturn[[length(panelListToReturn) + 1]] = br()
        # }
        panelListToReturn = list(panelListToReturn,
                                 span(lapply(songInstanceLyrics,
                                             function(lyricsLine) {
                                               list(tags$i(lyricsLine), br())
                                             })))
        # Return new content panel
        return(panelListToReturn)
      }
    ))
    return(songInfoToReturn)
  }
)
names(allSongPanels) = as.character(allSongInfo$SongID)