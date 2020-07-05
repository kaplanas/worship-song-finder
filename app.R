library(shiny)
library(shinyjs)
library(shinyWidgets)
library(RMySQL)
library(tidyr)
library(date)
library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(gridExtra)
library(binom)
# library(quanteda)
# library(readtext)

source("database_connection_local.R", local = T)
#source("database_connection.R", local = T)

# Connect to the database
wsf_con = dbConnect(MySQL(), user = db.user, password = db.password, dbname = db.name, host = db.host)
on.exit(dbDisconnect(wsf_con), add = TRUE)
dbGetQuery(wsf_con, "SET NAMES utf8")

# Load stuff from the database
source("load_from_database.R", local = T)

# Settings for date inputs and outputs
dateInputSep = " to "
dateInputFormat = "MM d, yyyy"
dateOutputFormat = "%B %e, %Y"
dateInputMin = "2017-03-26"
dateInputStart = seq(Sys.Date(), length = 7, by = "-1 month")[7]

# Create content panels
source("create_panels.R", local = T)

# Define UI
ui <- navbarPage(
  
  # Page title
  "Worship Song Finder",
  theme = "theme.css",
  
  # Search page
  tabPanel("Search for songs",
           
    useShinyjs(),
    
    sidebarLayout(
      
      # The filters
      sidebarPanel(
        navlistPanel(
          songTitleFilter,
          artistNameFilter,
          topicFilter,
          scriptureReferenceFilter,
          languageFilter,
          songbookFilter,
          arrangementTypeFilter,
          keySignatureFilter,
          timeSignatureFilter,
          meterFilter,
          recencyFilter,
          requestFilter,
          well = F,
          widths = c(6, 6)
        ),
        width = 4
      ),
      
      # The results
      mainPanel(
        uiOutput("songList"),
        width = 8
      )
      
    )
    
  ),
  
  # Worship history page
  tabPanel("Worship history",
    navlistPanel(
      recentSongsPanel,
      frequentSongsPanel,
      frequentTopicsPanel,
      songsByYearPanel
    )
  ),
  
  # Song analysis page
  tabPanel("Song analysis",
    navlistPanel(
      songbookComparisonPanel,
      changeOverTimePanel
    )
  ),
  
  # Help page
  tabPanel("Help",
    navlistPanel(
      tabPanel("What is this site for?", includeHTML("help/what_for.html")),
      tabPanel("Songs vs. instances", includeHTML("help/songs_instances.html")),
      tabPanel("The title and artist filters", includeHTML("help/title_artist_filters.html")),
      tabPanel("The language filter", includeHTML("help/language_filter.html")),
      tabPanel("The topic, arrangement, and songbook filters", includeHTML("help/topic_arrangement_songbook_filters.html")),
      tabPanel("The key and time signature filters", includeHTML("help/key_time_signature_filters.html")),
      tabPanel("The meter filter", includeHTML("help/meter_filter.html")),
      tabPanel("The scripture reference filter", includeHTML("help/scripture_reference_filter.html")),
      tabPanel("Same lyrics/tune/meter", includeHTML("help/same_lyrics_tune_meter.html")),
      tabPanel("Who should I contact with questions/comments/complaints/etc.?", includeHTML("help/contact.html"))
    )
  )
  
)

# Define server logic
server <- function(input, output, session) {
  
  # Get worship history
  wsf_con = dbConnect(MySQL(), user = db.user, password = db.password, dbname = db.name, host = db.host)
  on.exit(dbDisconnect(wsf_con), add = TRUE)
  dbGetQuery(wsf_con, "SET NAMES utf8")
  worshiphistory.tab = dbGetQuery(wsf_con, "SELECT WorshipHistoryID, SongInstanceID, WorshipHistoryDate, WorshipSlotID FROM worshiphistory")
  worshiphistory.tab$WorshipHistoryDate = as.Date(worshiphistory.tab$WorshipHistoryDate)
  worshiphistory.tab = filter(worshiphistory.tab, format(worshiphistory.tab$WorshipHistoryDate, "%Y") >= 2017)
  
  # Populate the scripture filter based on the selected options
  includeScriptureEnd = reactive({
    ifelse(input$scriptureOptions == "Range", T, F)
  })
  observe({
    if(includeScriptureEnd()) {
      updateSelectInput(session, "scriptureChapterStart", label = "Starting chapter:")
      updateSelectInput(session, "scriptureVerseStart", label = "Starting verse:")
      shinyjs::show(id = "scriptureChapterEnd")
      shinyjs::show(id = "scriptureVerseEnd")
    }
    else {
      shinyjs::hide(id = "scriptureChapterEnd")
      shinyjs::hide(id = "scriptureVerseEnd")
      updateSelectInput(session, "scriptureChapterStart", label = "Chapter:")
      updateSelectInput(session, "scriptureVerseStart", label = "Verse:")
    }
  })
  dynamicChapters = reactive({
    sort(unique(scripturereferences.tab$Chapter[scripturereferences.tab$BookID == input$scriptureBook]))
  })
  observe({
    updateSelectInput(session, "scriptureChapterStart", choices = dynamicChapters())
    updateSelectInput(session, "scriptureChapterEnd", choices = dynamicChapters())
  })
  dynamicVersesStart = reactive({
    sort(unique(scripturereferences.tab$Verse[scripturereferences.tab$BookID == input$scriptureBook &
                                              scripturereferences.tab$Chapter == input$scriptureChapterStart]))
  })
  observe({
    updateSelectInput(session, "scriptureVerseStart", choices = dynamicVersesStart())
  })
  dynamicVersesEnd = reactive({
    sort(unique(scripturereferences.tab$Verse[scripturereferences.tab$BookID == input$scriptureBook &
                                              scripturereferences.tab$Chapter == input$scriptureChapterEnd]))
  })
  observe({
    updateSelectInput(session, "scriptureVerseEnd", choices = dynamicVersesEnd())
  })
  
  # Populate the results list
  getSongListResults = reactive({
    # Start with a list of all songs
    songListResults = songs.tab %>%
      select(SongID, SongName)
    # Filter the songs
    source("filter_results.R", local = T)
    # Order the results
    songListResults = inner_join(songListResults, songs.tab) %>%
      arrange(SongNameForSort)
    # Return the results
    songListResults
  })
  output$songList = renderUI({
    
    # # Start with a list of all songs
    # songListResults = songs.tab %>%
    #   select(SongID, SongName)
    # 
    # # Filter the songs
    # source("filter_results.R", local = T)
    # 
    # # Order the results
    # songListResults = inner_join(songListResults, songs.tab) %>%
    #   arrange(SongNameForSort)
    
    # Create the list of results
    songListResults = getSongListResults()
    if(nrow(songListResults) > 0 &
       (input$songTitle != "" |
        input$artistName != "" |
        length(input$topicChoices) > 0 |
        input$scriptureBook != 0 |
        length(input$songbookChoices) > 0 |
        length(input$arrangementChoices) > 0 |
        length(input$languageChoices) > 0 |
        length(input$keyChoices) > 0 |
        length(input$timeChoices) > 0 |
        length(input$meterChoices) > 0) |
        length(input$requestChoices) > 0) {
      songPanels = lapply(1:nrow(songListResults),
                          function(i) {
                            return(tabPanel(title = allSongPanelTitles[[as.character(songListResults$SongID[i])]],
                                            allSongPanels[[as.character(songListResults$SongID[i])]]))
                          })
      songPanels[["widths"]] = c(5,7)
      songPanels[["well"]] = F
      songPanels[["id"]] = "songResultsPanel"
      do.call(navlistPanel, songPanels)
    }
  })
  
  # Populate all other outputs
  source("populate_outputs.R", local = T)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

