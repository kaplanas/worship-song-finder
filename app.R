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

#source("database_connection_local.R", local = T)
source("database_connection.R", local = T)

#### Useful initial settings ####

# Connect to the database
wsf.con = dbConnect(MySQL(), user = db.user, password = db.password,
                    dbname = db.name, host = db.host, port = 3306)
on.exit(dbDisconnect(wsf.con), add = T)
dbGetQuery(wsf.con, "SET NAMES utf8")

# Load stuff from the database
source("load_from_database.R", local = T)

# Settings for date inputs and outputs
date.input.sep = " to "
date.input.format = "MM d, yyyy"
date.output.format = "%B %e, %Y"
date.input.min = "2017-03-26"
date.input.start = seq(Sys.Date(), length = 7, by = "-1 month")[7]

# Create content panels
source("create_panels.R", local = T)

#### UI ####

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
          song.title.filter,
          artist.name.filter,
          topic.filter,
          scripture.reference.filter,
          language.filter,
          songbook.filter,
          arrangement.type.filter,
          key.signature.filter,
          time.signature.filter,
          meter.filter,
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
      recent.songs.panel,
      frequent.songs.panel,
      frequent.topics.panel,
      songs.by.year.panel
    )
  ),
  
  # Song analysis page
  tabPanel("Song analysis",
    navlistPanel(
      songbook.comparison.panel,
      change.over.time.panel
    )
  ),
  
  # Help page
  tabPanel("Help",
    navlistPanel(
      tabPanel("What is this site for?",
               includeHTML("help/what_for.html")),
      tabPanel("Songs vs. instances",
               includeHTML("help/songs_instances.html")),
      tabPanel("The title and artist filters",
               includeHTML("help/title_artist_filters.html")),
      tabPanel("The language filter",
               includeHTML("help/language_filter.html")),
      tabPanel("The topic, arrangement, and songbook filters",
               includeHTML("help/topic_arrangement_songbook_filters.html")),
      tabPanel("The key and time signature filters",
               includeHTML("help/key_time_signature_filters.html")),
      tabPanel("The meter filter",
               includeHTML("help/meter_filter.html")),
      tabPanel("The scripture reference filter",
               includeHTML("help/scripture_reference_filter.html")),
      tabPanel("Same lyrics/tune/meter",
               includeHTML("help/same_lyrics_tune_meter.html")),
      tabPanel("Who should I contact with questions/comments/complaints/etc.?",
               includeHTML("help/contact.html"))
    )
  )
  
)

#### Server ####

# Define server logic
server <- function(input, output, session) {
  
  # Get worship history
  wsf.con = dbConnect(MySQL(), user = db.user, password = db.password,
                      dbname = db.name, host = db.host)
  on.exit(dbDisconnect(wsf.con), add = TRUE)
  dbGetQuery(wsf.con, "SET NAMES utf8")
  worship.history.df = dbGetQuery(wsf.con,
                                  "SELECT WorshipHistoryID, SongInstanceID,
                                          WorshipHistoryDate, WorshipSlotID
                                   FROM worshiphistory") %>%
    dplyr::select(worship.history.id = WorshipHistoryID,
                  song.instance.id = SongInstanceID,
                  worship.history.date = WorshipHistoryDate,
                  worship.slot.id = WorshipSlotID)
  worship.history.df$worship.history.date = as.Date(worship.history.df$worship.history.date)
  worship.history.df = filter(worship.history.df,
                              format(worship.history.df$worship.history.date, "%Y") >= 2017)
  
  # Populate the scripture filter based on the selected options
  includeScriptureEnd = reactive({
    ifelse(input$scriptureOptions == "Range", T, F)
  })
  observe({
    if(includeScriptureEnd()) {
      updateSelectInput(session, "scriptureChapterStart",
                        label = "Starting chapter:")
      updateSelectInput(session, "scriptureVerseStart",
                        label = "Starting verse:")
      shinyjs::show(id = "scriptureChapterEnd")
      shinyjs::show(id = "scriptureVerseEnd")
    }
    else {
      shinyjs::hide(id = "scriptureChapterEnd")
      shinyjs::hide(id = "scriptureVerseEnd")
      updateSelectInput(session, "scriptureChapterStart",
                        label = "Chapter:")
      updateSelectInput(session, "scriptureVerseStart",
                        label = "Verse:")
    }
  })
  dynamicChapters = reactive({
    sort(unique(scripture.references.df$chapter[scripture.references.df$book.id == input$scriptureBook]))
  })
  observe({
    updateSelectInput(session, "scriptureChapterStart",
                      choices = dynamicChapters())
    updateSelectInput(session, "scriptureChapterEnd",
                      choices = dynamicChapters())
  })
  dynamicVersesStart = reactive({
    sort(unique(scripture.references.df$verse[scripture.references.df$book.id == input$scriptureBook &
                                                scripture.references.df$chapter == input$scriptureChapterStart]))
  })
  observe({
    updateSelectInput(session, "scriptureVerseStart",
                      choices = dynamicVersesStart())
  })
  dynamicVersesEnd = reactive({
    sort(unique(scripture.references.df$verse[scripture.references.df$book.id == input$scriptureBook &
                                              scripture.references.df$chapter == input$scriptureChapterEnd]))
  })
  observe({
    updateSelectInput(session, "scriptureVerseEnd",
                      choices = dynamicVersesEnd())
  })
  
  # Populate the results list
  get.song.list.results = reactive({
    # Start with a list of all songs
    results.df = songs.df %>%
      select(song.id, song.name)
    # Filter the songs
    source("filter_results.R", local = T)
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
        length(input$meterChoices) > 0) |
        length(input$requestChoices) > 0) {
      song.panels = lapply(1:nrow(results.df),
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
  
  # Populate all other outputs
  source("populate_outputs.R", local = T)
  
}

#### Run ####

# Run the application 
shinyApp(ui = ui, server = server)

