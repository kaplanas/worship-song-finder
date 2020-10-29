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
library(igraph)
library(visNetwork)
library(magrittr)
library(stringr)
library(DT)
library(RColorBrewer)
library(tibble)

# version = "ctcc"
version = "general"

source("database_connection_local.R", local = T)

#### Useful initial settings ####

# Connect to the database
wsf.shiny.con = dbConnect(MySQL(), user = db.user, password = db.password,
                          dbname = db.name, host = db.host, port = 3306)
on.exit(dbDisconnect(wsf.shiny.con), add = T)
dbGetQuery(wsf.shiny.con, "SET NAMES utf8")

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

# Plotting theme
theme_set(theme_bw())

#### UI ####

# Panels we might or might not include in the UI
page.title = "Worship Song Finder"
page.theme = "theme.css"
search.page = tabPanel("Search for songs",
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
              )
if(version == "ctcc") {
  worship.history.page = tabPanel("Worship history",
                                  navlistPanel(
                                    recent.songs.panel,
                                    frequent.songs.panel,
                                    frequent.topics.panel,
                                    songs.by.year.panel
                                  )
                         )
}
song.analysis.page = tabPanel("Song analysis",
                              navlistPanel(
                                songbook.comparison.panel,
                                songbook.overlap.panel,
                                change.over.time.panel,
                                widths = c(3, 9)
                              )
                     )
psalm.singing.page = tabPanel("Psalm singing",
                              sidebarLayout(
                                sidebarPanel(
                                  psalm.number.filter,
                                  psalm.song.type.filter,
                                  width = 3
                                ),
                                mainPanel(
                                  uiOutput("psalmSongList"),
                                  width = 9
                                )
                               )
                     )
help.page = tabPanel("Help",
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
                       tabPanel("Who should I contact with questions/comments/complaints/etc.?",
                                includeHTML("help/contact.html"))
                     )
            )

# Define UI
if(version == "ctcc") {
  ui <- navbarPage(
    page.title,
    theme = page.theme,
    search.page,
    worship.history.page,
    song.analysis.page,
    psalm.singing.page,
    help.page
  )
} else {
  ui <- navbarPage(
    page.title,
    theme = page.theme,
    search.page,
    song.analysis.page,
    psalm.singing.page,
    help.page
  )
}

#### Server ####

# Define server logic
server <- function(input, output, session) {

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

  # Populate outputs
  source("populate_outputs.R", local = T)
  
}

#### Run ####

# Run the application 
shinyApp(ui = ui, server = server)

