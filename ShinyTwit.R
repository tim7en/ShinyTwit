rm(list = ls())
source("pcg.R")

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 70 * 1024^2) # Max csv data limit set to 60 mb

  #Show popup on click
  observeEvent(input$map_click, {
    click <- input$map_click
    text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
    proxy <- leafletProxy("map")
    proxy %>% clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap) %>% 
      setView(lng = -4, lat= 52.54, zoom = 3)
  })

  plotTrending <- eventReactive(input$looktrending, {
    req(input$TweetsN)
    req(input$trending)
    x <- searchTwitter(input$trending, n = input$TweetsN, lang = "en")
    x <- twListToDF(x)
  })

  textClean <- reactive({
    x <- plotTrending()
    # extract text
    x_text <- x$text
    # convert all text to lower case
    x_text <- tolower(x_text)
    # Replace blank space ("rt")
    x_text <- gsub("rt", "", x_text)
    # Replace @UserName
    x_text <- gsub("@\\w+", "", x_text)
    # Remove punctuation
    x_text <- gsub("[[:punct:]]", "", x_text)
    # Remove links
    x_text <- gsub("http\\w+", "", x_text)
    # Remove tabs
    x_text <- gsub("[ |\t]{2,}", "", x_text)
    # Remove blank spaces at the beginning
    x_text <- gsub("^ ", "", x_text)
    # Remove blank spaces at the end
    x_text <- gsub(" $", "", x_text)
    # calculationg total score for each sentiment
  })

  sentim <- reactive({
    x_text <- textClean()
    # convert into corpus type
    x_text.text.corpus <- Corpus(VectorSource(x_text))
    # clean up by removing stop words
    x_text.text.corpus <- tm_map(x_text.text.corpus, function(x) removeWords(x, stopwords()))
    # getting emotions using in-built function
    mysentiment_x <- get_nrc_sentiment((x_text))
  })

  output$p1 <- renderPlot({
    mysentiment_x <- sentim()
    Sentimentscores_x <- data.frame(colSums(mysentiment_x[, ]))
    names(Sentimentscores_x) <- "Score"
    Sentimentscores_x <- cbind("sentiment" = rownames(Sentimentscores_x), Sentimentscores_x)
    rownames(Sentimentscores_x) <- NULL
    # plotting the sentiments with scores
    ggplot(data = Sentimentscores_x, aes(x = sentiment, y = Score)) + geom_bar(aes(fill = sentiment), stat = "identity") +
      theme(legend.position = "none") +
      xlab("Sentiments") + ylab("scores") + ggtitle(paste("Sentiments of people behind the tweets on", input$trending, sep = " "))
  }, height = 850, width = 1050)

  output$p2 <- renderPlot({
    req(textClean())
    x_text <- textClean()
    # convert into corpus type
    x_text.text.corpus <- Corpus(VectorSource(x_text))
    # clean up by removing stop words
    x_text.text.corpus <- tm_map(x_text.text.corpus, function(x) removeWords(x, stopwords()))
    dtm <- TermDocumentMatrix(x_text.text.corpus)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v), freq = v)
    # dev.cur(width = 1000, height = 1000, unit = "px")
    wordcloud(
      words = d$word, scale = c(4, .5), freq = sqrt(d$freq), min.freq = 1,
      max.words = 200, random.order = TRUE, use.r.layout = FALSE,
      rot.per = 0.35,
      colors = brewer.pal(8, "Dark2")
    )
  }, height = 650, width = 750)

  output$p3 <- renderPlot({
    req(textClean())
    x_text <- textClean()
    # convert into corpus type
    x_text.text.corpus <- Corpus(VectorSource(x_text))
    # clean up by removing stop words
    x_text.text.corpus <- tm_map(x_text.text.corpus, function(x) removeWords(x, stopwords()))
    dtm <- TermDocumentMatrix(x_text.text.corpus)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v), freq = v)
    barplot(d[1:30, ]$freq,
      las = 2, names.arg = d[1:30, ]$word,
      col = "lightblue", main = "Most frequent words",
      ylab = "Word frequencies"
    )
  }, height = 750, width = 750)

  trends <- reactive ({
    req(input$map_click)
    click <- input$map_click
    woeid <- closestTrendLocations(lat = click$lat, long = click$lng)
    current_trends <- getTrends(as.numeric(woeid[3]))
    current_trends <- as.data.frame(current_trends)
    current_trends$trend_date <- Sys.Date()
    names(current_trends)[1] <- "Trending"
    current_trends
  })

  output$trends <- DT::renderDT({
    req(input$map_click)
    as.data.frame(trends())
  })

  output$p <- renderPlot({
    req(trends())
    datas <- trends()
    x <- datas[, 1]
    y <- seq(1, length(x))
    y <- y^2
    wordcloud(x, sqrt(rev(y)), min.freq = 1, colors = brewer.pal(8, "Dark2"), random.order = TRUE, use.r.layout = FALSE, max.words = 200, rot.per = 0.35)
  }, height = 950, width = 1050)
}


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "ShinyTwit"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Trending", tabName = "dataInput", icon = icon("upload")),
      menuItem("Search", tabName = "regressions", icon = icon("random")),
      menuItem("Analytics", tabName = "mixmod", icon = icon("cubes"))
    )
  ),
  dashboardBody( # Body content
    tabItems(
      tabItem( # First tab content
        tabName = "dataInput",
        fluidRow(
          column(
            width = 9,
            box(
              title = "Twitter Review: ", status = "success", height =
                "1595", width = "12", solidHeader = T,
              tabsetPanel(
                tabPanel(
                  "Location",
                  box(
                    width = 12,
                    leafletOutput("map", height = 800)
                  )
                ),
                tabPanel(
                  "WordCloud",
                  column(12,
                         align = "center",
                    box(
                      width = 12,
                      plotOutput("p"), style = "height:800px;width:600;overflow-y: scroll;overflow-x: scroll;"
                    )
                  )
                ),
                tabPanel(
                  "Table",
                  box(
                    width = 12,
                    DT::dataTableOutput("trends"), style = "height:700px; overflow-y: scroll;overflow-x: scroll;"
                  )
                )
              )
            )
          )
        )
      ),
      tabItem( # First tab content
        tabName = "regressions",
        fluidRow(
          column(
            width = 3,
            box(
              title = "Twitter Input: ", status = "success", height =
                "1595", width = "12", solidHeader = T,
              textInput("trending", "Hashtag", "#Champion", width = 200),
              numericInput("TweetsN", "Number of tweets", 5, min = 1, max = 4000, width = 200),
              actionButton("looktrending", "Lookup") # updated from July 28
            )
          ),
          column(
            width = 9,
            box(
              title = "Twitter Input: ", status = "success", height =
                "1595", width = "12", solidHeader = T,
              tabsetPanel(
                tabPanel(
                  "Sentiment Plot",
                  box(
                    width = 12,
                    column(12,
                      align = "center",
                      plotOutput("p1"), style = "height:800px;width:600;overflow-y: scroll;overflow-x: scroll;"
                    )
                  )
                ),
                tabPanel(
                  "WordCloud",
                  box(
                    width = 12,
                    column(12,
                      align = "center",
                      plotOutput("p2"), style = "height:800px;width:600;overflow-y: scroll;overflow-x: scroll;"
                    )
                  )
                ),
                tabPanel(
                  "Word Frequency",
                  box(
                    width = 12,
                    column(12,
                      align = "center",
                      plotOutput("p3"), style = "height:800px;width:600;overflow-y: scroll;overflow-x: scroll;"
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# Run the app ----
shinyApp(ui, server)
