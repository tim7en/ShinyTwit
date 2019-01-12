rm(list = ls())
source("pcg.R")
source ("functions.R")

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 70 * 1024^2) # Max csv data limit set to 60 mb

  #Page 1 view, maps
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = -4, lat = 52.54, zoom = 3)
  })
  # Show popup on click
  observeEvent(input$map_click, {
    click <- input$map_click
    text <- paste("Lattitude ", click$lat, "Longtitude ", click$lng)
    proxy <- leafletProxy("map")
    proxy %>%
      clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })

  
  
  #Page 1 view, functions & plots
  trends <- reactive({
    req(input$map_click)
    click <- input$map_click
    woeid <- closestTrendLocations(lat = click$lat, long = click$lng)
    current_trends <- getTrends(as.numeric(woeid[3]))
    current_trends <- as.data.frame(current_trends)
    current_trends$trend_date <- Sys.Date()
    names(current_trends)[1] <- "Trending"
    current_trends
  })
  
  #Word cloud
  output$p <- renderPlot({
    req(trends())
    datas <- trends()
    x <- datas[, 1]
    y <- seq(1, length(x))
    y <- y^2
    wordcloud(x, sqrt(rev(y)), scale = c(1.2,0.2), min.freq = 1, colors = brewer.pal(8, "Dark2"), random.order = TRUE, use.r.layout = FALSE, max.words = 200, rot.per = 0.35)
  }, height = 330, width = 350)
  
  #Dynamic trends selection & sentiment analysis
  output$trends <- DT::renderDT({
    req(input$map_click)
    as.data.frame(trends())
  })
  
  output$top<- renderUI({
    datas<- trends()
    selectInput(inputId = "xE", label = "Sentiment & Frequency, N = 300", choices = datas$Trending)
  })

  Trending_top <- reactive({
    req(input$xE)
    x <- searchTwitter(input$xE, n = 300, lang = "en")
    x <- twListToDF(x)
  })
  
  textClean_top <- reactive({
    x <- Trending_top()
    cleanText (x)
  })
  
  sentim_top <- reactive({
    x_text <- textClean_top()
    x_text.text.corpus <- Corpus(VectorSource(x_text))
    x_text.text.corpus <- tm_map(x_text.text.corpus, function(x) removeWords(x, stopwords()))
    mysentiment_x <- get_nrc_sentiment((x_text))
  })
  
  output$p1_top <- renderPlot({
    mysentiment_x <- sentim_top()
    Sentimentscores_x <- data.frame(colSums(mysentiment_x[, ]))
    names(Sentimentscores_x) <- "Score"
    Sentimentscores_x <- cbind("sentiment" = rownames(Sentimentscores_x), Sentimentscores_x)
    rownames(Sentimentscores_x) <- NULL
    # plotting the sentiments with scores
    ggplot(data = Sentimentscores_x, aes(x = sentiment, y = Score)) + geom_bar(aes(fill = sentiment), stat = "identity") +
      theme(legend.position = "none") +
      xlab("Sentiments") + ylab("scores") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ggtitle(paste("Sentiments of people behind the tweets on", input$xE, sep = " "))
  }, height = 350, width = 350)
  
  output$p3_top <- renderPlot({
    req(textClean_top())
    x_text <- textClean_top()
    # convert into corpus type
    x_text.text.corpus <- Corpus(VectorSource(x_text))
    # clean up by removing stop words
    x_text.text.corpus <- tm_map(x_text.text.corpus, function(x) removeWords(x, stopwords()))
    dtm <- TermDocumentMatrix(x_text.text.corpus)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v), freq = v)
    barplot(d[1:10, ]$freq,
            las = 2, names.arg = d[1:10, ]$word,
            col = "salmon", main = "Most frequent words",
            ylab = "Word frequencies"#,
            #theme(axis.text.x = element_text(angle = 90, hjust = 1))
            
    )
  }, height = 350, width = 350)

  #Page 2 data exploration
  #Separate analysis
  Trending <- eventReactive(input$looktrending, {
    req(input$TweetsN)
    req(input$trending)
    x <- searchTwitter(input$trending, n = input$TweetsN, lang = "en")
    x <- twListToDF(x)
  })

  textClean <- reactive({
    x <- Trending()
    cleanText (x)
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
                "900", width = "12", solidHeader = T,
              tabsetPanel(
                tabPanel(
                  "Location",
                  box(
                    width = 12,
                    leafletOutput("map", height = 750)
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
          ),
          column(
            width = 3, align = "center",
            # box(
            #   title = "Current Trends: ", status = "warning", height =
            #     "400", width = "400", solidHeader = T,
            #   plotOutput("p"), style = "height:350px;width:400;"#overflow-y: scroll;overflow-x: scroll;"
            # ),
            box (
              title = "Top in Trends: ", status = "success", height =
                "900", width = "400", solidHeader = T,
              uiOutput('top'),
              column(12,
                     align = "center",
                     withSpinner(plotOutput("p1_top"))
              ),
              column(12,
                     align = "center",
                     withSpinner(plotOutput("p3_top"))
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
              numericInput("TweetsN", "Number of tweets", 5, min = 1, max = 10001, width = 200),
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
                      withSpinner(plotOutput("p1")), style = "height:800px;width:600;overflow-y: scroll;overflow-x: scroll;"
                    )
                  )
                ),
                tabPanel(
                  "WordCloud",
                  box(
                    width = 12,
                    column(12,
                      align = "center",
                      withSpinner(plotOutput("p2")), style = "height:800px;width:600;overflow-y: scroll;overflow-x: scroll;"
                    )
                  )
                ),
                tabPanel(
                  "Word Frequency",
                  box(
                    width = 12,
                    column(12,
                      align = "center",
                      withSpinner(plotOutput("p3")), style = "height:800px;width:600;overflow-y: scroll;overflow-x: scroll;"
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
