library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)


setwd("C:/Users/giran/OneDrive/Dokumentumok/UU MSBBSS/Markup-course/first-app")
options(repos = c(CRAN = "https://cloud.r-project.org/"))


rsconnect::setAccountInfo(name='kyra-giran', token='41480065E1A519F413C3AA99A5EE0FFA', secret='998IDe4a8+HWvvrteTIRpcU3iKtWfSXKU83UNw24')

# Load the dataset 
imdb_data <- read.csv("data//imdb_top_1000.csv")

# Clean and prepare data
imdb_data <- imdb_data %>%
  mutate(
    Year = as.numeric(gsub("[^0-9]", "", Released_Year)),  # Extract only numbers
    Rating = as.numeric(IMDB_Rating),
    Votes = as.numeric(gsub(",", "", No_of_Votes))  # Remove commas for numeric conversion
  ) %>%
  filter(!is.na(Year) & !is.na(Rating))

summary(imdb_data$Year)  

ui <- fluidPage(
  titlePanel("IMDB Movie Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h3("🔍 Filter Movies"),
      sliderInput("yearRange", "Year Range", 
                  min = min(imdb_data$Year), max = max(imdb_data$Year), 
                  value = c(2000, 2020)),
      sliderInput("ratingRange", "IMDB Rating", 
                  min = 0, max = 10, value = c(7, 10)),
      selectInput("genre", "Genre", 
                  choices = unique(imdb_data$Genre), 
                  selected = "Drama", multiple = TRUE),
      textInput("movieSearch", "Search Movie Title", value = ""),
      actionButton("reset", "Reset Filters")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", plotOutput("ratingTrend")),
        tabPanel("Top Movies", tableOutput("topMovies")),  # Standard table output
        tabPanel("Movie Details", uiOutput("movieDetails"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered data
  filteredData <- reactive({
    data <- imdb_data %>%
      filter(
        Year >= input$yearRange[1] & Year <= input$yearRange[2],
        Rating >= input$ratingRange[1] & Rating <= input$ratingRange[2]
      )
    
    # Filter by genre
    if (!is.null(input$genre) && length(input$genre) > 0) {
      data <- data %>%
        filter(grepl(paste(input$genre, collapse = "|"), Genre))
    }
    
    # Search by movie title
    if (input$movieSearch != "") {
      data <- data %>%
        filter(grepl(input$movieSearch, Series_Title, ignore.case = TRUE))
    }
    
    return(data)
  })
  
  # Plot: Rating Trend over Years
  output$ratingTrend <- renderPlot({
    filteredData() %>%
      group_by(Year) %>%
      summarize(AverageRating = mean(Rating, na.rm = TRUE)) %>%
      ggplot(aes(x = Year, y = AverageRating)) +
      geom_line(color = "pink", size = 1) +
      labs(title = "Average IMDB Rating Over Years", x = "Year", y = "Average Rating") +
      theme_minimal()
  })
  
  # Table: Top Movies (Now using renderTable)
  output$topMovies <- renderTable({
    filteredData() %>%
      arrange(desc(Rating)) %>%
      select(Series_Title, Genre, Year, Rating, No_of_Votes) %>%
      head(10)  # Show top 10 movies
  }, striped = TRUE, hover = TRUE)
  
  # Movie Details UI
  output$movieDetails <- renderUI({
    req(input$movieSearch)
    movie <- filteredData() %>%
      filter(Series_Title == input$movieSearch) %>%
      slice(1)
    
    if (nrow(movie) == 0) {
      return("No movie found. Try another title.")
    }
    
    tagList(
      h2(movie$Series_Title),
      tags$img(src = movie$Poster_Link, height = "300px"),
      h4("Genre: ", movie$Genre),
      h4("Year: ", movie$Year),
      h4("Rating: ", movie$Rating),
      h4("Director: ", movie$Director),
      h4("Overview: "),
      p(movie$Overview)
    )
  })
  
  # Reset filters
  observeEvent(input$reset, {
    updateSliderInput(session, "yearRange", value = c(2000, 2020))
    updateSliderInput(session, "ratingRange", value = c(7, 10))
    updateSelectInput(session, "genre", selected = "Drama")
    updateTextInput(session, "movieSearch", value = "")
  })
}

shinyApp(ui = ui, server = server)

library(rsconnect)
deployApp()


