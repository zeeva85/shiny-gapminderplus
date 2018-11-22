library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)

gpmd2 <- read.csv("./gapminder2.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  includeCSS("styles2.css"),
  
  tags$script(
    "//alert('Hi welcome to the Gapminderplus dataset page');"
  ),
  
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  # ),
  
  titlePanel("Gapminderplus"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Life expectancy (years)", 0, 100, c(20, 80)),
      radioButtons("typeInput", "year",
                   choices = c("All", paste(unique(gpmd2$year))),
                   selected = "All"),
      radioButtons("filetype", "File type:",
                   choices = c("csv", "tsv")),
      downloadButton('downloadData', 'Download'),
      uiOutput("countryOutput"),
      span("Data Source:",
           a("Gapminderplus",
             href = "https://github.com/zeeva85/gapminderplus"))
    ),
    mainPanel(
      tags$img(src = "./logo.png"),
      br(), br(),
      plotOutput("coolplot"),
      br(), br(),
      br(), br(),
      DT::dataTableOutput("results")
    )
  )
)


# Not in use because using library (shinyWidgets)
# if (input$countryInput != "All") {
#   country == input$countryInput
# } else {
#   year == paste(unique(gpmd2$country))
  

server <- function(input, output) {
  output$countryOutput <- renderUI({
    pickerInput("countryInput", "Country",
                sort(unique(gpmd2$country)),
                options = list(`actions-box` = TRUE), multiple = T,
                selected = "Malaysia"
                )
    # selectInput("countryInput", "Country",
    #             sort(c("All", unique(gpmd2$country))),
    #             selected = "All")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    gpmd2 %>%
      filter(lifeExp >= input$priceInput[1],
             lifeExp <= input$priceInput[2],
             if (input$typeInput != "All") {
               year %in% input$typeInput
             } else {
               year %in% unique(gpmd2$year)
             },
             if (input$countryInput != "All"){
               country %in% input$countryInput 
             } else {
               country %in% unique(gpmd2$country)
               }
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(gdpPercap, lifeExp, size = pop, colour = continent)) +
      geom_point()
  })
  
  output$results <- DT::renderDataTable({
    filtered()
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      ext <- switch(input$filetype, "csv" = ".csv", "tsv" = ".tsv")
      paste("gapminderplus-", Sys.Date(), ext, sep = "")
    },
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      # ftype <- input$filetype # altenative
      # if (ftype == "csv") sep <- "," else sep <- "\t"
      write.table(filtered(), file, sep = sep, row.names = F)
    }
  )
}

shinyApp(ui = ui, server = server)

