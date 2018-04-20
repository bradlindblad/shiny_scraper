get.data <- function(x){
  library(rvest)
  urlz <- read_html("https://coinmarketcap.com/gainers-losers/")
  urlz <- html_table(urlz)
  
  
  bra <- urlz[[1]]
  bra$`% 1h` <- gsub("%","",bra$`% 1h`)
  bra$`% 1h`<- as.numeric(bra$`% 1h`)
  bra$Symbol <- as.factor(bra$Symbol)
  
  bra$Symbol <- factor(bra$Symbol, levels = bra$Symbol[order(bra$'% 1h')])
  bra
}


library(shiny)


# Define the UI

ui <- fluidPage(
  
  titlePanel("1HR Gainers/Losers"),
  
  titlePanel("Brad did this; data refreshes every 60 sec"),
  
  fluidRow(DT::dataTableOutput("table")),
  
  fluidRow(plotOutput("plot"))
  
  
)

library(ggplot2)

# Define the server code
server <- function(input, output) {
  
  #############
  
  liveish_data <- reactive({
    invalidateLater(60000)
    get.data()
  })
  
  
  #############
  
  
  
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- liveish_data()}))
  
  output$plot <- renderPlot({ (ggplot(data=liveish_data(), aes(x=Symbol, y=`% 1h`)) +
                                 geom_bar(stat="identity") +
                                 theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                 ggtitle("Gainers from the Last Hour"))
  })
  
}
# Return a Shiny app object
shinyApp(ui = ui, server = server)


