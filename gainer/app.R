library(shiny)
library(tidyverse)
library(shinydashboard)
library(rvest)

#####################
####### F N S #######
#####################

get.data <- function(x){

  urlz <- read_html("https://coinmarketcap.com/gainers-losers/")
  urlz <- html_table(urlz)
  
  
  bra <- urlz[[1]]
  bra$`% 1h` <- gsub("%","",bra$`% 1h`)
  bra$`% 1h`<- as.numeric(bra$`% 1h`)
  bra$Symbol <- as.factor(bra$Symbol)
  
  bra$Symbol <- factor(bra$Symbol, levels = bra$Symbol[order(bra$'% 1h')])
  bra
}

get.infobox.val <- function(x){
  
  df1 <- get.data()
  df1 <- df1$`% 1h`[1]
  df1
  
}

get.infobox.coin <- function(x){
  
  df <- get.data()
  df <- df$Name[1]
  df
  
}

#####################
####### U I #########
#####################

ui <- dashboardPage(
  
  
  # H E A D E R
  
  dashboardHeader(title = "Alt Coin Gainers"),
  
  # S I D E B A R
  
  dashboardSidebar(
    
    h5("A slightly interactive dashboard that pulls the top gainers from the last hour from
       coinmarketcap.com. Refreshes every 60 seconds."),
    
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    
    h6("Built by Brad Lindblad in the R computing language 
      [  R Core Team (2018). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
  Vienna, Austria. URL https://www.R-project.org/]"),
    br(),
    h6("R version 3.4.4 (2018-03-15) 'Someone to Lean On'"),
    br(),
    a("bradley.lindblad@gmail.com", href="mailto:bradley.lindblad@gmail.com")
    
  ),
  
  # B O D Y
  dashboardBody(
    
  fluidRow(
    
    # InfoBox
    infoBoxOutput("top.coin",
                  width = 3),
    
    # InfoBox
    infoBoxOutput("top.name",
                  width = 3)
    
  ),
  
  fluidRow(

    column(
    # Datatable
      box(
        status = "primary",
        headerPanel("Data Table"),
        solidHeader = T,
        br(),
        DT::dataTableOutput("table", height = "350px"),
        width = 6,
        height = "560px"
      ),
      
      # Chart
      box(
        status = "primary",
        headerPanel("Chart"),
        solidHeader = T,
        br(),
        plotOutput("plot", height = "400px"),
        width = 6,
        height = "500px"
      ),
      width = 12
    )
    
  )
  )
  
)
  
  

#####################
#### S E R V E R ####
#####################

server <- function(input, output) {
  

  # R E A C T I V E 
  liveish_data <- reactive({
    invalidateLater(60000)    # refresh the report every 60k milliseconds (60 seconds)
    get.data()                # call our function from above
  })
  
  
  live.infobox.val <- reactive({
    invalidateLater(60000)    # refresh the report every 60k milliseconds (60 seconds)
    get.infobox.val()                # call our function from above
  })
  
  
  live.infobox.coin <- reactive({
    invalidateLater(60000)    # refresh the report every 60k milliseconds (60 seconds)
    get.infobox.coin()                # call our function from above
  })
  
  # D A T A   T A B L E   O U T P U T
  output$table <- DT::renderDataTable(DT::datatable({
    data <- liveish_data()}))
  
  
  # P L O T   O U T P U T
  output$plot <- renderPlot({ (ggplot(data=liveish_data(), aes(x=Symbol, y=`% 1h`)) +
                                 geom_bar(stat="identity", fill = "springgreen3") +
                                 theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                 ggtitle("Gainers from the Last Hour"))
  })
 
  
  
  # I N F O B O X   O U T P U T - V A L
  output$top.coin <- renderInfoBox({
    infoBox(
      "Gain in Last Hour",
      paste0(live.infobox.val(), "%"),
      icon = icon("signal"),
      color = "purple",
      fill = TRUE)
  })
   
  
  # I N F O B O X   O U T P U T - N A M E
  output$top.name <- renderInfoBox({
    infoBox(
      "Coin Name",
      live.infobox.coin(),
      icon = icon("bitcoin"),
      color = "purple",
      fill = TRUE)
  })
  
}

#####################
#### D E P L O Y ####
#####################

# Return a Shiny app objectshinyApp(ui = ui, server = server)
shinyApp(ui = ui, server = server)

