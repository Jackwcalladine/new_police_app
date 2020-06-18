library(shiny)
library(leaflet)
library(shinyjs)
library(shinyBS)
library(leaflet)
library(highcharter)
library(dplyr)
library(ggplot2)
library(lubridate)
library(viridisLite)
library(webshot)
library(rsconnect)

source("functions.R")

rsconnect::setAccountInfo(name='jackwilliamcalladine',
                          token='2DFEA3E1A58F574D44C7326569CEBE1D',
                          secret='zAWIiQqo8vs/S+W5CL5WSqAZBiW6UAvctcpoYtVu')



# global objects ----------------------------------------------------------

df <- read.csv("stop_search_data.csv")
df$Dates <- as.Date(df$Dates, format = "%d/%m/%Y")

# colour theme for graphs
cols <- viridis(3)
cols <- substr(cols, 0, 7)

thm <- hc_theme(
  colors = cols,
  chart = list(
    backgroundColor = NULL
  )
)  


ui <- shinyUI(navbarPage(title = "Police Project",
                         theme = "style1.css",
                         footer = includeHTML("footer.html"),
                         fluid = TRUE, 
                         collapsible = TRUE,
                         
                         # ----------------------------------
                         # tab panel 1 - Home
                         tabPanel("Home",
                                  includeHTML("home.html"),
                                  tags$script(src = "plugins/scripts.js"),
                                  tags$head(
                                      tags$link(rel = "stylesheet", 
                                                type = "text/css", 
                                                href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                                      tags$link(rel = "icon", 
                                                type = "image/png", 
                                                href = "images/logo_icon.png")
                                  )
                         ),
                         
                         # ----------------------------------
                         # tab panel 2 - Crime Browser
                         tabPanel("Crime",
                                  CrimeDescription()
                                  
                         )
)
)

server <- function(input, output) {
    
    ##  ............................................................................
    ##  Crime browser                                                           ####
  

# creating data subset ----------------------------------------------------
  
  crimeVal <- eventReactive(input$graphButton, {
      searchData <- df %>%
      filter(as.character(Dates) > input$Time[1] & as.character(Dates) < input$Time[2])%>%
      filter(as.character(SearchReason) %in% input$SearchReason)

  })
  
  #data for drilldown graph
  drilldown <- eventReactive(input$canvasClicked, {
    df <- crimeVal()
    
    df %>%
      group_by(month=floor_date(Dates, "month"))%>%
      filter(month == input$canvasClicked[2])%>%
      group_by(SearchReason) %>%
      tally()
    
  })
      
  #data for demographic graphs
  dem_graph <- eventReactive(input$graphButton, {
    df <- crimeVal()
    
    df <- df %>%
      filter(as.character(Dates) > input$Time[1] & as.character(Dates) < input$Time[2])%>%
      filter(as.character(SearchReason) %in% input$SearchReason)%>%
      group_by(Officer.defined.ethnicity) %>%
      tally()
  })
  
  outcome_data <- eventReactive(input$graphButton, {
    df <- crimeVal()
    
    df <- df %>%
      group_by(Outcome)%>%
      tally()
  })
  

# map output --------------------------------------------------------------
    
  output$map <- renderLeaflet({
    df <- crimeVal()
    
    leaflet() %>%
      addTiles()%>%
      setView(lng = 0.915, lat = 51.89, zoom = 15)%>%
      addProviderTiles(input$MapType)%>%
      addCircleMarkers(lat = df$Latitude, lng = df$Longitude)
  })
  
  

# charts ------------------------------------------------------------------

  
  
  output$searchChart <- renderHighchart({
    df <- crimeVal()
    
    df <- df %>%
      group_by(month=floor_date(Dates, "month")) %>%
      tally()
    
    canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")
      
    highchart() %>%
      hc_xAxis(categories = df$month) %>% 
      hc_add_series(name = "number", data = df$n) %>%
      hc_plotOptions(series = list(events = list(click = canvasClickFunction))) %>%
      hc_chart(type = "line")%>%
      hc_title(text = "Number of Stop and Searches")%>%
      hc_add_theme(thm)
    
  })
  
  output$drilldownChart <- renderHighchart({
    df <- drilldown()
    
    highchart() %>%
      hc_xAxis(categories = df$SearchReason)%>%
      hc_add_series(name = "number", data = df$n) %>%
      hc_plotOptions(column = list(colorByPoint = TRUE))%>%
      hc_chart(type = "column") %>%
      hc_title(text = paste("data for", input$canvasClicked[2])) %>%
      hc_add_theme(thm)
  
  })
  
  output$demgraph <- renderHighchart({
    df <- dem_graph()
    
    highchart() %>%
      hc_xAxis(categories = df$Officer.defined.ethnicity)%>%
      hc_add_series(name = "number", data = df$n) %>%
      hc_chart(type = "column") %>%
      hc_plotOptions(column = list(colorByPoint = TRUE))%>%
      hc_title(text = "Demographic Breakdown of Searches")%>%
      hc_add_theme(thm)
     
  }) 
  
  output$outcomegraph <- renderHighchart({
    df <- outcome_data()
    
    highchart() %>%
      hc_xAxis(categories = df$Outcome)%>%
      hc_add_series(name = "number", data = df$n) %>%
      hc_chart(type = "column") %>%
      hc_plotOptions(column = list(colorByPoint = TRUE))%>%
      hc_title(text = "Outcome of Searches")%>%
      hc_add_theme(thm)

  })
}

    
# run app -----------------------------------------------------------------
shinyApp(ui = ui, server)
