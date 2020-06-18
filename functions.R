#   ____________________________________________________________________________
#   Crime Browser                                                           ####

CrimeDescription <- function() {
  tagList(
    div(class = "container",
        h1("Crime Browser", class = "title fit-h1"),
        p("Colchester is home to the University of Essex, a diverse university with students from all over the UK and world."),
        p("Use this tool to understand the distribution of arrests, stop and searches, and other forms of policing, aggregated by race and location"),
        fluidRow(
          column(7,
                 leafletOutput("map", height = 600)
          ),
          column(4,
                 selectInput("MapType", "Choose Map Theme",
                             choices = c("Streetview" = "Esri.WorldImagery",
                                         "Normal" = "OpenStreetMap.Mapnik"), 
                             selected = "Normal"),
                 sliderInput("Time", "Choose a Date Range",
                             min = as.Date("01/04/2017", "%d/%m/%Y"),
                             max = as.Date("31/03/2020", "%d/%m/%Y"),
                             value = c(as.Date("01/04/2017", "%d/%m/%Y"),as.Date("31/03/2020", "%d/%m/%Y")),
                             timeFormat="%d/%m/%Y"),
                 selectInput("SearchReason", "Object of Search",
                             multiple = TRUE,
                             choices = c("Firearms",
                                         "Controlled Drugs",
                                         "Stolen goods",
                                         "Offensive weapons",
                                         "Article for use in theft",
                                         "Evidence of offences under the Act",
                                         "Articles for use in criminal damage",
                                         "Anything to threaten or harm anyone")),
                 actionButton("graphButton", "Begin Analyzing")
          ) #End of Columns
        ), #End of First Fluid Row
        
        fluidRow(
          column(6,
                 highchartOutput("searchChart")),
          column(6,
                 highchartOutput("drilldownChart")
          ) #End of Columns
        ), #End of Second Fluid Row
        
        fluidRow(
          column(6,
                 highchartOutput("demgraph")
          ) #End of Columns
        ), #End of Third Fluid Row 
        
        fluidRow(
          column(6,
                 highchartOutput("outcomegraph"))
        )
        
    )
  )
}