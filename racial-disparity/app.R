
library(tidyverse)
library(scales)
library(shiny)
library(plotly)

# read in the data
load("arrests.RData")

arb <- arrests %>% group_by(arrestoffense) %>% 
  summarise(count=sum(numberofarrestees, na.rm=T)) %>% arrange(-count)

arb <- subset(arb, count>4)

arrests$racespec <- sapply(strwrap(arrests$racespec, 20, simplify=FALSE), paste, collapse="\n" )

groupsum <- arrests %>% group_by(arrestoffense, racespec) %>% summarize(groupsum=sum(numberofarrestees, na.rm=T))

arrests <- left_join(arrests, groupsum, by=names(groupsum)[1:2])

arrests$perc <- 100*arrests$numberofarrestees/arrests$groupsum

wide <- spread(arrests[,-c(4:5)], key=arresttype, value="perc")

# wide$racespec <- factor(wide$racespec, levels=c("White Alone",
#                                                 "White & Hispanic or\nLatino",
#                                                 "Black or African\nAmerican",
#                                                 "Asian",
#                                                 "American Indian or\nAlaska Native",
#                                                 "Native Hawaiian or\nOther Pacific\nIslander"))
# 
# wide$racespec <- droplevels(wide$racespec)
# .........................................................................


ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput(inputId = "offense",
                  label = "Arrest Offense",
                  choices= arb$arrestoffense, 
                  selected="Crimes Against Person")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotlyOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  output$distPlot <- renderPlotly({
    wide %>% subset(arrestoffense==input$offense) %>% 
      plot_ly(x = ~racespec, y = ~`Taken into Custody`, name = 'Taken into Custody', type = 'bar') %>% 
      add_trace(y = ~`Summoned / Cited`, name = 'Summoned / Cited') %>% 
      add_trace(y = ~`On-View`, name='On-View') %>% 
      layout(yaxis = list(title = 'Percent % of Arrestees'), barmode = 'stack')
    })
  
}

shinyApp(ui = ui, server = server)

