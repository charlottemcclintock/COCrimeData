
# crime type by race by arrest type
arrests %>% subset(arrestoffense %in% c("Crimes Against Person", 
                                        "Crimes Against Property", 
                                        "Crimes Against Society")) %>% 
  ggplot(aes(x=arresttype, y=numberofarrestees, fill=arresteerace)) + 
  geom_bar(stat="identity", position="fill") + facet_wrap(~arrestoffense) + 
  scale_x_discrete(labels=wrap_format(10)) + 
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 8))

# crime type by race by arrest type
arrests %>% subset(arrestoffense %in% c("Crimes Against Person", 
                                        "Crimes Against Property", 
                                        "Crimes Against Society")) %>% 
  subset(!arresteerace %in% c("Missing", "Unknown") & !arrestoffense=="Missing") %>% 
  ggplot(aes(x=arresteerace, y=numberofarrestees, fill=arresttype)) + 
  geom_bar(stat="identity", position="fill") + facet_wrap(~arrestoffense) + 
  scale_x_discrete(labels=wrap_format(10)) + 
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 8))

# ........................................................................

# low level crimes

arb <- arrests %>% group_by(arrestoffense) %>% 
  summarise(sum(numberofarrestees, na.rm=T))

lowlevel <- c("Destruction / Damage / Vandalism of Property", "Shoplifting", 
              'Drug / Narcotic Violations', "All Other Larceny", 
              "Drug Equipment Violations", "Theft From Building", "Driving Under the Influence", 
              "Disorderly Conduct", "Credit Card / Automated Teller Machine Fraud","Liquor Law Violations", 
              "Theft of Motor Vehicle Parts / Accessories", "Curfew / Loitering / Vagrancy Violations")

# arrest type by race for low level offenses
arrests %>% 
  subset(!arrestoffense %in% lowlevel & !arresttype=="Missing" & 
           !arresteerace %in% c("Missing", "Unknown")) %>% 
  group_by(arresttype, arresteerace)  %>% 
  summarize(numberofarrestees=sum(numberofarrestees, na.rm = T)) %>% 
  ggplot(aes(arresteerace, numberofarrestees, fill=arresttype)) +
  geom_bar(stat="identity", position="fill") + coord_flip()

# arrests for low level offenses
arrests %>% 
  subset(!arrestoffense %in% lowlevel & !arresttype=="Missing") %>% 
  group_by(arresttype, arresteerace)  %>% 
  summarize(numberofarrestees=sum(numberofarrestees, na.rm = T)) %>% 
  ggplot(aes(arresttype, numberofarrestees, fill=arresteerace)) +
  geom_bar(stat="identity", position="fill")

# drug arrests
arrests %>% subset(arrestoffense=="Drug/Narcotic Violations") %>% 
  ggplot(aes(arresteerace, numberofarrestees, fill=arresttype)) + 
  geom_bar(stat="identity", position="fill")

# ........................................................................

# how often is someone taken into custody vs. summons for each offense type?

# arrests by specific offense type and arrest type 
arrests %>% subset(!arrestoffense %in% c("Crimes Against Person", 
                                         "Crimes Against Property", 
                                         "Crimes Against Society") & 
                     !numberofarrestees=="0") %>% 
  group_by(arrestoffense, arresttype) %>% 
  summarize(numberofarrestees=sum(numberofarrestees, na.rm = T)) %>% 
  ggplot(aes(reorder(arrestoffense, numberofarrestees), numberofarrestees, fill=arresttype)) + 
  geom_bar(stat="identity", position="fill") + coord_flip() + 
  scale_x_discrete(labels=wrap_format(25)) + 
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 8)) 

# 

arrests %>% subset(arrestoffense=="Crimes Against Person" & !arresttype=="Missing") %>%  
  ggplot(aes(x=arresttype, y=numberofarrestees, fill=arresteerace)) + 
  geom_bar(stat="identity", position="fill") + 
  scale_x_discrete(labels=wrap_format(10)) + theme_minimal() +
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 8))

arrests %>% subset(arrestoffense=="Crimes Against Person" & !arresttype=="Missing" & 
                     !numberofarrestees=="0") %>%  
  ggplot(aes(x=arresteerace, y=numberofarrestees, fill=arresttype)) + 
  geom_bar(stat="identity", position="fill") + 
  scale_x_discrete(labels=wrap_format(10)) + theme_minimal() +
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 8))


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



