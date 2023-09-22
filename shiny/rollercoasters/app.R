#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


# My notes are located here: https://github.com/benda18/shiny_dash_BoS#shiny-resources
# my proof of concept dash from ncceh: https://github.com/benda18/shiny_dash_BoS/tree/main/proof_of_concept_app

library(dplyr)
library(readr)
library(shiny)
library(data.table)
library(ggplot2)
# library(tigris)
# library(tidycensus)
library(glue)


# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel("Proof of Concept Shiny Dashboard - Tim Bender, NCCEH (April 2023)"),
                div(h4("Proof of Concept Shiny Dashboard Demonstrating the following:"),
                    ("* Ability to add [dummy] data table(s) to dashboard"),
                    br("* Ability to add map(s) to dashboard"), 
                    ("* Ability to interact with table(s) and map(s) through user inputs (checkbox)"), 
                    br("* Hosted web deployment"),
                    h4("Still needs to demonstrate the Following:"), 
                    ("* tidy and clean data to meet dashboard's full needs"), 
                    br("* implement date range slider as an interactive filter"), 
                    ("* Aesthetic improvements (colors, sizes, locations, layouts, etc)"), 
                    br("* Demonstrate working proof of complex multi-filter dashboard interactions (i.e. Race + Date + Region)"), 
                    ("* all items/issues in smartsheet can be addressed")),
                # sidebar----
                sidebarLayout(sidebarPanel(
                  # Select Regions by checkbox----
                  checkboxGroupInput(inputId = "checkGroup01", 
                                     label = h3("Select by Region"), # h3 is a markup / html tag for heading
                                     choices = list("Region 1" = "Region 1", 
                                                    "Region 2" = "Region 2", 
                                                    "Region 3" = "Region 3", 
                                                    "Region 4" = "Region 4", 
                                                    "Region 5" = "Region 5", 
                                                    "Region 6" = "Region 6", 
                                                    "Region 7" = "Region 7", 
                                                    "Region 8" = "Region 8", 
                                                    "Region 9" = "Region 9", 
                                                    "Region 10" = "Region 10", 
                                                    "Region 11" = "Region 11", 
                                                    "Region 12" = "Region 12", 
                                                    "Region 13" = "Region 13"),
                                     selected = paste("Region", 1:13, sep = " ")),
                  #hr(),  # this is an html tag <hr> for creating a thematic break in a page
                  # fluidRow(column(3, verbatimTextOutput("value01"))), # prints the results of the checkboxes
                ), 
                mainPanel(plotOutput(outputId = "basemap01"), 
                          tableOutput(outputId = "table01"))))

# Define server logic required to draw a histogram
server <- function(input, output) {
  # load data
  pretend.df <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/shiny_dash_BoS/main/proof_of_concept_app/summary_agegroups_by_region.csv")
  devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/shiny_dash_BoS/main/modules/MODULE_mapping.R?raw=TRUE")
  
  # other stuff
  output$basemap01 <- renderPlot({
    basemap+
      geom_sf(data = bos_regions[bos_regions$Region %in% input$checkGroup01,], # filter regions here 
              color = "white",
              aes(fill = Region_f))+
      geom_sf(data = nc_bound, 
              color = "grey", 
              fill = NA) +
      geom_sf(data = bos_counties[bos_counties$Region %in% input$checkGroup01,],  # filter regions here
              fill = NA, color = "black") +
      coord_sf(xlim = range(bbox.nc[c("xmin", "xmax")]), 
               ylim = range(bbox.nc[c("ymin", "ymax")]))+
      scale_fill_discrete(name = "BoS Region")
  })
  output$table01 <- renderTable({
    pretend.df[pretend.df$Region %in% input$checkGroup01,]
  })
  # You can access the values of the widget (as a vector)
  # with input$checkGroup01, e.g.
  output$value01 <- renderPrint({ 
    input$checkGroup01 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
