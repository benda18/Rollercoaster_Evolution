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
library(devtools)

source("C:/Users/bende/Documents/R/play/rollercoaster_evolution/shiny/rollercoasters/module.R")

# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel("title"),
                div("<div>"),
                mainPanel("mainPanel", 
                          tableOutput(outputId = "table01"), 
                          tableOutput(outputId = "table02"), 
                          tableOutput(outputId = "table03"),
                          tableOutput(outputId = "table04"))
                # sidebar----
                # sidebarLayout("sidebarLayout",
                #               sidebarPanel("<sidebar panel>"), 
                #               mainPanel("mainpanel"
                #                         #plotOutput(outputId = "basemap01"), 
                #                         #tableOutput(outputId = "table01")
                #               )
                # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # load data
  # pretend.df <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/shiny_dash_BoS/main/proof_of_concept_app/summary_agegroups_by_region.csv")
  # devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/shiny_dash_BoS/main/modules/MODULE_mapping.R?raw=TRUE")
  
  # other stuff
  # output$basemap01 <- renderPlot({
  #   basemap+
  #     geom_sf(data = bos_regions[bos_regions$Region %in% input$checkGroup01,], # filter regions here 
  #             color = "white",
  #             aes(fill = Region_f))+
  #     geom_sf(data = nc_bound, 
  #             color = "grey", 
  #             fill = NA) +
  #     geom_sf(data = bos_counties[bos_counties$Region %in% input$checkGroup01,],  # filter regions here
  #             fill = NA, color = "black") +
  #     coord_sf(xlim = range(bbox.nc[c("xmin", "xmax")]), 
  #              ylim = range(bbox.nc[c("ymin", "ymax")]))+
  #     scale_fill_discrete(name = "BoS Region")
  # })
  output$table01 <- renderTable({
    SHINY_avg.length_by.design_by.yr
    #pretend.df[pretend.df$Region %in% input$checkGroup01,]
  })
  output$table02 <- renderTable({
    SHINY_ride.design_by.year_by.park
  })
  output$table03 <- renderTable({
    SHINY_avg.length_by.design_by.yr
  })
  output$table04 <- renderTable({
    SHINY_ride.specs_by.year
  })
  # You can access the values of the widget (as a vector)
  # with input$checkGroup01, e.g.
  output$value01 <- renderPrint({ 
    input$checkGroup01 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
