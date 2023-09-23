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
                          #tableOutput(outputId = "table01"), 
                          plotOutput(outputId = "plot01")
                          #tableOutput(outputId = "table02"), 
                          #tableOutput(outputId = "table03"),
                          #tableOutput(outputId = "table04")
                          ),
                #ui  slider----
                sliderInput("slider_ts", label = h3("Slider"), min = 10, 
                            max = 30, value = 18),
                
                # ui.R RADIO BUTTIONS
                radioButtons("radio", label = h3("Radio buttons"),
                             choices = list("Total Rides" = "stack", 
                                            "Percent of Rides" = "fill"), 
                             selected = "fill"),
                
                hr(),
                fluidRow(column(3, verbatimTextOutput("valueR"))),
                
                # ui.R CHECKBOX GROUP----
                checkboxGroupInput("park_name01", 
                                   label = h3("Checkbox group"), 
                                   choices = list("Kings Island" = "kings_island", 
                                                  "Cedar Point" = "cedar_point", 
                                                  "Carowinds" = "carowinds", 
                                                  "Kings Dominion" = "kings_dominion"),
                                   selected = "cedar_point"),
                
                # # sidebar----
                # sidebarLayout("sidebarLayout",
                #               sidebarPanel("<sidebar panel>"),
                #               
                #               # mainPanel("mainpanel"
                #               #           #plotOutput(outputId = "basemap01"),
                #               #           #tableOutput(outputId = "table01")
                #               # )
                # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # slider for plot text size----
  output$valueS <- renderPrint({
    input$slider_ts
  })
  # server.R RADIO BUTTON TO SELECT PLOT LAYOUT
  output$valueR <- renderPrint({ 
    input$radio 
    })
  
  # Render tables----
  output$table01 <- renderTable({
    SHINY_avg.length_by.design_by.yr %>%
      data.table::as.data.table() %>%
      dcast(., 
            year ~ design, 
            fun.aggregate = sum, 
            value.var = "avg_length", 
            fill = 0) %>%
      as.data.frame() %>%
      mutate(., 
             year = as.integer(year))
    
  })
  output$plot01 <- renderPlot({
    SHINY_ride.design_by.year_by.park[SHINY_ride.design_by.year_by.park$park_name %in% 
                                        input$park_name01,] %>%
      ggplot(data = ., 
             aes(x = year, y = n_rides, 
                 fill = design)) + 
      geom_col(position = input$radio)+
      #theme(text = element_text(size = 25))+
      theme(text = element_text(size = input$slider_ts))+
      scale_y_continuous(labels = ifelse(input$radio == "fill", 
                                   scales::percent, 
                                   scales::comma))
    
    
    
  })
  output$table03 <- renderTable({
    SHINY_avg.length_by.design_by.yr
  })
  output$table04 <- renderTable({
    SHINY_ride.specs_by.year
  })
  
  
  # CHECKBOX TO SELECT THEMEPARK----
  output$checkbox01 <- renderPrint({ 
    # this creates a variable vector of park_names that you can use to filter
    # the plot data before rendering
    input$park_name01  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
