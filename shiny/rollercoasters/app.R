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
ui <- fluidPage(headerPanel(""),
                titlePanel(""), 
                
                sidebarPanel(h4(HTML(r"(<u>Plot Parks Together or Separate</u>)")), 
                             checkboxInput("checkbox_f", 
                                           label = ("Separately (up to 4 parks)"), 
                                           value = F),
                             # ui.R Radio2 Buttons
                             radioButtons("radio2", 
                                          label = h4(HTML(r"(<u>Outline Parks:</u>)")), 
                                          choices = list("Don't color" = "no color", 
                                                         "All the same color" = "same color", 
                                                         "Each with unique color" = "different colors"), 
                                          selected = "no color"),
                             # #ui  slider
                             # sliderInput("slider_ts", 
                             #             label = h4(HTML(r"(<u>Adjust Text Size</u>)")), 
                             #             min = 10, 
                             #             max = 30, 
                             #             value = 18),
                             # ui.R RADIO BUTTIONS
                             radioButtons("radio", 
                                          label = h4(HTML(r"(<u>Rides as a Number or Percentage</u>)")),
                                          choices = list("Number of Rides" = "stack", 
                                                         "Percent of Rides" = "fill"), 
                                          selected = "stack"), 
                             # ui.R CHECKBOX GROUP----
                             checkboxGroupInput("park_name01", 
                                                label = h2(HTML(r"(<u>Select Parks</u>)")), 
                                                choices = park.names.list,
                                                selected = c("kings_island"))),
                mainPanel("", 
                          fluidRow(
                            plotOutput(outputId = "plot01", 
                                       height = plot.height)
                            ), 
                          fluidRow(
                            plotOutput(outputId = "plot02", 
                                       height = plot.height)),
                          ),
                
                fluidRow(tableOutput(outputId = "table04")
                          #tableOutput(outputId = "table01"),
                          
                          #tableOutput(outputId = "table02"), 
                          #tableOutput(outputId = "table03"),
                          
                          ),
                
                # hr(),
                # fluidRow(column(3, verbatimTextOutput("valueFacet"))),
                
                
                
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
  # checkbox for facet_grid/facet_wrap----
  output$valueFacet <- renderPrint({ 
    input$checkbox_f 
    })
  # # slider for plot text size
  # output$valueS <- renderPrint({
  #   input$slider_ts
  # })
  # radio button for coloring parks in plot
  output$valueR2 <- renderPrint({
    input$radio2
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
    # SHINY_ride.design_by.year_by.park[SHINY_ride.design_by.year_by.park$park_name %in% 
    #                                     input$park_name01,] %>%
      the.plot.01 <- ggplot(data = SHINY_ride.design_by.year_by.park[SHINY_ride.design_by.year_by.park$park_name %in% input$park_name01,], 
             aes(x = year, y = n_rides, 
                 fill = design)) + 
      geom_col(position = input$radio) +
      #theme(text = element_text(size = 25))+
      theme(text = element_text(size = text.size), #input$slider_ts), 
            legend.position = "bottom", 
            legend.direction = "horizontal", 
            legend.box = "vertical", 
            #panel.border = element_rect(color = "blue"),
            plot.background = element_rect(color = "black"))+
      scale_y_continuous(name = ifelse(input$radio == "fill", "Percent-share of Rides", "Number of Rides"), 
                         labels = ifelse(input$radio == "fill", 
                                   scales::percent, scales::comma))+
      scale_fill_discrete(name = "Ride Design")+
        scale_color_discrete(name = "Park Name")
      
      # add facets here----
     
      if(input$checkbox_f & 
         length(input$park_name01) > 1 & 
         length(input$park_name01) <= 4){
        the.plot.01 <- the.plot.01 + 
          facet_grid(park_name~.)
      } 
      
      # if(input$radio2 == "same color") {
      #   the.plot.01 <- the.plot.01 +
      #     geom_col(position = input$radio, 
      #              color = "white") 
      # }
      # 
      # if(input$radio2 == "different colors") {
      #   the.plot.01 <- the.plot.01 +
      #     geom_col(position = input$radio, 
      #              aes(color = park_name)) 
      # }
      # 
      # if(input$radio2 == "no color"){
      #   the.plot.01 <- the.plot.01 +
      #     geom_col(position = input$radio)
      # }
      
      
      
    print(the.plot.01)
    
  }, 
  # control plot size----
  height = plot.height, 
  width = plot.width)
  
  output$plot02 <- renderPlot({
  ggplot() + 
      theme_dark()+
      theme(text = element_text(size = text.size),
            plot.background = element_rect(color = "black"))+
      labs(title = "output$plot02")
    }, 
  height = plot.height, 
  width = plot.width)
  
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
