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
library(glue)
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
                                           label = ("Separately (up to 16 parks)"), 
                                           value = F
                             ),
                            
                             # ui.R RADIO BUTTIONS
                             radioButtons("radio", 
                                          label = h4(HTML(r"(<u>Rides as a Number or Percentage</u>)")),
                                          choices = list("Number of Rides" = "stack", 
                                                         "Percent of Rides" = "fill"), 
                                          selected = "stack"
                             ), 
                             # Multiple selectInput----
                             selectInput(inputId  = "park_name01", #"Columns",
                                         label    = h2(HTML(r"(<u>Select Parks</u>)")), #"Columns",
                                         choices  = park.names.list, #names(mtcars), 
                                         #selected = c("kings_island"),
                                         #selected = ref.park.names$park_name[ref.park.names$park_operator == "cedar_fair"],
                                         selected = ref.park.names$park_name[grepl(pattern = "^kings_|^carowinds", 
                                                                                   x = ref.park.names$park_name)],
                                         multiple = TRUE)
                ),
                
                # mainPanel----
                mainPanel("", 
                          fluidRow(
                            plotOutput(outputId = "plot01", 
                                       height = plot.height)
                          ), 
                          fluidRow(
                            column(6,plotOutput(outputId = "plot02", 
                                                height = plot.height, 
                                                width = "50%")), 
                            column(6, plotOutput(outputId = "plot03", 
                                               height = plot.height, 
                                               width = "50%")))
                ),
                # Future Table placement VVV
                fluidRow(#tableOutput(outputId = "table04")
                  #tableOutput(outputId = "table01"),
                  #tableOutput(outputId = "table02"), 
                  #tableOutput(outputId = "table03"),
                ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # checkbox for facet_grid/facet_wrap----
  output$valueFacet <- renderPrint({ 
    input$checkbox_f 
  })
  # # slider for plot text size (hold for future use)
  # output$valueS <- renderPrint({
  #   input$slider_ts
  # })
  # server.R RADIO BUTTON TO SELECT PLOT LAYOUT
  output$valueR <- renderPrint({ 
    input$radio 
  })
  
  # Render tables----
  # output$table01 <- renderTable({
  #   # SHINY_avg.length_by.design_by.yr %>%
  #   #   data.table::as.data.table() %>%
  #   #   dcast(., 
  #   #         year ~ design, 
  #   #         fun.aggregate = sum, 
  #   #         value.var = "avg_length", 
  #   #         fill = 0) %>%
  #   #   as.data.frame() %>%
  #   #   mutate(., 
  #   #          year = as.integer(year))
  #   
  # })
  output$plot01 <- renderPlot({
    the.plot.01 <- SHINY_ride.design_by.year_by.park[SHINY_ride.design_by.year_by.park$park_name %in% 
                                                       input$park_name01,] %>%
      left_join(., ref.park.names[,c("park_name", "Park_Name.facet")]) %>%
      ggplot(data = ., 
             aes(x = year, y = n_rides, 
                 fill = design_f)) + 
      labs(title = "Ride-Design by Park by Year", 
           subtitle = glue("Selected Parks in the United States, {min(SHINY_ride.design_by.year_by.park[SHINY_ride.design_by.year_by.park$park_name %in% 
                                                       input$park_name01,]$year)}-Present"), 
           caption = "Source: rcdb.com")+
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
        facet_grid(Park_Name.facet~.)
    } 
    if(input$checkbox_f & 
       length(input$park_name01) > 4 & 
       length(input$park_name01) <= 16){
      the.plot.01 <- the.plot.01 + 
        facet_wrap(Park_Name.facet~.)
    }
    
    print(the.plot.01)
    
  }, 
  # control plot size----
  height = plot.height, 
  width = plot.width)
  
  output$plot02 <- renderPlot({
    the.plot.02 <- ungroup(summarise(group_by(SHINY_park_inventory[SHINY_park_inventory$park_name %in% 
                                                      input$park_name01,], 
                               park_url, park_name, ride_url, ride_url_f,
                               ride_name, ride_status,
                               type, type_f,
                               design,design_f,
                               scale, scale_f,
                               yro_best, yrc_best))) %>%
      left_join(., 
                ref.park.names[,c("park_name", "Park_Name.facet", "Park_Name")]) %>%
      ggplot(data = ., 
             aes(color = type_f)) + 
      geom_segment(aes(y = ride_url_f, yend = ride_url_f, 
                       x = yro_best, xend = yrc_best))+
      geom_point(aes(x = yro_best, y = ride_url_f))+
      geom_point(aes(x = yrc_best, y = ride_url_f))+
      scale_y_discrete(name = "Ride Name", 
                       breaks = SHINY_park_inventory[SHINY_park_inventory$park_name %in% 
                                                       input$park_name01,]$ride_url_f,
                       labels = SHINY_park_inventory[SHINY_park_inventory$park_name %in% 
                                                       input$park_name01,]$ride_name)+
      theme(text = element_text(size = text.size),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "vertical",
            plot.background = element_rect(color = "black"))+
      labs(title = "Park Rides by Years Opened-Closed by Build Material")+
      facet_wrap(~Park_Name.facet, scales = "free_y")+
      scale_color_discrete(name = "Build Material")+
      scale_x_continuous(name = "Year")
    
    if(length(input$park_name01) == 1){
      print(the.plot.02)
    }else{
      print(ggplot() + 
              labs(title = "\n    <Too Many Parks Selected>\n    (Plots just 1)")+
              theme_minimal()+
              theme(text = element_text(size = text.size, color = "red"),
                    legend.position = "bottom",
                    legend.direction = "horizontal",
                    legend.box = "vertical",
                    plot.background = element_rect(color = "black")))
    }
    
    
  }, 
  height = plot.height, 
  width = plot.width/2)
  
  output$plot03 <- renderPlot({
    the.plot.02 <- ggplot() +
      theme_minimal()+
      theme(text = element_text(size = text.size, color = "red"),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "vertical",
            plot.background = element_rect(color = "black"))
    
    if(length(input$park_name01) == 1){
      print(the.plot.02)
    }else{
      print(ggplot() + 
              labs(title = "\n    <Too Many Parks Selected>\n    (Plots just 1)")+
              theme_minimal()+
              theme(text = element_text(size = text.size, color = "red"),
                    legend.position = "bottom",
                    legend.direction = "horizontal",
                    legend.box = "vertical",
                    plot.background = element_rect(color = "black")))
    }
    
    
  }, 
  height = plot.height, 
  width = plot.width/2)
  
  output$table03 <- renderTable({
    SHINY_avg.length_by.design_by.yr
  })
  # output$table04 <- renderTable({
  #   SHINY_ride.specs_by.year %>%
  #     mutate(., 
  #            year = as.integer(year), 
  #            n_parks = as.integer(n_parks))
  # })
  
  
  # CHECKBOX TO SELECT THEMEPARK----
  output$checkbox01 <- renderPrint({ 
    # this creates a variable vector of park_names that you can use to filter
    # the plot data before rendering
    input$park_name01  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

