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
ui <- fluidPage(headerPanel("Roller Coasters of the United States"),
                titlePanel("Selected Features of past and present rides by Park"), 
                #verticalLayout(sidebarPanel(shiny::wellPanel("foo")),
                sidebarPanel(h3(HTML(r"(<b><u>Selection Filters</u></b>)")),
                  # Multiple selectInput----
                             selectInput(inputId  = "park_name01", #"Columns",
                                         label    = h4(HTML(r"(<u>Select Parks</u>)")), #"Columns",
                                         choices  = park.names.list, #names(mtcars), 
                                         #selected = c("kings_island"),
                                         #selected = ref.park.names$park_name[ref.park.names$park_operator == "cedar_fair"],
                                         selected = ref.park.names$park_name[grepl(pattern = "^kings_|^carowinds", 
                                                                                   x = ref.park.names$park_name)],
                                         multiple = TRUE), 
                             h4(HTML(r"(<u>Plot Parks Together or Separate</u>)")), 
                             checkboxInput("checkbox_f", 
                                           label = ("Separately (up to 16 parks)"), 
                                           value = T
                             ),
                            
                             # ui.R RADIO BUTTIONS
                             radioButtons("radio", 
                                          label = h4(HTML(r"(<u>Rides as a Number or Percentage</u>)")),
                                          choices = list("Number of Rides" = "stack", 
                                                         "Percent of Rides" = "fill"), 
                                          selected = "stack"
                             ), 
                  h3(HTML(r"(<b><u>Legend</u></b><br><h4>Ride Design Examples</h4><br>)")),
                  wellPanel(HTML(r"(* wing:&#9<a title="Jeremy Thompson from United States of America, CC BY 2.0 &lt;https://creativecommons.org/licenses/by/2.0&gt;, via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File:GateKeeper_020_(9547680779).jpg"><img width="90" alt="GateKeeper 020 (9547680779)\" src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/15/GateKeeper_020_%289547680779%29.jpg/90px-GateKeeper_020_%289547680779%29.jpg"></a>)")),
                  wellPanel((HTML(r"(* flying:&#9 <a title="Jeremy Thompson from United States of America, CC BY 2.0 &lt;https://creativecommons.org/licenses/by/2.0&gt;, via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File:Firehawk_horseshoe_element.jpg"><img width="90" alt="Firehawk horseshoe element" src="https://upload.wikimedia.org/wikipedia/commons/thumb/7/7b/Firehawk_horseshoe_element.jpg/90px-Firehawk_horseshoe_element.jpg"></a><br>)"))),
                  wellPanel((HTML(r"(* sit-down:&#9 <a title="Eli Duke, CC BY-SA 2.0 &lt;https://creativecommons.org/licenses/by-sa/2.0&gt;, via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File:Cincinnati_Kings_Island.jpg"><img width="90" alt="Cincinnati Kings Island" src="https://upload.wikimedia.org/wikipedia/commons/thumb/2/29/Cincinnati_Kings_Island.jpg/90px-Cincinnati_Kings_Island.jpg"></a><br>)"))),
                  wellPanel((HTML(r"(* suspended:&#9 <a title="Chris Hagerman, CC BY-SA 3.0 &lt;http://creativecommons.org/licenses/by-sa/3.0/&gt;, via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File:PKI-Top_Gun.jpg"><img width="90" alt="PKI-Top Gun" src="https://upload.wikimedia.org/wikipedia/commons/thumb/c/c7/PKI-Top_Gun.jpg/90px-PKI-Top_Gun.jpg"></a><br>)"))),
                  wellPanel((HTML(r"(* inverted:&#9 <a title="uploader (User:Breakdancer), CC BY-SA 3.0 &lt;https://creativecommons.org/licenses/by-sa/3.0&gt;, via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File:Black_mamba_first_drop.jpg"><img width="90" alt="Black mamba first drop" src="https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Black_mamba_first_drop.jpg/90px-Black_mamba_first_drop.jpg"></a><br>)"))),
                  wellPanel((HTML(r"(* stand-up:&#9 <a title="Joekid, CC BY-SA 2.5 &lt;https://creativecommons.org/licenses/by-sa/2.5&gt;, via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File:King_Cobra_(Kings_Island).jpg"><img width="90" alt="King Cobra (Kings Island)\" src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/0c/King_Cobra_%28Kings_Island%29.jpg/90px-King_Cobra_%28Kings_Island%29.jpg"></a><br>)"))),
                  wellPanel(HTML(r"(* pipeline:&#9 <a title="Chris Hagerman from New Port Richey, FL, US, CC BY 2.0 &lt;https://creativecommons.org/licenses/by/2.0&gt;, via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File:Ultra_Twister.jpg"><img width="90" alt="Ultra Twister" src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Ultra_Twister.jpg/90px-Ultra_Twister.jpg"></a><br>)")),
                  wellPanel((HTML(r"(* bobsled:&#9 <a title="WillMcC at English Wikipedia, CC BY-SA 3.0 &lt;http://creativecommons.org/licenses/by-sa/3.0/&gt;, via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File:BPBbobsled.jpg"><img width="90" alt="BPBbobsled" src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/0b/BPBbobsled.jpg/90px-BPBbobsled.jpg"></a>)")))
                ),
                
                
                # mainPanel----
                mainPanel(wellPanel(
                          fluidRow(
                            plotOutput(outputId = "plot01", 
                                       height = plot.height)), 
                          fluidRow(
                            column(6,plotOutput(outputId = "plot02b", #"plot02", 
                                                height = plot.height, 
                                                width = "50%")), 
                            column(6, plotOutput(outputId = "plot03", 
                                               height = plot.height, 
                                               width = "50%"))),
                          fluidRow(wellPanel(HTML(r'(What the App Shows)'))),
                          fluidRow(wellPanel(HTML(r'(what the problem was)'))),
                          fluidRow(wellPanel(HTML(r'(how this app solved the problem)'))),
                          fluidRow(wellPanel(HTML(r'(www.rcdb.org)')))
                ))
                # Future Table placement VVV
                
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
            legend.position = "right", 
            legend.direction = "vertical", 
            legend.box = "vertical", 
            axis.text.x = element_text(angle = 45, hjust =1, vjust = 1),
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
  
  
  output$plot02b <- renderPlot({
    setwd(wd$output)
    shiny_rh <- read_csv("SHINY_ride_heights.csv")
    setwd(wd$shiny)
    
    shiny_rh <- left_join(shiny_rh,
              ref.park.names[,c("park_name", "Park_Name", "Park_Name.facet")])
    
    # ride_height as factor
    shiny_rh$ride_height_f <- factor(shiny_rh$ride_height)
    shiny_rh$ride_height_o <- shiny_rh$ride_height_f %>%
      as.ordered()
    
    # import other necessary data
    setwd(wd$data)
    files_wd.data <- paste(getwd(),list.files(),sep = "/")
    setwd(wd$output)
    files_wd.output <- paste(getwd(),list.files(),sep = "/")
    setwd(wd$shiny)
    
    #ride_specs <- read_csv(file = files_wd.data[grepl("ride_specs", files_wd.data)])
    park_inventory <- read_csv(file = files_wd.data[grepl("park_inventory", files_wd.data)])
    
    # join
    shiny_rh <- left_join(shiny_rh,park_inventory[,c("ride_url", "type", "design", "scale")]) 
    
    shiny_rh$ride_url_f <- factor(shiny_rh$ride_url, 
                                  levels = unique(shiny_rh$ride_url[order(shiny_rh$ride_height,
                                                                          decreasing = F)]))
    the.plot.02b <- shiny_rh %>%
      .[.$park_name %in% input$park_name01,] %>%
      group_by(park_name, ride_height_o, ride_height_f) %>%
      summarise(n_rides = n_distinct(ride_url)) %>%
      ggplot(data = .) +
      # geom_col(aes(y = park_name, 
      #              x = n_rides,
      #              fill = ride_height_o), 
      #          color = "white",
      #          position = ifelse(input$radio == "fill", "fill", "stack"))+
      #facet_wrap(park_name~., scales = "free_y")+ #, space = "free_y")+
      scale_x_continuous(name = ifelse(input$radio == "fill", "Percent-share of Rides", "Number of Rides"), 
                         n.breaks = 10,
                         #breaks = seq(0,100,by=1),
                         #breaks = ifelse(input$radio == "fill", seq(0,1,by=0.1), seq(0,100,by=1)),
                         labels = ifelse(input$radio == "fill",  
                                         scales::percent, scales::comma))+
      scale_y_discrete(name = "Park Name", 
                       breaks = shiny_rh[shiny_rh$park_name %in% 
                                                       input$park_name01,]$park_name,
                       labels = shiny_rh[shiny_rh$park_name %in% 
                                                       input$park_name01,]$Park_Name.facet) +
      # scale_fill_ordinal(name = "Minimum Rider\nHeight (inches)", direction = 1)+
      scale_fill_viridis_d(name = "Minimum Rider\nHeight (inches)")+
      #scale_fill_continuous()+
      theme(legend.position = "right",
            legend.direction = "vertical",
            legend.box = "vertical",
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            text = element_text(size = text.size),
            plot.background = element_rect(color = "black", fill = NULL))+
      labs(title = ifelse(input$radio == "fill", "Percent of Rides by Minimum Rider Height\nby Park", 
                          "Number of Rides by Minimum Rider Height\nby Park"), 
           caption = "Source: rcdb.com")
    
    if(input$checkbox_f){
      the.plot.02b <- the.plot.02b + 
        geom_col(aes(y = park_name, 
                     x = n_rides,
                     fill = ride_height_o), 
                 color = "white",
                 position = ifelse(input$radio == "fill", "fill", "stack"))
    }else{
      the.plot.02b <- the.plot.02b + 
        geom_col(aes(y = "all parks", 
                     x = n_rides,
                     fill = ride_height_o), 
                 color = "white",
                 position = ifelse(input$radio == "fill", "fill", "stack"))
    }
    
    
    # print
    print(the.plot.02b)
    
  }, 
  height = plot.height, 
  width = plot.width/2)
  
  # output$plot02 <- renderPlot({
  #   the.plot.02 <- ungroup(summarise(group_by(SHINY_park_inventory[SHINY_park_inventory$park_name %in% 
  #                                                     input$park_name01,], 
  #                              park_url, park_name, ride_url, ride_url_f,
  #                              ride_name, ride_status,
  #                              type, type_f,
  #                              design,design_f,
  #                              scale, scale_f,
  #                              yro_best, yrc_best))) %>%
  #     left_join(., 
  #               ref.park.names[,c("park_name", "Park_Name.facet", "Park_Name")]) %>%
  #     ggplot(data = ., 
  #            aes(color = type_f)) + 
  #     geom_segment(aes(y = ride_url_f, yend = ride_url_f, 
  #                      x = yro_best, xend = yrc_best))+
  #     geom_point(aes(x = yro_best, y = ride_url_f))+
  #     geom_point(aes(x = yrc_best, y = ride_url_f))+
  #     scale_y_discrete(name = "Ride Name", 
  #                      breaks = SHINY_park_inventory[SHINY_park_inventory$park_name %in% 
  #                                                      input$park_name01,]$ride_url_f,
  #                      labels = SHINY_park_inventory[SHINY_park_inventory$park_name %in% 
  #                                                      input$park_name01,]$ride_name)+
  #     theme(text = element_text(size = text.size),
  #           legend.position = "bottom",
  #           legend.direction = "horizontal",
  #           legend.box = "vertical",
  #           plot.background = element_rect(color = "black"))+
  #     labs(title = "Park Rides by Years Opened-Closed by Build Material")+
  #     facet_wrap(~Park_Name.facet, scales = "free_y")+
  #     scale_color_discrete(name = "Build Material")+
  #     scale_x_continuous(name = "Year")
  #   
  #   if(length(input$park_name01) == 1){
  #     print(the.plot.02)
  #   }else{
  #     print(ggplot() + 
  #             labs(title = "\n    <Too Many Parks Selected>\n    (Plots just 1)")+
  #             theme_minimal()+
  #             theme(text = element_text(size = text.size, color = "red"),
  #                   legend.position = "bottom",
  #                   legend.direction = "horizontal",
  #                   legend.box = "vertical",
  #                   plot.background = element_rect(color = "black")))
  #   }
  #   
  #   
  # }, 
  # height = plot.height, 
  # width = plot.width/2)
  
  output$plot03 <- renderPlot({
    
    the.plot.03 <- yearly.specs %>%
      left_join(., 
                ungroup(summarise(group_by(SHINY_park_inventory, ride_url, park_name))), 
                by = c("ride_url_f" = "ride_url")) %>%
      .[.$park_name %in% input$park_name01,] %>%
      #.[.$park_name %in% c("kings_island", "carowinds"),] %>%
      .[!duplicated(.),] %>%
      as.data.table() %>%
      melt(., 
           measure.vars = c("length.ft", "height.ft", "speed.mph")) %>%
      as.data.frame() %>%
      as_tibble() %>%
      group_by(year_active, variable) %>%
      slice_max(order_by = value, n = 1) %>% 
      ggplot(data = ., 
             aes(x = year_active, y = value)) +
      #geom_col(aes(fill = Park_Name), position = "dodge")+
      #geom_col(aes(fill = ride_name), position = "dodge")+
      facet_grid(variable~., scales = "free_y")+
      scale_y_continuous(name = NULL, 
                         labels = scales::comma)+
      theme(text = element_text(size = text.size),
            legend.position = "right",
            legend.direction = "vertical",
            axis.text.x = element_text(angle = 45, 
                                       hjust = 1, 
                                       vjust = 1),
            #legend.box = "vertical",
            plot.background = element_rect(color = "black"))+
      #facet_wrap(~Park_Name.facet, scales = "free_y")+
      scale_fill_discrete(name = "Park Name")+
      scale_x_continuous(name = "Year")
      
      if(length(input$park_name01) > 1){
        # by_park
        the.plot.03 <- the.plot.03 +
          geom_col(aes(fill = park_name), position = "dodge")+
          labs(title = "Tallest, Longest & Fastest Park by Year", 
               caption = "Source: rcdb.com")
          
      }else{
        # by_ride
        the.plot.03 <- the.plot.03 +
          geom_col(aes(fill = ride_name), position = "dodge")+
          labs(title = "Tallest, Longest & Fastest Ride by Year", 
               caption = "Source: rcdb.com")
      }
    
    print(the.plot.03)
    # if(length(input$park_name01) <=5){
    #   print(the.plot.03)
    # }else{
    #   print(ggplot() + 
    #           labs(title = "\n    <Too Many Parks Selected>\n    (Plots just 1)")+
    #           theme_minimal()+
    #           theme(text = element_text(size = text.size, color = "red"),
    #                 legend.position = "bottom",
    #                 legend.direction = "horizontal",
    #                 legend.box = "vertical",
    #                 plot.background = element_rect(color = "black")))
    # }
    
    
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

