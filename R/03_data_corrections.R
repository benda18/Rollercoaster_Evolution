# data_corrections

library(dplyr)
library(rvest)
library(glue)
library(lubridate)
#library(ggplot2)
library(xml2)
#library(igraph)
library(data.table)
library(janitor)
library(readr)

rm(list=ls());cat('\f');gc()

# FUNS ----
get_ride_years <- function(park.url = "https://rcdb.com/5056.htm"){
  Sys.sleep(3)
  park.html <- read_html(park.url)
  
  # get park name
  the.parkname <- html_element(x = park.html, 
                               xpath = "/html/body/section[1]/div[1]/div/div") %>%
    html_children() %>%
    as.character() %>%
    .[1] %>%
    gsub("<h1>|</h1>", "", .)
  
  # xpath - get all table headers and corresponding section numbers
  
  xp.body <- "/html/body"
  
  
  n_sections <- html_element(park.html, 
                             xpath = xp.body) %>%
    html_children() %>%
    html_name()  == "section"
  n_sections <- sum(n_sections)
  
  
  df.out <- NULL
  for(s in 1:n_sections){
    #print(paste("s = ", s, sep = ""))
    temp <- html_element(park.html, 
                         xpath = glue("/html/body/section[{s}]"))
    if(length(temp) >= 2){
      if(grepl("Roller Coaster", html_children(temp)[1])){
        # section name
        temp.table_name <- temp %>%
          html_children() %>%
          .[1] %>%
          as.character() %>%
          strsplit(., "\"") %>%
          unlist() %>%
          .[grepl("Roller", .)] %>%
          gsub("^<h4>|:.*$", "", .)
        
        # operating table----
        if(grepl("Operating", temp.table_name)){
          
          temp.table <- html_element(x = park.html, 
                                     xpath = glue("/html/body/section[{s}]")) %>%
            html_children() %>%
            .[2] %>%
            html_children()%>%
            html_children()%>%
            html_children() %>%
            .[2:length(.)]
          
            
          #out.df <- NULL  
          for(i in 1:length(temp.table)){
            #print(paste("i = ", i, sep = ""))
            # ride_url
            temp.ride_url <- temp.table[i] %>%
              html_children() %>%
              .[2] %>% 
              html_children() %>%
              xml2::xml_attr(., "href") 
            # ride_opened
            temp.Opened <- temp.table[i] %>%
              html_children() %>%
              .[6] %>%
              xml_attr(., "data-sort")
            
            # ride_closed
            temp.Closed <- NA
            
            # bind
            df.out <- rbind(df.out, 
                            data.frame(section_id = s, 
                                       section_name = temp.table_name,
                                       row_id     = i,
                                       ride_url = temp.ride_url,
                                       park_url = park.url,
                                       Opened = temp.Opened, 
                                       Closed = temp.Closed))
          }
          
        }
        
        # construction table----
        if(grepl("Under Construction", temp.table_name)){
          
          temp.table <- html_element(x = park.html, 
                                     xpath = glue("/html/body/section[{s}]")) %>%
            html_children() %>%
            .[2] %>%
            html_children()%>%
            html_children()%>%
            html_children() %>%
            .[2:length(.)]
          
          
          #out.df <- NULL  
          for(i in 1:length(temp.table)){
            #print(paste("i = ", i, sep = ""))
            # ride_url
            temp.ride_url <- temp.table[i] %>%
              html_children() %>%
              .[2] %>% 
              html_children() %>%
              xml2::xml_attr(., "href") 
            # ride_opened
            temp.Opened <- temp.table[i] %>%
              html_children() %>%
              .[6] %>%
              xml_attr(., "data-sort")
            
            # ride_closed
            temp.Closed <- NA
            
            # bind
            df.out <- rbind(df.out, 
                            data.frame(section_id = s, 
                                       section_name = temp.table_name,
                                       row_id     = i,
                                       ride_url = temp.ride_url,
                                       park_url = park.url,
                                       Opened = temp.Opened, 
                                       Closed = temp.Closed))
          }
          }
        
        # sbno table----
        if(grepl("SBNO", temp.table_name)){
          
          temp.table <- html_element(x = park.html, 
                                     xpath = glue("/html/body/section[{s}]")) %>%
            html_children() %>%
            html_children() %>%
            .[2] %>%  
            html_children() %>%
            html_children()%>%
            .[2:length(.)]
          
          
          #out.df <- NULL  
          for(i in 1:length(temp.table)){
            #print(paste("i = ", i, sep = ""))
            # ride_url
            temp.ride_url <- temp.table[i] %>%
              html_children() %>%
              .[2] %>% 
              html_children() %>%
              xml2::xml_attr(., "href") 
            # ride_opened
            temp.Opened <- temp.table[i] %>%
              html_children() %>%
              .[6] %>%
              xml_attr(., "data-sort")
            
            if(temp.Opened == "00000000"){
              temp.Opened <- temp.table[i] %>%
                html_children() %>%
                .[6] %>%
                html_children() %>%
                .[1] %>%
                xml_attr(., "datetime")
              
              if(nchar(temp.Opened) == 4){
                temp.Opened <- paste(temp.Opened, "1111", sep = "")
              }
              
            }
            
            # ride_closed
            temp.Closed <- NA
            
            # bind
            df.out <- rbind(df.out, 
                            data.frame(section_id = s, 
                                       section_name = temp.table_name,
                                       row_id     = i,
                                       ride_url = temp.ride_url,
                                       park_url = park.url, 
                                       Opened = temp.Opened, 
                                       Closed = temp.Closed))
          }
          }
        
        # defunct table----
        if(grepl("Defunct", temp.table_name)){
          
          temp.table <- html_element(x = park.html, 
                                     xpath = glue("/html/body/section[{s}]")) %>%
            html_children() %>%
            .[2] %>%
            html_children()%>%
            html_children()%>%
            html_children() %>%
            .[2:length(.)]
          
          
          #out.df <- NULL  
          for(i in 1:length(temp.table)){
            #print(paste("i = ", i, sep = ""))
            # ride_url
            temp.ride_url <- temp.table[i] %>%
              html_children() %>%
              .[2] %>% 
              html_children() %>%
              xml2::xml_attr(., "href") 
            # ride_opened
            temp.Opened <- temp.table[i] %>%
              html_children() %>%
              .[6] %>%
              xml_attr(., "data-sort")
            
            if(temp.Opened == "00000000"){
              temp.Opened <- temp.table[i] %>%
                html_children() %>%
                .[6] %>%
                html_children() %>%
                .[1] %>%
                xml_attr(., "datetime")
              
              try(if(nchar(temp.Opened) == 4){
                temp.Opened <- paste(temp.Opened, "1111", sep = "")
              })
              
            }
            
            # ride_closed
            temp.Closed <- temp.table[i] %>%
              html_children() %>%
              .[7] %>%
              xml_attr(., "data-sort")
            
            # bind
            df.out <- rbind(df.out, 
                            data.frame(section_id = s, 
                                       section_name = temp.table_name,
                                       row_id     = i,
                                       ride_url = temp.ride_url, 
                                       park_url = park.url,
                                       Opened = temp.Opened, 
                                       Closed = temp.Closed))
          }
          
        }
        
        # df.out <- rbind(df.out, 
        #                 temp.table)
        
      }
    }
  }
  
  # add park_url 
  df.out$park_url <- park.url
  
  # # fix ride_url 
  df.out$ride_url <-  paste("https://rcdb.com",
                            df.out$ride_url, sep = "")
  
  
  # simplify down to urls and dates
  df.out <- df.out[,c("ride_url", "park_url", "Opened", "Closed")]
  
  return(df.out)
}
clean_str <- function(v){
  
  out <- v %>%
    gsub("Batman - The", "Batman_The", .) %>%
    gsub("Superman - ", "Superman_", .) %>%
    gsub("Mini - Mine|Mini-Mine", "mini_mine", .) %>%
    gsub("-O-|-o-", "_O_", .) %>%
    gsub(" - ", "-", .) %>%
    gsub("Two-Face", "two_face", .) %>%
    gsub("Twist-N-Shout", "Twist_N_Shout", .)%>%
    gsub("X-Fli", "xfli", .) %>%
    gsub("XL-200", "xl200", .) %>%
    gsub("RC-48", "rc48", .) %>%
    gsub("TL.* Coaster", "TL3_coaster", .) %>%
    gsub("Sandy[[:punct:]]s ", "sandys", .) %>%
    gsub("Dale[[:punct:]]s ", "dales", .) %>%
    gsub("&", "and", .) %>%
    gsub(" / ", "_", .) %>%
    gsub(":|,|!|\\.|-|\'", "", .) %>%
    gsub(pattern = " ", 
         replacement = "_", 
         x = .) %>%
    tolower() %>%
    gsub("^d.*_vu$", "deja_vu", .) 
  return(out)
  
}



# DIRS ----
wd        <- list()
wd$data   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/data"
wd$R      <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/R"
wd$output <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/output"
wd$home   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution"


# park_inventory.csv fixes----
setwd(wd$data)

# load data
park_inventory <- read_csv("park_inventory.csv")



# missing data----

stop("xcelerator_the_ride  missing from knotts berry farm")

# change data



park_inventory[park_inventory$ride_name == "super_coaster" & park_inventory$park_name == "cedar_point",]$closed <- "19641111"
park_inventory[park_inventory$ride_name == "super_coaster" & park_inventory$park_name == "cedar_point",]$yrc_best <- 1964

park_inventory[park_inventory$ride_name == clean_str("Kiddie Coaster") & 
                 grepl(clean_str("Dorney Park"), park_inventory$park_name),]$closed <- "19801111"
park_inventory[park_inventory$ride_name == clean_str("Kiddie Coaster") & 
                 grepl(clean_str("Dorney Park"), park_inventory$park_name),]$yrc_best <- 1980

park_inventory[park_inventory$ride_name == clean_str("Roller Coaster") & 
                 grepl(clean_str("Adventureland"), park_inventory$park_name),]$opened    <- 19631111
park_inventory[park_inventory$ride_name == clean_str("Roller Coaster") & 
                 grepl(clean_str("Adventureland"), park_inventory$park_name),]$yro_best <- 1963
park_inventory[park_inventory$ride_name == clean_str("Roller Coaster") & 
                 grepl(clean_str("Adventureland"), park_inventory$park_name),]$closed    <- "19801111"
park_inventory[park_inventory$ride_name == clean_str("Roller Coaster") & 
                 grepl(clean_str("Adventureland"), park_inventory$park_name),]$yrc_best <- 1980

park_inventory[park_inventory$ride_name == clean_str("Tater Bug Terror") & 
                 grepl(clean_str("Adventurers Family"), park_inventory$park_name),]$opened <- 20021111
park_inventory[park_inventory$ride_name == clean_str("Tater Bug Terror") & 
                 grepl(clean_str("Adventurers Family"), park_inventory$park_name),]$yro_best <- 2002

park_inventory[park_inventory$ride_name == clean_str("Flash") & 
                 grepl(clean_str("Adventurers Family"), park_inventory$park_name),]$opened <- 19821111
park_inventory[park_inventory$ride_name == clean_str("Flash") & 
                 grepl(clean_str("Adventurers Family"), park_inventory$park_name),]$yro_best <- 1982

park_inventory[park_inventory$ride_name == clean_str("Kiddie Coaster") & 
                 grepl(clean_str("Adventurers Family"), park_inventory$park_name),]$closed <- "20021111"
park_inventory[park_inventory$ride_name == clean_str("Kiddie Coaster") & 
                 grepl(clean_str("Adventurers Family"), park_inventory$park_name),]$yrc_best <- 2002
park_inventory[park_inventory$ride_name == clean_str("Kiddie Coaster") & 
                 grepl(clean_str("Adventurers Family"), park_inventory$park_name),]$opened <- 19661111
park_inventory[park_inventory$ride_name == clean_str("Kiddie Coaster") & 
                 grepl(clean_str("Adventurers Family"), park_inventory$park_name),]$yro_best <- 1966

# write data
# write_csv(park_inventory, 
#           file = "park_inventory.csv")


# # ride_specs.csv tidying and fixes----
# setwd(wd$data)
# ride_specs <- read_csv("ride_specs.csv")
# 
# 
# ride_specs$length.ft <- gsub(" ft$", "", ride_specs$Length) %>%
#   as.numeric()
# ride_specs$height.ft <- gsub(" ft$", "", ride_specs$Height) %>%
#   as.numeric()
# ride_specs$speed.mph <- gsub(" mph$", "", ride_specs$Speed) %>%
#   as.numeric()
# 
# write_csv(x = ride_specs, file = "ride_specs.csv")
