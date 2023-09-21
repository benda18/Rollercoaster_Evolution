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
get_ride_years <- function(park.url="https://rcdb.com/4717.htm"){
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
                                       Opened = temp.Opened, 
                                       Closed = temp.Closed))
          }
          
        }
        
        # construction table----
        if(grepl("Under Construction", temp.table_name)){
          
          # temp.table <- html_element(x = park.html, 
          #                            xpath = glue("/html/body/section[{s}]")) %>%
          #   rvest::html_table(., convert = T, trim = T) %>%
          #   .[,2:6] %>%
          #   mutate(., 
          #          Opened = NA_character_, 
          #          Closed = NA_character_,
          #          #Opened2 = NA_character_,
          #          yr_opened = NA_real_, 
          #          yr_closed = NA_real_, 
          #          park_name = the.parkname, 
          #          ride_status = temp.table_name, 
          #          ride_url = NA)
          # 
          # for(i in 1:nrow(temp.table)){
          #   temp.ride_url <- html_element(x = park.html, 
          #                                 xpath = glue("/html/body/section[{s}]/div/table")) %>%
          #     html_children() %>%
          #     .[2] %>%  
          #     html_children() %>%
          #     .[i] %>%  
          #     html_children() %>%
          #     .[2] %>%
          #     html_element(., xpath = "a") %>%
          #     .[[1]] %>%
          #     xml2::xml_attrs() 
          #   temp.date <- html_element(x = park.html, 
          #                             xpath = glue("/html/body/section[{s}]/div/table")) %>%
          #     html_children() %>%
          #     .[2] %>%  # 2 = "body"
          #     html_children() %>%
          #     .[i] %>%  # 1 = 1st row of rides
          #     html_children() %>%
          #     as.character() %>%
          #     .[6] %>% # represents columns of table (i.e. "i")
          #     strsplit(., "\"") %>%
          #     unlist() 
          #   
          #   temp.table$Opened[i] <- temp.date[2]
          #   #df.ex_rides$Opened2[i] <- temp.date[4]
          #   temp.table$yr_opened[i] <- as.numeric(temp.date[2]) %/% 10000
          #   temp.table$ride_url[i] <- temp.ride_url
          # }
          # temp.table <- temp.table[!colnames(temp.table) %in% c("Opening")]
          
          
        }
        # sbno table----
        if(grepl("SBNO", temp.table_name)){
          
          # temp.table <- html_element(x = park.html, 
          #                            xpath = glue("/html/body/section[{s}]")) %>%
          #   rvest::html_table(., convert = T, trim = T) %>%
          #   .[,2:6] %>%
          #   mutate(., 
          #          Opened = NA_character_, 
          #          Closed = NA_character_,
          #          #Opened2 = NA_character_,
          #          yr_opened = NA_real_, 
          #          yr_closed = NA_real_, 
          #          park_name = the.parkname, 
          #          ride_status = temp.table_name, 
          #          ride_url = NA)
          # 
          # for(i in 1:nrow(temp.table)){
          #   temp.ride_url <- html_element(x = park.html, 
          #                                 xpath = glue("/html/body/section[{s}]/div/table")) %>%
          #     html_children() %>%
          #     .[2] %>%  
          #     html_children() %>%
          #     .[i] %>%  
          #     html_children() %>%
          #     .[2] %>%
          #     html_element(., xpath = "a") %>%
          #     .[[1]] %>%
          #     xml2::xml_attrs() 
          #   temp.date <- html_element(x = park.html, 
          #                             xpath = glue("/html/body/section[{s}]/div/table")) %>%
          #     html_children() %>%
          #     .[2] %>%  # 2 = "body"
          #     html_children() %>%
          #     .[i] %>%  # 1 = 1st row of rides
          #     html_children() %>%
          #     as.character() %>%
          #     .[6] %>% # represents columns of table (i.e. "i")
          #     strsplit(., "\"") %>%
          #     unlist() 
          #   
          #   temp.table$Opened[i] <- temp.date[2]
          #   #df.ex_rides$Opened2[i] <- temp.date[4]
          #   temp.table$yr_opened[i] <- as.numeric(temp.date[2]) %/% 10000
          #   temp.table$ride_url[i] <- temp.ride_url
          # }
          # temp.table <- temp.table[!colnames(temp.table) %in% c("Since")]
        }
        
        # defunct table----
        if(grepl("Defunct", temp.table_name)){
          
          
          # temp.table <- html_element(x = park.html, 
          #                            xpath = glue("/html/body/section[{s}]")) %>%
          #   rvest::html_table(., convert = T, trim = T) %>%
          #   .[,2:7] %>%
          #   mutate(., 
          #          Opened = NA_character_, 
          #          Closed = NA_character_,
          #          #Opened2 = NA_character_,
          #          yr_opened = NA_real_, 
          #          yr_closed = NA_real_, 
          #          park_name = the.parkname, 
          #          ride_status = temp.table_name, 
          #          ride_url = NA)
          # 
          # for(i in 1:nrow(temp.table)){
          #   temp.ride_url <- html_element(x = park.html, 
          #                                 xpath = glue("/html/body/section[{s}]/div/table")) %>%
          #     html_children() %>%
          #     .[2] %>%  
          #     html_children() %>%
          #     .[i] %>%  
          #     html_children() %>%
          #     .[2] %>%
          #     html_element(., xpath = "a") %>%
          #     .[[1]] %>%
          #     xml2::xml_attrs() 
          #   # opening date
          #   temp.date <- html_element(x = park.html, 
          #                             xpath = glue("/html/body/section[{s}]/div/table")) %>%
          #     html_children() %>%
          #     .[2] %>%  # 2 = "body"
          #     html_children() %>%
          #     .[i] %>%  # 1 = 1st row of rides
          #     html_children() %>%
          #     as.character() %>%
          #     .[6] %>% # represents columns of table (i.e. "i")
          #     strsplit(., "\"") %>%
          #     unlist() 
          #   
          #   temp.table$Opened[i] <- temp.date[2]
          #   temp.table$yr_opened[i] <- as.numeric(temp.date[2]) %/% 10000
          #   
          #   # closed date
          #   temp.date <- html_element(x = park.html, 
          #                             xpath = glue("/html/body/section[{s}]/div/table")) %>%
          #     html_children() %>%
          #     .[2] %>%  # 2 = "body"
          #     html_children() %>%
          #     .[i] %>%  # 1 = 1st row of rides
          #     html_children() %>%
          #     as.character() %>%
          #     .[7] %>% # represents columns of table (i.e. "i")
          #     strsplit(., "\"") %>%
          #     unlist() 
          #   
          #   temp.table$Closed[i] <- temp.date[2]
          #   #df.ex_rides$Opened2[i] <- temp.date[4]
          #   temp.table$yr_closed[i] <- as.numeric(temp.date[2]) %/% 10000
          #   temp.table$ride_url[i] <- temp.ride_url
          # }
          # temp.table
          
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
  
  return(df.out)
}

get_ride_years()

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

# change data


park_inventory[park_inventory$Name == "Super Coaster" & park_inventory$park_name == "Cedar Point",]$Closed <- "19641111"
park_inventory[park_inventory$Name == "Super Coaster" & park_inventory$park_name == "Cedar Point",]$yr_closed <- 1964

park_inventory[park_inventory$Name == "Kiddie Coaster" & grepl("Dorney Park", park_inventory$park_name),]$Closed <- "19801111"
park_inventory[park_inventory$Name == "Kiddie Coaster" & grepl("Dorney Park", park_inventory$park_name),]$yr_closed <- 1980

park_inventory[park_inventory$Name == "Roller Coaster" & grepl("Adventureland", park_inventory$park_name),]$Opened    <- "19631111"
park_inventory[park_inventory$Name == "Roller Coaster" & grepl("Adventureland", park_inventory$park_name),]$yr_opened <- 1963
park_inventory[park_inventory$Name == "Roller Coaster" & grepl("Adventureland", park_inventory$park_name),]$Closed    <- "19801111"
park_inventory[park_inventory$Name == "Roller Coaster" & grepl("Adventureland", park_inventory$park_name),]$yr_closed <- 1980

park_inventory[park_inventory$Name == "Tater Bug Terror" & grepl("Adventurers Family", park_inventory$park_name),]$Opened <- "20021111"
park_inventory[park_inventory$Name == "Tater Bug Terror" & grepl("Adventurers Family", park_inventory$park_name),]$yr_opened <- 2002

park_inventory[park_inventory$Name == "Flash" & grepl("Adventurers Family", park_inventory$park_name),]$Opened <- "19821111"
park_inventory[park_inventory$Name == "Flash" & grepl("Adventurers Family", park_inventory$park_name),]$yr_opened <- 1982

park_inventory[park_inventory$Name == "Kiddie Coaster" & grepl("Adventurers Family", park_inventory$park_name),]$Closed <- "20021111"
park_inventory[park_inventory$Name == "Kiddie Coaster" & grepl("Adventurers Family", park_inventory$park_name),]$yr_closed <- 2002
park_inventory[park_inventory$Name == "Kiddie Coaster" & grepl("Adventurers Family", park_inventory$park_name),]$Opened <- "19661111"
park_inventory[park_inventory$Name == "Kiddie Coaster" & grepl("Adventurers Family", park_inventory$park_name),]$yr_opened <- 1966

# write data
write_csv(park_inventory, 
          file = "park_inventory.csv")


# ride_specs.csv tidying and fixes----
setwd(wd$data)
ride_specs <- read_csv("ride_specs.csv")


ride_specs$length.ft <- gsub(" ft$", "", ride_specs$Length) %>%
  as.numeric()
ride_specs$height.ft <- gsub(" ft$", "", ride_specs$Height) %>%
  as.numeric()
ride_specs$speed.mph <- gsub(" mph$", "", ride_specs$Speed) %>%
  as.numeric()

write_csv(x = ride_specs, file = "ride_specs.csv")
