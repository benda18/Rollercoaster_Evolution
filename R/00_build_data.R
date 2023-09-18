# Build Data
library(dplyr)
library(janitor)
#library(here)  # project-oriented workflow // '??here'
library(rvest)
library(xml2)
library(glue)
library(lubridate)
#library(ggplot2)
library(readr)


rm(list=ls());cat('\f');gc()

# FUNS ----
get_park_urls <- function(searchpage.url){
  temp <- searchpage.url %>%
    read_html() %>%
    html_element(., 
                 xpath = "/html/body/section/div[2]/table/tbody") %>%
    html_children()
  
  out <- NULL
  for(i in 1:length(temp)){
    out.url <- temp %>%
               .[i] %>%  # replace # with 'i'
               html_children() %>%
               .[2] %>%#   #2 (of 6) represents the park name and url
               as.character() %>%
               strsplit(., "\"") %>%
               unlist() %>%
               .[grepl(pattern = "\\d{4,}\\.htm$", .)]
    out.park <- temp %>%
                    .[i] %>%  # replace # with 'i'
                    html_children() %>%
                    .[2] %>%#   #2 (of 6) represents the park name and url
                    as.character() %>%
                    strsplit(., "\"") %>%
                    unlist() %>%
                    .[3] %>%
                    gsub("^>|</a>|</td>|\n", "", .) 
    out <- rbind(out, 
                 data.frame(park_name = out.park, 
                            park_url = out.url))
  }
  out
  # append url prefix
  out$park_url <- paste("https://rcdb.com", out$park_url, 
               sep = "")
  return(out)
}

park_info <- function(park.url){
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
    #print(i)
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
            rvest::html_table(., convert = T, trim = T) %>%
            .[,2:6] %>%
            mutate(., 
                   Opened = NA_character_, 
                   Closed = NA_character_,
                   #Opened2 = NA_character_,
                   yr_opened = NA_real_, 
                   yr_closed = NA_real_, 
                   park_name = the.parkname, 
                   ride_status = temp.table_name)
          
          for(i in 1:nrow(temp.table)){
            temp.date <- html_element(x = park.html, 
                                      xpath = glue("/html/body/section[{s}]/div/table")) %>%
              html_children() %>%
              .[2] %>%  # 2 = "body"
              html_children() %>%
              .[i] %>%  # 1 = 1st row of rides
              html_children() %>%
              as.character() %>%
              .[6] %>% # represents columns of table (i.e. "i")
              strsplit(., "\"") %>%
              unlist() 
            
            temp.table$Opened[i] <- temp.date[2]
            #df.ex_rides$Opened2[i] <- temp.date[4]
            temp.table$yr_opened[i] <- as.numeric(temp.date[2]) %/% 10000
            
          }
          temp.table
        }
        
        # construction table----
        if(grepl("Under Construction", temp.table_name)){
          
          temp.table <- html_element(x = park.html, 
                                     xpath = glue("/html/body/section[{s}]")) %>%
            rvest::html_table(., convert = T, trim = T) %>%
            .[,2:6] %>%
            mutate(., 
                   Opened = NA_character_, 
                   Closed = NA_character_,
                   #Opened2 = NA_character_,
                   yr_opened = NA_real_, 
                   yr_closed = NA_real_, 
                   park_name = the.parkname, 
                   ride_status = temp.table_name)
          
          for(i in 1:nrow(temp.table)){
            temp.date <- html_element(x = park.html, 
                                      xpath = glue("/html/body/section[{s}]/div/table")) %>%
              html_children() %>%
              .[2] %>%  # 2 = "body"
              html_children() %>%
              .[i] %>%  # 1 = 1st row of rides
              html_children() %>%
              as.character() %>%
              .[6] %>% # represents columns of table (i.e. "i")
              strsplit(., "\"") %>%
              unlist() 
            
            temp.table$Opened[i] <- temp.date[2]
            #df.ex_rides$Opened2[i] <- temp.date[4]
            temp.table$yr_opened[i] <- as.numeric(temp.date[2]) %/% 10000
            
          }
          temp.table <- temp.table[!colnames(temp.table) %in% c("Opening")]
          
          
        }
        # sbno table----
        if(grepl("SBNO", temp.table_name)){
          
          temp.table <- html_element(x = park.html, 
                                     xpath = glue("/html/body/section[{s}]")) %>%
            rvest::html_table(., convert = T, trim = T) %>%
            .[,2:6] %>%
            mutate(., 
                   Opened = NA_character_, 
                   Closed = NA_character_,
                   #Opened2 = NA_character_,
                   yr_opened = NA_real_, 
                   yr_closed = NA_real_, 
                   park_name = the.parkname, 
                   ride_status = temp.table_name)
          
          for(i in 1:nrow(temp.table)){
            temp.date <- html_element(x = park.html, 
                                      xpath = glue("/html/body/section[{s}]/div/table")) %>%
              html_children() %>%
              .[2] %>%  # 2 = "body"
              html_children() %>%
              .[i] %>%  # 1 = 1st row of rides
              html_children() %>%
              as.character() %>%
              .[6] %>% # represents columns of table (i.e. "i")
              strsplit(., "\"") %>%
              unlist() 
            
            temp.table$Opened[i] <- temp.date[2]
            #df.ex_rides$Opened2[i] <- temp.date[4]
            temp.table$yr_opened[i] <- as.numeric(temp.date[2]) %/% 10000
            
          }
          temp.table <- temp.table[!colnames(temp.table) %in% c("Since")]
        }
        
        # defunct table----
        if(grepl("Defunct", temp.table_name)){
          
          
          temp.table <- html_element(x = park.html, 
                                     xpath = glue("/html/body/section[{s}]")) %>%
            rvest::html_table(., convert = T, trim = T) %>%
            .[,2:7] %>%
            mutate(., 
                   Opened = NA_character_, 
                   Closed = NA_character_,
                   #Opened2 = NA_character_,
                   yr_opened = NA_real_, 
                   yr_closed = NA_real_, 
                   park_name = the.parkname, 
                   ride_status = temp.table_name)
          
          for(i in 1:nrow(temp.table)){
            
            # opening date
            temp.date <- html_element(x = park.html, 
                                      xpath = glue("/html/body/section[{s}]/div/table")) %>%
              html_children() %>%
              .[2] %>%  # 2 = "body"
              html_children() %>%
              .[i] %>%  # 1 = 1st row of rides
              html_children() %>%
              as.character() %>%
              .[6] %>% # represents columns of table (i.e. "i")
              strsplit(., "\"") %>%
              unlist() 
            
            temp.table$Opened[i] <- temp.date[2]
            temp.table$yr_opened[i] <- as.numeric(temp.date[2]) %/% 10000
            
            # closed date
            temp.date <- html_element(x = park.html, 
                                      xpath = glue("/html/body/section[{s}]/div/table")) %>%
              html_children() %>%
              .[2] %>%  # 2 = "body"
              html_children() %>%
              .[i] %>%  # 1 = 1st row of rides
              html_children() %>%
              as.character() %>%
              .[7] %>% # represents columns of table (i.e. "i")
              strsplit(., "\"") %>%
              unlist() 
            
            temp.table$Closed[i] <- temp.date[2]
            #df.ex_rides$Opened2[i] <- temp.date[4]
            temp.table$yr_closed[i] <- as.numeric(temp.date[2]) %/% 10000
            
          }
          temp.table
          
        }
        
        df.out <- rbind(df.out, 
                        temp.table)
        
      }
    }
  }
  
  return(df.out)
}
rand_sleep <- function(n.times=1){
  temp <-  abs(log(c(runif(n = 560, 3,5),
                     runif(n = 1000, 0, 4)))) + runif(1,0,1) * prod(runif(2,0.5,2))
  return(sample(temp, size = n.times, replace = T))
}


# DIRS ----
wd        <- list()
wd$data   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/data"
wd$R      <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/R"
wd$output <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/output"
wd$home   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution"

# VARS ----

# BUILD ----
# Build selected_parks.csv ----

# Pre-build logic check so as to conserve resources / not re-build data that has
# already been built
setwd(wd$data)
if(!"selected_parks.csv" %in% list.files()){
  # build
  setwd(wd$data)
  
  temp.selectedparks <- NULL
  for(i in 1:17){
    # sleep 
    Sys.sleep(rand_sleep())
    # generate each search page's url iteratively  
    ps_url_str <- glue::glue("https://rcdb.com/r.htm?order=28&page={i}&st=93&ot=3&ol=59&ex")
    temp.selectedparks <- rbind(temp.selectedparks, 
                            get_park_urls(ps_url_str))
  }
  
  
  # convert from vector to data.frame 
  selected_parks <- temp.selectedparks
  
  # write to csv
  setwd(wd$data)
  write_csv(x = selected_parks, 
            file = "selected_parks.csv")
  
  # cleanup
  rm(temp.selectedparks, selected_parks,i,ps_url_str)
  
}else{
  # no-build
  print("skipping build of 'selected_parks.csv'; was built previously")
}

setwd(wd$home)




# Build crosswalk_pr.csv----


# Build park_inventory.csv----
setwd(wd$data)

# Pre-build logic is now 2-step:  

# 1) If 'park_inventory.csv' is not already written to dir, proceed as normal by
# generating a new file from scratch and variable 'park_inventory' from NULL

# 2) If 'park_inventory.csv' is written to dir, create variable 'park_inventory'
# from importing it, then as you loop through every park in your park.url list,
# before pinging that url check to see if you've already pulled down that data.
# If so, skip; if not - ping & log.

if(!"park_inventory.csv" %in% list.files()){
  # park_inventory.csv HAS NOT BEEN created
  temp.park_urls <- read_csv("selected_parks.csv")$park_url
  park_inventory <- NULL
  
  
}else{
  # park_inventory.csv HAS BEEN created
  temp.park_urls <- read_csv("selected_parks.csv")$park_url
  park_inventory <- read_csv("park_inventory.csv")
}




if(!"park_inventory.csv" %in% list.files()){
  
  # import list of park urls
  temp.park_urls <- read_csv("selected_parks.csv")$park_url
  
  park_inventory <- NULL
  for(i in temp.park_urls){
    # sleep
    Sys.sleep(rand_sleep())
    
    park_inventory <- rbind(park_inventory, 
                            park_info(i))
  }
  
  # write csv
  write_csv(park_inventory, 
            file = "park_inventory.csv",
            append = T)
  # cleanup
  rm(i, temp.park.urls, park_inventory)
  
}else{
  # no-build
  print("skipping build of 'park_inventory.csv'; was built previously")
}













# Build ride_inventory.csv

# Build ride_specs.csv

# Build ride_status.csv