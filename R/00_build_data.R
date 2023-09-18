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
get_park_urls <- function(searchpage.url = "https://rcdb.com/r.htm?order=28&page=9&st=93&ot=3&ol=59&ex"){
  temp <- searchpage.url %>%
    read_html() %>%
    html_element(., 
                 xpath = "/html/body/section/div[2]/table/tbody") %>%
    html_children()
  
  out <- NULL
  for(i in 1:length(temp)){
    out <- c(out, 
             temp %>%
               .[i] %>%  # replace # with 'i'
               html_children() %>%
               .[2] %>%#   #2 (of 6) represents the park name and url
               as.character() %>%
               strsplit(., "\"") %>%
               unlist() %>%
               .[grepl(pattern = "\\d{4,}\\.htm$", .)])
  }
  
  # append url prefix
  out <- paste("https://rcdb.com", out, 
               sep = "")
  return(out)
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
    Sys.sleep(1)
    # generate each search page's url iteratively  
    ps_url_str <- glue::glue("https://rcdb.com/r.htm?order=28&page={i}&st=93&ot=3&ol=59&ex")
    temp.selectedparks <- c(temp.selectedparks, 
                            get_park_urls(ps_url_str))
  }
  
  
  # convert from vector to data.frame 
  selected_parks <- data.frame(park_url = temp.selectedparks)
  
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

# Build park_inventory.csv

# Build ride_inventory.csv

# Build ride_specs.csv

# Build ride_status.csv