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

ride.url <- "https://rcdb.com/69.htm"
# ride.url <- "https://rcdb.com/530.htm"  # Invertigo
# ride.url <- "https://rcdb.com/location.htm?id=17774"  # Mason, OH
# 
# ride_info("https://rcdb.com/location.htm?id=17774")

# FUNS ----
ride_info <- function(ride.url){
  ride.html <- try(read_html(ride.url))
  
  #LOGICHECK
  if("try-error" %in% class(ride.html)){
    stop(glue("<ERROR - variable: 'ride.html'> ({ride.url})"))
  }
  #/LOGICHECK
  
  # get park name
  
  the.ridename <- try(html_element(x = ride.html, 
                               xpath = "//*[@id=\"objdiv\"]") %>%
    html_children() %>%
    html_children() %>%
    .[2] %>%
    .[1] %>%
    as.character() %>%
    strsplit(., 
             "\n") %>% 
    unlist() %>%
    .[grepl("<h1>", .)] %>%
    gsub("<h1>|</h1>", "", .))
  
  # LOGICHECK
  if(any(c(length(the.ridename) ==0 ,
     is.na(the.ridename) , 
     is.null(the.ridename) ,
     "try-error" %in% class(the.ridename))) ){
    stop(glue("<ERROR - variable: 'the.ridename'> ({ride.url})"))
  }
  # /LOGICHECK
  
  the.parkname <- try(html_element(x = ride.html, 
                               xpath = "//*[@id=\"objdiv\"]") %>%
    html_children() %>%
    html_children() %>%
    .[2] %>%
    .[1] %>%
    as.character() %>%
    strsplit(., 
             "\n") %>% 
    unlist() %>%
    .[grepl("location\\.htm?", .)] %>%
    gsub("\\(.*$", "", .) %>% 
    trimws() %>%
    strsplit(., "\"") %>%
    unlist() %>%
    .[!grepl("<a href|\\.htm", .)] %>%
    gsub(">|</a>", "", .))
  
  # LOGICHECK
  if(any(c(length(the.parkname) ==0 ,
           is.na(the.parkname) , 
           is.null(the.parkname) ,
           "try-error" %in% class(the.parkname))) ){
    stop(glue("<ERROR - variable: 'the.parkname'> ({ride.url})"))
  }
  # /LOGICHECK
  
  the.table <- try(html_element(x = ride.html, 
                            xpath = "/html/body") %>% 
    html_children() %>%
    .[grepl("Tracks", .)] %>%
    html_table() %>%
    .[[1]] %>%
      .[,c("X1", "X2")])  # added for the racer to address racing rides with 2 or more columns
  
  # LOGICHECK
  if(#length(the.table) != 1 |
     #is.na(the.table) | 
     any(c(is.null(the.table),
     "try-error" %in% class(the.table))) ){
    stop(glue("<ERROR - variable: 'the.table'> ({ride.url})"))
  }
  # /LOGICHECK
  
  the.table <- rbind(the.table, 
                     data.frame(X1 = c("ride_name", "park_name"), 
                                X2 = c(the.ridename, the.parkname)))
  
  out <- NULL
  for(i in 1:nrow(the.table)){
    temp <- the.table[i,] %>% unlist() %>% unname()
    
    temp.df <- data.frame(temp[2])
    colnames(temp.df) <- temp[1]
    
    
    if(is.null(out)){
      out <- temp.df
    }else{
      out <- cbind(out, temp.df)
    }
    
  }
  
  out <- out[,c("ride_name", "park_name", 
                "Length", "Height", "Speed")]
  
  # add ride_url 
  out$ride_url <- ride.url
  
  return(out)
}

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
                   ride_status = temp.table_name, 
                   ride_url = NA)
          
          for(i in 1:nrow(temp.table)){
            temp.ride_url <- html_element(x = park.html, 
                                          xpath = glue("/html/body/section[{s}]/div/table")) %>%
              html_children() %>%
              .[2] %>%  
              html_children() %>%
              .[i] %>%  
              html_children() %>%
              .[2] %>%
              html_element(., xpath = "a") %>%
              .[[1]] %>%
              xml2::xml_attrs() 
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
            temp.table$ride_url[i] <- temp.ride_url
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
                   ride_status = temp.table_name, 
                   ride_url = NA)
          
          for(i in 1:nrow(temp.table)){
            temp.ride_url <- html_element(x = park.html, 
                                          xpath = glue("/html/body/section[{s}]/div/table")) %>%
              html_children() %>%
              .[2] %>%  
              html_children() %>%
              .[i] %>%  
              html_children() %>%
              .[2] %>%
              html_element(., xpath = "a") %>%
              .[[1]] %>%
              xml2::xml_attrs() 
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
            temp.table$ride_url[i] <- temp.ride_url
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
                   ride_status = temp.table_name, 
                   ride_url = NA)
          
          for(i in 1:nrow(temp.table)){
            temp.ride_url <- html_element(x = park.html, 
                                          xpath = glue("/html/body/section[{s}]/div/table")) %>%
              html_children() %>%
              .[2] %>%  
              html_children() %>%
              .[i] %>%  
              html_children() %>%
              .[2] %>%
              html_element(., xpath = "a") %>%
              .[[1]] %>%
              xml2::xml_attrs() 
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
            temp.table$ride_url[i] <- temp.ride_url
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
                   ride_status = temp.table_name, 
                   ride_url = NA)
          
          for(i in 1:nrow(temp.table)){
            temp.ride_url <- html_element(x = park.html, 
                                          xpath = glue("/html/body/section[{s}]/div/table")) %>%
              html_children() %>%
              .[2] %>%  
              html_children() %>%
              .[i] %>%  
              html_children() %>%
              .[2] %>%
              html_element(., xpath = "a") %>%
              .[[1]] %>%
              xml2::xml_attrs() 
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
            temp.table$ride_url[i] <- temp.ride_url
          }
          temp.table
          
        }
        
        df.out <- rbind(df.out, 
                        temp.table)
        
      }
    }
  }
  
  # add park_url 
  df.out$park_url <- park.url
  
  # fix ride_url 
  df.out$ride_url <-  paste("https://rcdb.com", 
                            df.out$ride_url, sep = "")
  
  return(df.out)
}

rand_sleep <- function(n.times=1){
  temp <-  abs(log(c(runif(n = 560, 3,5),
                     runif(n = 1000, 0, 4)))) + runif(1,0,1) * prod(runif(2,0.5,0.9))
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
    Sys.sleep(rand_sleep()*2.1+0.5)
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
# generating a new file from scratch and variable 'park_inventory' from NULL. If
# 'park_inventory.csv' is written to dir, create variable 'park_inventory' from
# importing it

if(!"park_inventory.csv" %in% list.files()){
  # park_inventory.csv HAS NOT BEEN created
  temp.park_urls <- read_csv("selected_parks.csv")
  write_csv(x = park_info("https://rcdb.com/4540.htm"), 
            file = "park_inventory.csv", 
            append = F)
  park_inventory <- read_csv("park_inventory.csv")
}else{
  # park_inventory.csv HAS BEEN created
  temp.park_urls <- read_csv("selected_parks.csv")
  park_inventory <- read_csv("park_inventory.csv")
}

# 2) Then you loop through every park in temp.park_urls, before pinging that url
# check to see if you've already pulled down that data. If so, skip; if not -
# ping & log. if is.null(park_inventory) you will need to skip this

for(i in 1:nrow(temp.park_urls)){
  # check to see if park_url from temp.park_urls$park_url[i] is in
  # park_inventory$park_url
  
  if(!temp.park_urls$park_url[i] %in% unique(park_inventory$park_url)){
    # sleep
    Sys.sleep(rand_sleep()*2.1+0.5)
    print(Sys.time())
    # ping website and write to csv
    try(write_csv(x = park_info(temp.park_urls$park_url[i]), 
              file = "park_inventory.csv",
              append = T))
    # # also, append to park_inventory
    # park_inventory <- rbind(park_inventory, 
    #       park_info(temp.park_urls$park_url[i])) %>%
    #   .[!duplicated(.),]
  }else{
    print("skipped running 'park_info()' bc data has already been logged")
  }
  
}

# cleanup
rm(temp.park_urls, i, park_inventory)
setwd(wd$home)


# Build ride_specs.csv----
setwd(wd$data)

# 1) If 'ride_specs.csv' is not already written to dir, proceed as normal by
# generating a new file from scratch and variable 'ride_specs' from NULL. If
# 'ride_specs.csv' is written to dir, create variable 'ride_specs' from
# importing it

if(!"ride_specs.csv" %in% list.files()){
  # ride_specs.csv HAS NOT BEEN created
  temp.ride_urls <- read_csv("park_inventory.csv")
  write_csv(x = ride_info("https://rcdb.com/530.htm"), # invertigo
            file = "ride_specs.csv", 
            append = F)
  ride_specs <- read_csv("ride_specs.csv")
}else{
  # ride_specs.csv HAS BEEN created
  temp.ride_urls <- read_csv("park_inventory.csv")
  ride_specs <- read_csv("ride_specs.csv")
}

# 2) Then you loop through every park in temp.ride_urls, before pinging that url
# check to see if you've already pulled down that data. If so, skip; if not -
# ping & log. if is.null(park_inventory) you will need to skip this

for(i in 1:nrow(temp.ride_urls)){
  # check to see if park_url from temp.park_urls$park_url[i] is in
  # park_inventory$park_url
  
  if(!temp.ride_urls$ride_url[i] %in% unique(ride_specs$ride_url)){
    # sleep
    Sys.sleep(rand_sleep()*2.1+0.5)
    print(Sys.time())
    # ping website and write to csv
    info.try <- try(write_csv(x = ride_info(temp.ride_urls$ride_url[i]), 
                  file = "ride_specs.csv",
                  append = T))
   if("try-error" %in% class(info.try)){
     stop(glue("<ERROR> - MISSING COLUMNS ({temp.ride_urls$ride_url[i]})"))
   }
    
    
  }else{
    print("skipped running 'ride_info()' bc data has already been logged")
  }
  
}



setwd(wd$home)

# Build ride_status.csv----
setwd(wd$data)

park_inventory <- read_csv("park_inventory.csv")

park_inventory %>%
  group_by(park_name, park_url, 
           ride_name = Name, )

ride_specs <- read_csv("ride_specs.csv")

ride_specs %>%
  group_by(park_name) %>%
  summarise(n = n())
