# Cedar Fair Only

# Build Data
library(dplyr)
library(janitor)
library(readr)
library(ggplot2)

rm(list=ls());cat('\f');gc()

# FUNS ----
# functions----
get_ride_years <- function(park.url){
  Sys.sleep(1.5)
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
              
              if(nchar(temp.Opened) == 4){
                temp.Opened <- paste(temp.Opened, "1111", sep = "")
              }
              
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
fun_tidy.years <- function(park.inventory = 'read_csv("park_inventory.csv")'){
  
  # simplify dataset
  dfqa <- park.inventory %>%
    .[colnames(.) %in% 
        c("park_url", "ride_url",
          "Opened", "Closed", "yr_opened", "yr_closed")] %>%
    .[!duplicated(.),]
  
  # fix problem with 'Opened' and 'Closed' dates ending in '0000'
  dfqa$Opened <- ifelse(nchar(as.numeric(dfqa$Opened)) %in% c(5,8), 
                        #gsub(pattern = "\\d{4,4}$", "1111", dfqa$Opened), 
                        substr(dfqa$Opened, 1, 4),
                        dfqa$Opened) %>% as.numeric() 
  dfqa$Closed <- ifelse(nchar(as.numeric(dfqa$Closed)) %in% c(5,8), 
                        #gsub(pattern = "\\d{4,4}$", "1111", dfqa$Closed), 
                        substr(dfqa$Closed, 1, 4),
                        dfqa$Closed) %>% as.numeric() 
  
  # if Opened and yr_opened don't match...
  dfqa$yro_best <- ifelse(mapply(identical, 
                                 x = as.numeric(dfqa$Opened), 
                                 y = as.numeric(dfqa$yr_opened)), 
                          yes = as.numeric(dfqa$Opened), 
                          no  = "no match")
  dfqa$yrc_best <- ifelse(mapply(identical, 
                                 x = as.numeric(dfqa$Closed), 
                                 y = as.numeric(dfqa$yr_closed)), 
                          yes = as.numeric(dfqa$Closed), 
                          no  = "no match")
  
  dfqa <- dfqa[,c("ride_url", "park_url", "yro_best", "yrc_best")]
  
  return(dfqa)
  # end
}


# DIRS ----
wd        <- list()
wd$data   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/data"
wd$R      <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/R"
wd$output <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/output"
wd$home   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution"

setwd(wd$data)

# VARS ----
dat_files <- list.files(pattern = "park|ride")

# load cedar fair parks
cw_cedarfair <- data.frame(operator_name = c("Cedar Fair"), 
                           operator_url = "https://rcdb.com/12487.htm", 
                           
                           park_url = paste("https://rcdb.com/", 
                                            c("4541", 
                                              #"4539",  # canada's wonderland - not in dataset bc outside usa 
                                              4542, 4529, 4588, 4544,
                                              4540,4546,4578,4552,4533), 
                                            ".htm",
                                            sep = "")) %>%
  as_tibble()


# IMPORT DATA ----

for(i in dat_files){
  # format variable name
  temp.varname <- gsub(pattern = "\\.csv", replacement = "", x = i)
  # assign i to variable
  assign(temp.varname,read_csv(i))
  # cleanup
  rm(temp.varname)
}


# FILTER DATA TO CEDAR FAIR----
cf_park_inventory <- park_inventory[park_inventory$park_url %in% cw_cedarfair$park_url,] 
cf_ride_specs     <- ride_specs[ride_specs$ride_url %in% cf_park_inventory$ride_url,] 

# ride_years----
cf_years     <- fun_tidy.years(park.inventory = cf_park_inventory)

# cleanup----
colnames(cf_park_inventory)[colnames(cf_park_inventory) == "Name"] <- "ride_name"
cf_park_inventory <- clean_names(cf_park_inventory)
cf_ride_specs     <- clean_names(cf_ride_specs)

# simplify----
cf_ride_specs     <- cf_ride_specs[,c("ride_url", "length_ft", "height_ft", "speed_mph")]
cf_park_inventory <- cf_park_inventory[!colnames(cf_park_inventory) %in% 
                                         c("closed", "yr_closed", "opened", "yr_opened")] 

# simplify park inventory
cf_park_inventory$ride_status <- cf_park_inventory$ride_status %>%
  gsub("Roller Coasters", "", .) %>%
  trimws() %>%
  tolower() %>%
  gsub(" ", "_", .)

cf_park_inventory$design <- cf_park_inventory$design %>% 
  gsub(" ", "_", .) %>%
  tolower() 

cf_park_inventory$scale <- cf_park_inventory$scale %>% tolower()
cf_park_inventory$scale <- ifelse(is.na(cf_park_inventory$scale), 
                                  "none_provided", cf_park_inventory$scale)

cf_park_inventory$park_name <- cf_park_inventory$park_name %>% 
  gsub("[[:punct:]]", "", .) %>%
  gsub(" amp ", " and ", .) %>%
  tolower() %>%
  gsub(" ", "_", .)

cf_park_inventory$ride_name <- cf_park_inventory$ride_name %>% 
  gsub("[[:punct:]]", "", .) %>%
  gsub(" amp ", " and ", .) %>%
  tolower() %>%
  gsub(" ", "_", .)


# simplify ride_specs
cf_ride_specs

# reorg----

cf_parks <- full_join(cf_park_inventory, cf_ride_specs) %>%
  group_by(park_name, park_url, ride_url) %>%
  summarise() %>%
  ungroup()

cf_rides <- full_join(cf_park_inventory, cf_ride_specs) %>%
  group_by(ride_name, ride_url, park_url, type, design, scale, 
           ride_status, length_ft, height_ft, speed_mph) %>%
  summarise() %>%
  ungroup() %>%
  left_join(., cf_years)

# cleanup----
rm(park_inventory, ride_specs, selected_parks, 
   cf_park_inventory, cf_ride_specs)

# ALL DATA----
cw_cedarfair
cf_parks
cf_rides
cf_years




