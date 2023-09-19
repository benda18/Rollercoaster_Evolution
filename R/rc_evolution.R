# roller coaster evolution

library(dplyr)
library(rvest)
library(glue)
library(lubridate)
library(ggplot2)
library(xml2)

rm(list=ls());cat('\f');gc()

# Functions----

ride_info("https://rcdb.com/530.htm")

ride_info <- function(ride.url){
  ride.html <- read_html(ride.url)
  
  # get park name
  
  the.ridename <- html_element(x = ride.html, 
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
    gsub("<h1>|</h1>", "", .)
  
  the.parkname <- html_element(x = ride.html, 
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
    gsub(">|</a>", "", .)
  
  
  
  the.table <- html_element(x = ride.html, 
                            xpath = "/html/body") %>% 
    html_children() %>%
    .[grepl("Tracks", .)] %>%
    html_table() %>%
    .[[1]]
  
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


# generate all the urls for the theme parks in north america that are extant----







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



# Pull rc info----

park_rides <- function(park.url){
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
        # # section name
        # temp.table_name <- temp %>%
        #   html_children() %>%
        #   .[1] %>%
        #   as.character() %>%
        #   strsplit(., "\"") %>%
        #   unlist() %>%
        #   .[grepl("Roller", .)] %>%
        #   gsub("^<h4>|:.*$", "", .)
        
        ## operating table----
        # if(grepl("Operating", temp.table_name)){
          
        temp.rides <-  html_element(x = park.html, 
                                     xpath = glue("/html/body/section[{s}]")) %>%
            html_children() %>%
            .[2] %>%
            html_children() %>%
            html_children() %>%
            html_children() 
        for(i in 1:length(temp.rides)){
          temp.rides2 <- temp.rides %>%
            .[i] %>%  # iterate through rides here
            html_children() %>%
            .[2]      # ride url suffix here and name
            
          temp.ride_url <- temp.rides2 %>%
              html_element(., xpath = "a") %>%
            .[[1]] %>%
            xml2::xml_attrs() 
          temp.ride_name <- temp.rides2 %>% html_text()
          
          df.out <- rbind(df.out, 
                          data.frame(name = temp.ride_name, 
                                     url = temp.ride_url))
        }
        
      }
    }
  }
  
  # clean
  df.out <- df.out[!is.na(df.out$url),]
  
  # update url
  
  df.out$url <- paste("https://rcdb.com", df.out$url, sep = "")
  df.out$park <- the.parkname
  
  return(df.out)
}





# Vars----

park.urls <- c("https://rcdb.com/4540.htm", # kings island
               "https://rcdb.com/4593.htm", # dollywood
               "https://rcdb.com/4734.htm", # universal studios island of adventure
               "https://rcdb.com/4736.htm", #universal studios florida
               "https://rcdb.com/4546.htm", # knott's berry farm
               "https://rcdb.com/4552.htm", #valley fair
               "https://rcdb.com/4533.htm", # worlds of fun
               "https://rcdb.com/4588.htm", #dorney park
               "https://rcdb.com/4578.htm", #michigan adventure
               "https://rcdb.com/4541.htm", # california's great america
               "https://rcdb.com/4539.htm", # canada wonderland
               "https://rcdb.com/4542.htm", # carowinds
               "https://rcdb.com/4544.htm", # kings dominion
               "https://rcdb.com/4529.htm", 
              "https://rcdb.com/4532.htm", 
              "https://rcdb.com/4534.htm", 
              "https://rcdb.com/4530.htm",
              "https://rcdb.com/4531.htm", 
              "https://rcdb.com/4545.htm", 
              "https://rcdb.com/4535.htm", 
              "https://rcdb.com/4538.htm", 
              "https://rcdb.com/4565.htm", 
              "https://rcdb.com/4570.htm", 
              "https://rcdb.com/4548.htm", 
              "https://rcdb.com/4711.htm", 
              "https://rcdb.com/4536.htm", 
              "https://rcdb.com/4558.htm", 
              "https://rcdb.com/4541.htm", 
              "https://rcdb.com/4543.htm", 
              "https://rcdb.com/9250.htm", 
              "https://rcdb.com/4552.htm", 
              "https://rcdb.com/4581.htm",
              "https://rcdb.com/4746.htm",
              "https://rcdb.com/4553.htm", 
              "https://rcdb.com/4533.htm", 
              "https://rcdb.com/4579.htm", 
              "https://rcdb.com/4564.htm", 
              "https://rcdb.com/4599.htm", 
              "https://rcdb.com/4560.htm", 
              "https://rcdb.com/4578.htm", 
              "https://rcdb.com/4576.htm", 
              "https://rcdb.com/4601.htm", 
              "https://rcdb.com/4703.htm", 
              "https://rcdb.com/4574.htm", 
              "https://rcdb.com/5320.htm", 
              "https://rcdb.com/4554.htm",
              "https://rcdb.com/4563.htm", 
              "https://rcdb.com/4596.htm", 
              "https://rcdb.com/4557.htm", 
              "https://rcdb.com/4584.htm", 
              "https://rcdb.com/18828.htm", 
              "https://rcdb.com/4683.htm", 
              "https://rcdb.com/4682.htm", 
              "https://rcdb.com/4597.htm", 
              "https://rcdb.com/4547.htm",
              "https://rcdb.com/15593.htm", 
              "https://rcdb.com/4575.htm", 
              "https://rcdb.com/4559.htm", 
              "https://rcdb.com/4646.htm", 
              "https://rcdb.com/4561.htm") %>% unique()


all.rcs <- NULL
for(pu1 in park.urls) {
  Sys.sleep(1)
  all.rcs <- rbind(all.rcs, 
                   park_info(pu1))
}

# adjustments 

all.rcs <- all.rcs[!(all.rcs$Name %in% c("Kiddie Coaster") & 
                     grepl("Dorney", all.rcs$park_name)),]
all.rcs$yr_closed[all.rcs$Name == "Scamper" & 
                    all.rcs$park_name == "Dollywood"] <- 1972
# all.rcs$yr_closed[all.rcs$Name == "Kiddie Coaster" & 
#                     all.rcs$park_name == "Dorney Park &amp; Wildwater Kingdom"] <- NA
all.rcs$yr_closed[all.rcs$Name == "Super Coaster" & 
                    all.rcs$park_name == "Cedar Point"] <- 1964

all.rcs$yr_closed[is.na(all.rcs$yr_closed)] <- year(Sys.Date())

ggplot() + 
  geom_segment(data = all.rcs[all.rcs$park_name == "Kings Island",], 
               aes(x = yr_opened, 
                   xend = yr_closed, 
                   y = Name, yend = Name, 
                   color = Design, 
                   linewidth = 4, linetype = Scale)) +
  facet_grid(Type+Design~., scales = "free_y", space = "free_y")

all.rcs[grepl("son of beast", all.rcs$Name, ignore.case = T),]

all.rcs <- all.rcs[!(all.rcs$yr_closed == 0 | all.rcs$yr_opened == 0),]

# cumulative 
ki.density.scale  <- NULL
ki.density.design <- NULL
ki.density.type   <- NULL

for(i in 1:nrow(all.rcs)){
  temp.park   <- all.rcs$park_name[i]
  temp.name   <- all.rcs$Name[i]
  temp.scale  <- all.rcs$Scale[i]
  temp.design <- all.rcs$Design[i]
  temp.type   <- all.rcs$Type[i]
  temp.yrs    <- try(seq(all.rcs$yr_opened[i], 
                             all.rcs$yr_closed[i], 
                             by = 1))
  if(!class(temp.yrs) == "try-error"){
    ki.density.design <- rbind(ki.density.design, 
                               data.frame(name = temp.name,
                                          park = temp.park,
                                          design = temp.design, 
                                          year = temp.yrs))
    
    ki.density.scale <- rbind(ki.density.scale, 
                              data.frame(name = temp.name,
                                         park = temp.park,
                                         scale = temp.scale, 
                                         year = temp.yrs))
    ki.density.type <- rbind(ki.density.type, 
                             data.frame(name = temp.name,
                                        park = temp.park,
                                        type = temp.type, 
                                        year = temp.yrs))
  }
  
}

ki.density.design2 <- ki.density.design %>% 
  group_by(year, design, park) %>%
  summarise(n_rides = n_distinct(name))

ki.density.scale2 <- ki.density.scale %>%
  group_by(year, scale, park) %>%
  summarise(n_rides = n_distinct(name))

ki.density.type2 <- ki.density.type %>%
  group_by(year,type,park) %>%
  summarise(n_rides = n_distinct(name))

ki.density.design2$design_f <- factor(ki.density.design2$design, 
                                     levels = rev(unique(ki.density.design2$design[order(ki.density.design2$year)])))
ki.density.scale2$scale_f <- factor(ki.density.scale2$scale, 
                                      levels = rev(unique(ki.density.scale2$scale[order(ki.density.scale2$year)])))
ki.density.type2$type_f <- factor(ki.density.type2$type, 
                                      levels = rev(unique(ki.density.type2$type[order(ki.density.type2$year)])))
ggplot(data = ki.density.design2, 
       aes(x = year, y = n_rides, 
           fill = design_f)) + 
  #geom_point(position = "stack") +
  geom_col(position = "fill")+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")+
  scale_y_continuous()

ggplot(data = ki.density.scale2, 
       aes(x = year, y = n_rides, 
           fill = scale_f)) + 
  #geom_point(position = "stack") +
  geom_col(position = "fill")+
  theme(legend.position = "bottom", 
        legend.direction = "vertical")+
  scale_y_continuous()

ggplot(data = ki.density.type2, 
       aes(x = year, y = n_rides, 
           fill = type_f)) + 
  #geom_point(position = "stack") +
  geom_col(position = "fill")+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")+
  #scale_y_sqrt()+
  geom_vline(data = data.frame(year = c(1972, 1979, 2000, 2017)), 
             aes(xintercept = year))+
  scale_x_continuous(breaks = seq(0,10000,by=10))
  

# rides constructed each year

all.rcs %>%
  group_by(yr_closed, Type) %>%
  .[.$yr_closed < 2023,] %>%
  summarise(n = n()) %>%
  ggplot(data = ., 
         aes(x = yr_closed, y = n, 
             fill = Type))+
  geom_col(position = "stack")
