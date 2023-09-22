# All Parks

# Build Data
library(dplyr)
library(janitor)
library(readr)
library(ggplot2)

rm(list=ls());cat('\f');gc()

# FUNS ----
# functions----
build_the_year <- function(yr, df.rides = cf_rides){
  df.rides$yrc_best[is.na(df.rides$yrc_best) & 
                      df.rides$ride_status != "under_construction"] <- year(Sys.Date())
  df.rides <- df.rides[!df.rides$ride_status %in% c("under_construction", "sbno"),]
  
  which.in.yr <- NULL
  for(i in 1:nrow(df.rides)){
    if(between(x = yr,
               lower = df.rides$yro_best[i], 
               upper = df.rides$yrc_best[i])){
      which.in.yr <- c(which.in.yr, 
                       i)
    }
  }
  
  out.df <- data.frame(ride_url = df.rides$ride_url[which.in.yr], 
                       year_active = yr)
  return(out.df)
}


#issue.url <- "https://rcdb.com/4534.htm"  # six flags great adventure
#get_ride_years(issue.url)

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
              
              if(nchar(temp.Opened) == 4){  # TODO 
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
              
              if(length(temp.Opened) > 0){
                if(nchar(temp.Opened) == 4){
                  temp.Opened <- paste(temp.Opened, "1111", sep = "")
                }else{
                  temp.Opened <- NA
                }
              }else{
                temp.Opened <- NA
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
# fun_tidy.years <- function(park.inventory = 'read_csv("park_inventory.csv")'){
#   
#   # simplify dataset
#   dfqa <- park.inventory %>%
#     .[colnames(.) %in% 
#         c("park_url", "ride_url",
#           "Opened", "Closed", "yr_opened", "yr_closed")] %>%
#     .[!duplicated(.),]
#   
#   # fix problem with 'Opened' and 'Closed' dates ending in '0000'
#   dfqa$Opened <- ifelse(nchar(as.numeric(dfqa$Opened)) %in% c(5,8), 
#                         #gsub(pattern = "\\d{4,4}$", "1111", dfqa$Opened), 
#                         substr(dfqa$Opened, 1, 4),
#                         dfqa$Opened) %>% as.numeric() 
#   dfqa$Closed <- ifelse(nchar(as.numeric(dfqa$Closed)) %in% c(5,8), 
#                         #gsub(pattern = "\\d{4,4}$", "1111", dfqa$Closed), 
#                         substr(dfqa$Closed, 1, 4),
#                         dfqa$Closed) %>% as.numeric() 
#   
#   # if Opened and yr_opened don't match...
#   dfqa$yro_best <- ifelse(mapply(identical, 
#                                  x = as.numeric(dfqa$Opened), 
#                                  y = as.numeric(dfqa$yr_opened)), 
#                           yes = as.numeric(dfqa$Opened), 
#                           no  = "no match")
#   dfqa$yrc_best <- ifelse(mapply(identical, 
#                                  x = as.numeric(dfqa$Closed), 
#                                  y = as.numeric(dfqa$yr_closed)), 
#                           yes = as.numeric(dfqa$Closed), 
#                           no  = "no match")
#   
#   dfqa <- dfqa[,c("ride_url", "park_url", "yro_best", "yrc_best")]
#   
#   return(dfqa)
#   # end
# }


# DIRS ----
wd        <- list()
wd$data   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/data"
wd$R      <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/R"
wd$output <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/output"
wd$home   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution"

setwd(wd$data)

# VARS ----
dat_files <- list.files(pattern = "park|ride")

# IMPORT DATA ----

for(i in dat_files){
  # format variable name
  temp.varname <- gsub(pattern = "\\.csv", replacement = "", x = i)
  # assign i to variable
  assign(temp.varname,read_csv(i))
  # cleanup
  rm(temp.varname)
}

# Get the Best Years----
park_inventory
#get_ride_years(park.url = NA)

# figure out which order to query parks
operating_parks <- park_inventory %>%
  .[.$ride_status == "Operating Roller Coasters",] %>%
  left_join(., ride_specs[,c("ride_url", "length.ft", "height.ft", "speed.mph")]) %>%
  .[!is.na(.$length.ft),] %>%
  group_by(park_url) %>%
  summarise(n_rides = n_distinct(ride_url), 
            t_length = sum(length.ft), 
            max_length = max(length.ft)) %>%
  .[order(.$n_rides,decreasing = T),]  %>%
  .[.$n_rides > 1,]

new_pr_years <- NULL
for(i in unique(operating_parks$park_url)){
  new_pr_years <- rbind(new_pr_years, 
                        get_ride_years(park.url = i))
}

new_pr.backup <- new_pr_years
# incomplete Opened 
new_pr_years[is.na(new_pr_years$Opened),]
new_pr_years <- new_pr_years[!is.na(new_pr_years$Opened),]



# manual fixes Opened
new_pr_years$opened2 <- new_pr_years$Opened
new_pr_years$opened2 <- gsub(pattern = "0000$", replacement = "1111", x = new_pr_years$opened2)
new_pr_years$opened2 <- gsub(pattern = "00$", replacement = "11", 
                             x = new_pr_years$opened2)

ymd(new_pr_years$Opened) %>% range(., na.rm = T)


# manual fixes Closed
new_pr_years$closed2 <- new_pr_years$Closed
new_pr_years$closed2 <- gsub(pattern = "0000$", replacement = "1111", x = new_pr_years$closed2)
new_pr_years$closed2 <- gsub(pattern = "00$", replacement = "11", 
                             x = new_pr_years$closed2)
new_pr_years$closed2 <- gsub(pattern = "^0000\\d{4,4}$", replacement = NA, 
                             x = new_pr_years$closed2)

ymd(new_pr_years$closed2) %>% range(., na.rm = T)

# calc_year----

new_pr_years$yro_best <- year(ymd(new_pr_years$opened2))
new_pr_years$yrc_best <- year(ymd(new_pr_years$closed2))

new_pr_years <- as_tibble(new_pr_years)


# rejoin to park_inventory
park_inventory <- park_inventory[!colnames(park_inventory) %in%
                                   c("Opened", "yr_opened", 
                                     "Closed", "yr_closed")] %>%
  left_join(., 
            new_pr_years) 

park_inventory %>% #[park_inventory$park_name == "Cedar Point",] %>%
  group_by(Type) %>%
  summarise(n = n_distinct(ride_url), 
            range_yo = range(yro_best,na.rm = T), 
            range_yc = max(yrc_best,na.rm = T))


# write_csv(park_inventory,
#           "park_inventory.csv")


# # roller coaster evolution
# 
# library(dplyr)
# library(rvest)
# library(glue)
# library(lubridate)
# library(ggplot2)
# library(xml2)
# library(igraph)
# library(data.table)
# library(janitor)
# library(readr)
# 
# rm(list=ls());cat('\f');gc()
# 
# # DIRS ----
# wd        <- list()
# wd$data   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/data"
# wd$R      <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/R"
# wd$output <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/output"
# wd$home   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution"
# 
# # FUNS ----
# 
# fun_tidy.years <- function(park.inventory = read_csv("park_inventory.csv")){
#   
#   # simplify dataset
#   dfqa <- park.inventory %>%
#     .[colnames(.) %in% 
#         c("park_url", "ride_url",
#           "Opened", "Closed", "yr_opened", "yr_closed")] %>%
#     .[!duplicated(.),]
#   
#   # fix problem with 'Opened' and 'Closed' dates ending in '0000'
#   dfqa$Opened <- ifelse(nchar(as.numeric(dfqa$Opened)) %in% c(5,8), 
#                         #gsub(pattern = "\\d{4,4}$", "1111", dfqa$Opened), 
#                         substr(dfqa$Opened, 1, 4),
#                         dfqa$Opened) %>% as.numeric() 
#   dfqa$Closed <- ifelse(nchar(as.numeric(dfqa$Closed)) %in% c(5,8), 
#                         #gsub(pattern = "\\d{4,4}$", "1111", dfqa$Closed), 
#                         substr(dfqa$Closed, 1, 4),
#                         dfqa$Closed) %>% as.numeric() 
#   
#   # if Opened and yr_opened don't match...
#   dfqa$yro_best <- ifelse(mapply(identical, 
#                                  x = as.numeric(dfqa$Opened), 
#                                  y = as.numeric(dfqa$yr_opened)), 
#                           yes = as.numeric(dfqa$Opened), 
#                           no  = "no match")
#   dfqa$yrc_best <- ifelse(mapply(identical, 
#                                  x = as.numeric(dfqa$Closed), 
#                                  y = as.numeric(dfqa$yr_closed)), 
#                           yes = as.numeric(dfqa$Closed), 
#                           no  = "no match")
#   
#   dfqa <- dfqa[,c("ride_url", "park_url", "yro_best", "yrc_best")]
#   
#   return(dfqa)
#   # end
# }
# 
# best_open.date <- function(yr.opened, opened){
#   require(lubridate)
#   v1 <- yr.opened
#   v2 <- opened
#   
#   # create df
#   temp.df <- data.frame(v1op       = v1, 
#                         v2op       = v2, 
#                         asnumbw_v1 = NA, 
#                         asnumbw_v2 = NA, 
#                         ymdyrbw_v1 = NA, 
#                         ymdyrbw_v2 = NA, 
#                         n_v1op     = NA, 
#                         n_v2op     = NA,
#                         v1op.yr    = NA, 
#                         v2op.yr    = NA) %>% 
#     as_tibble()
#   
#   # iterate
#   for(i in 1:nrow(temp.df)){
#     # convert 8-dig values that end in '0000' to '1111'
#     temp.df$v1op[i] <- temp.df$v1op[i] %>% gsub(pattern = "0000$", replacement = "1111", x = .)
#     temp.df$v2op[i] <- temp.df$v2op[i] %>% gsub(pattern = "0000$", replacement = "1111", x = .)
#     
#     # convert both to numeric, look for value between 1850 and 2029
#     temp.df$asnumbw_v1[i] <- data.table::between(as.numeric(temp.df$v1op[i]),1850,2030)
#     temp.df$asnumbw_v2[i] <- data.table::between(as.numeric(temp.df$v2op[i]),1850,2030)
#     
#     # ymd() %>% year() both, look for value between 1850 and 2029
#     temp.df$ymdyrbw_v1[i] <- data.table::between(year(ymd(temp.df$v1op[i])),1850,2030) %>% ifelse(is.na(.), F, .)
#     temp.df$ymdyrbw_v2[i] <- data.table::between(year(ymd(temp.df$v2op[i])),1850,2030) %>% ifelse(is.na(.), F, .)
#     
#     # generate year value for both v1 and v2
#     temp.df$v1op.yr[i] <- ifelse(temp.df$asnumbw_v1[i], as.numeric(temp.df$v1op[i]), year(ymd(temp.df$v1op[i])))
#     temp.df$v2op.yr[i] <- ifelse(temp.df$asnumbw_v2[i], as.numeric(temp.df$v2op[i]), year(ymd(temp.df$v2op[i])))
#     
#   }
#   
#   temp.df <- mutate(temp.df, 
#                     n_v1op = asnumbw_v1 + ymdyrbw_v1,
#                     n_v2op = asnumbw_v2 + ymdyrbw_v2)
#   
#   
#   temp.df$best <- NA
#   temp.df$best <- ifelse(temp.df$v1op.yr == temp.df$v2op.yr, temp.df$v1op.yr, temp.df$best)
#   temp.df$best <- ifelse(temp.df$v1op.yr > temp.df$v2op.yr, temp.df$v1op.yr, temp.df$best) # son of beast fails here
#   temp.df$best <- ifelse(temp.df$v1op.yr < temp.df$v2op.yr, temp.df$v2op.yr, temp.df$best)
#   
#   v.out <- temp.df$best
#   return(v.out)
# }
# 
# # IMPORT DATA ----
# 
# setwd(wd$data)
# 
# dat_files <- list.files(pattern = "park|ride")
# for(i in dat_files){
#   # format variable name
#   temp.varname <- gsub(pattern = "\\.csv", replacement = "", x = i)
#   # assign i to variable
#   assign(temp.varname,read_csv(i))
#   # cleanup
#   rm(temp.varname)
# }
# rm(i,dat_files)
# 
# 
# # Use Cedar Fair Parks As Baseline to Gauge Data Accuracy----
# setwd(wd$data)
# 
# cw_cedarfair <- data.frame(operator_name = c("Cedar Fair"), 
#                            operator_url = "https://rcdb.com/12487.htm", 
#                            #park_name = c(NA), 
#                            park_url = c("4541", 
#                                         #"4539",  # canada's wonderland - not in dataset bc outside usa 
#                                         4542, 4529, 4588, 4544,
#                                         4540,4546,4578,4552,4533)) %>%
#   as_tibble()
# 
# cw_cedarfair$park_url <- cw_cedarfair$park_url %>%
#   paste(., ".htm", sep = "") %>%
#   paste("https://rcdb.com/", ., 
#         sep = "")
# 
# cw_cedarfair <- left_join(cw_cedarfair, 
#           park_inventory[,c("park_name", "park_url")] ) %>%
#   .[!duplicated(.),]
# 
# 
# #cw_cedarfair$park_name[cw_cedarfair$park_url == "https://rcdb.com/4539.htm"] <- "Canada's Wonderland"
# 
# cf_park_inventory <- park_inventory %>%
#   .[.$park_url %in% cw_cedarfair$park_url,]
# 
# cf_ride_specs <- ride_specs %>%
#   .[.$ride_url %in% cf_park_inventory$ride_url,]
# 
# # Confirm parks are in the base data
# cf_park_inventory %>%
#   group_by(park_url) %>%
#   summarise(n_rides = n_distinct(Name, na.rm = T)) %>%
#   full_join(., 
#             cw_cedarfair)
# 
# 
# # park_density <- park_inventory %>%
# #   .[.$ride_status == "Operating Roller Coasters",] %>%
# #   group_by(park_name, Design) %>%
# #   summarise(n = n()) %>%
# #   as.data.table() %>%
# #   dcast(., park_name ~ Design, value.var = "n", 
# #         fun.aggregate = sum, fill = 0, 
# #         drop = F) %>%
# #   as.data.frame() %>%
# #   as_tibble() %>%
# #   clean_names() %>%
# #   mutate(., 
# #          total_rides = bobsled + flying + inverted + sit_down + 
# #            stand_up + suspended + wing) %>%
# #   .[order(.$total_rides, decreasing = T),]
# # 
# # ggplot() + 
# #   geom_density(data = park_density, 
# #                aes(x = total_rides))+
# #   scale_x_continuous(limits = c(0,NA), 
# #                      breaks = seq(0,1000,by = 5))
# #   
# # 
# # park_inventory %>%
# #   #.[.$ride_status == "Operating Roller Coasters",] %>%
# #   group_by(ride_url, park_url, Type, Design, Scale, ride_status) %>%
# #   summarise() %>%
# #   group_by(park_url, by_var = ride_status, by_color = Scale) %>%
# #   summarise(n = n()) %>%
# #   ggplot(data = ., 
# #          aes(x = by_var,  y = n, fill = (by_color))) +
# #   geom_boxplot()
# 
# # QA ride dates----
# 
# #a.park <- cw_cedarfair$park_name[1]
# 
# qa_ride.dates <- full_join(cf_park_inventory[!colnames(cf_park_inventory) %in% "park_name"], 
#           cw_cedarfair) %>%
#   .[!duplicated(.),] %>%
#   #.[.$park_name == a.park,] %>%
#   # remove rides under construction 
#   .[!.$ride_status %in% c("Roller Coasters Under Construction"),]
# 
# # try based on manual look----
# keep.cols <- c("park_url", "ride_url",
#                "Opened", "Closed", "yr_opened", "yr_closed")
# 
# # create data subset to work with
# dfqa <- qa_ride.dates %>%
#   .[colnames(.) %in% keep.cols]
# 
# # fix problem with 'Opened' and 'Closed' dates ending in '0000'
# dfqa$Opened <- year(ymd(ifelse(nchar(dfqa$Opened)==8, 
#        gsub(pattern = "\\d{4,4}$", "1111", dfqa$Opened), 
#        dfqa$Opened)))
# dfqa$Closed <- year(ymd(ifelse(nchar(dfqa$Closed)==8, 
#        gsub(pattern = "\\d{4,4}$", "1111", dfqa$Closed), 
#        dfqa$Closed)))
# 
# # if Opened and yr_opened don't match...
# dfqa$yro_best <- ifelse(mapply(identical, 
#                                x = as.numeric(dfqa$Opened), 
#                                y = as.numeric(dfqa$yr_opened)), 
#                         yes = as.numeric(dfqa$Opened), 
#                         no  = "no match")
# dfqa$yrc_best <- ifelse(mapply(identical, 
#                                x = as.numeric(dfqa$Closed), 
#                                y = as.numeric(dfqa$yr_closed)), 
#                         yes = as.numeric(dfqa$Closed), 
#                         no  = "no match")
# 
# dfqa <- dfqa[,c("ride_url", "park_url", "yro_best", "yrc_best")]
# 
# 
# # end
# 
# 
# qa_ride.dates <- left_join(qa_ride.dates, dfqa)
# 
# qa_ride.dates %>%
#   group_by(ride_status) %>%
#   summarise(n = n(), 
#             min_yo = min((yro_best)), 
#             max_yo = max((yro_best)),
#             min_yc = min((yrc_best)), 
#             max_yc = max((yrc_best)))
# 
# qa_ride.dates$Name_f <- factor(qa_ride.dates$Name, 
#                                levels = unique(qa_ride.dates$Name[order(qa_ride.dates$yro_best)]))
# 
# ggplot() + 
#   geom_point(data = qa_ride.dates, 
#              aes(x = yro_best, y = Name_f))+
#   geom_point(data = qa_ride.dates, 
#              aes(x = yrc_best, y = Name_f)) +
#   geom_segment(data = qa_ride.dates, 
#                aes(x = yro_best, xend =  yrc_best, 
#                    y = Name_f, yend = Name_f)) +
#   facet_grid(ride_status~., space = "free_y", scales = "free_y")
# 
# 
# # YEARLY INVENTORY----
# cf_park_inventory
# qa_ride.dates
# 
# 
# back_in_time <- function(){
#   # build data set showing how many rides were in a park in any given year
#   
# }
# 
# 
# # TIMELINES----
# 
# quantify_parks <- park_inventory %>%
#   group_by(park_url, ride_status) %>%
#   summarise(n = n_distinct(ride_url))
# 
# quantify_parks %>%
#   mutate(., 
#          ride_status = gsub(" {0,1}Roller Coasters {0,1}", "", 
#                             ride_status)) %>%
#   as.data.table() %>% 
#   dcast(., 
#         park_url ~ ride_status, 
#         value.var = "n", 
#         fun.aggregate = sum, 
#         fill = 0, drop = F) %>%
#   clean_names() %>%
#   as.data.frame(.) %>%
#   mutate(., pct_operating = operating / (defunct + operating + sbno + under_construction)) %>%
#   as_tibble() %>%
#   ggplot(data = .) + 
#   geom_density(aes(x = pct_operating))+
#   labs(title = "<add title>")
# 
# 
# # ride_specs2 <- ride_specs
# # 
# # ride_specs2$Length <- gsub(" ft$", "", ride_specs2$Length) %>%
# #   as.numeric()
# # ride_specs2$Height <- gsub(" ft$", "", ride_specs2$Height) %>%
# #   as.numeric()
# # ride_specs2$Speed <- gsub(" mph$", "", ride_specs2$Speed) %>%
# #   as.numeric()
# # ride_specs2 <- clean_names(ride_specs2)
# # 
# # full_ridelist <- full_join(ride_specs2, park_inventory[,c("ride_url", "Type", "Design", "ride_status", 
# #                   "yr_opened", "yr_closed")] %>%
# #   .[!duplicated(.),]) %>%
# #   .[!duplicated(.),]
# 
# 
# # # fix yearclosed
# # 
# # # remove NAs 
# # full_ridelist <- full_ridelist 
# # 
# # full_ridelist <- full_ridelist[!full_ridelist$yr_opened == 0,]
# # full_ridelist$yr_closed[is.na(full_ridelist$yr_closed) | 
# #                           full_ridelist$yr_closed == 0] <- 0
# # full_ridelist[full_ridelist$yr_closed != 0,]
# # 
# # # remove defunct rides with is.na(yr_closed) | yr_closed == 0 
# # full_ridelist[full_ridelist$ride_status == "Defunct Roller Coasters",]
# # 
# # 
# # full_ridelist[full_ridelist$ride_status == "Operating Roller Coasters",]$yr_closed %>% range()
# # full_ridelist[full_ridelist$ride_status == "Defunct Roller Coasters",]$yr_closed %>% range()
# # full_ridelist[full_ridelist$ride_status == "SBNO Roller Coasters",]$yr_closed %>% range()
# # full_ridelist[full_ridelist$ride_status == "Roller Coasters Under Construction",]$yr_closed %>% range()
# # 
# # full_ridelist %>%
# #   .[.$park_name %in% c("Kings Island"),] %>%
# #   group_by(yr_opened, yr_closed) %>%
# #   summarise(n_sbno = sum(ride_status == "SBNO Roller Coasters"), 
# #             n_defu = sum(ride_status == "Defunct Roller Coasters"), 
# #             n_undc = sum(ride_status == "Roller Coasters Under Construction"), 
# #             n_oper = sum(ride_status == "Operating Roller Coasters")) 
# # 
# # 
# # 
# # full_ridelist$yr_closed %>% .[!grepl("\\d{4,4}", .)]
# # 
# # 
# # ggplot(data = full_ridelist[full_ridelist$yr_opened != 0 & 
# #                               !is.na(full_ridelist$yr_opened),], 
# #        aes(x = yr_opened, y = height)) + 
# #   geom_point()+
# #   geom_smooth()
# # 
# # full_ridelist %>%
# #   clean_names() %>%
# #   .[c("length","height","speed","type","design","ride_status")] %>%
# #   plot()
# # 
# # full_ridelist %>%
# #   .[.$park_name == "Cedar Point" & 
# #       .$ride_name == "Super Coaster" & 
# #       !is.na(.$ride_name),] %>%
# #   ggplot(data = .) +
# #   geom_segment(aes(y = ride_name, yend = ride_name, 
# #                  x = yr_opened, xend = yr_closed)) +
# #   facet_grid(ride_status~., scales = "free_y", space = "free_y")
# # 
# # 
# 
# 
# 
# # NETWORK----
# park_relations <- park_inventory %>%
#   group_by(park_url, ride_url) %>%
#   summarise() %>%
#   ungroup() %>%
#   mutate(., 
#          park_suffix = gsub("^.*\\.com/|\\.htm$", "", park_url), 
#          ride_suffix = gsub("^.*\\.com/|\\.htm$", "", ride_url))
# 
# grid_pr <- park_relations[,c("park_suffix", "ride_suffix")] %>%
#   igraph::graph_from_data_frame(., directed = F)
# 
# 
# asp <- igraph::all_shortest_paths(grid_pr, 
#                            from = unique(park_relations$park_suffix)) 
# 
# View(asp)
# 
# data.frame(park_suffix = park_relations$park_suffix, 
#            asp = asp$nrgeo)
# 
# igraph::distance_table(grid_pr)
# mean_distance(graph = grid_pr) 
# #plot(grid_pr)
# 
# 
# 
# 
# 
