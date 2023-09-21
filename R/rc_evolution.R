# roller coaster evolution

library(dplyr)
library(rvest)
library(glue)
library(lubridate)
library(ggplot2)
library(xml2)
library(igraph)
library(data.table)
library(janitor)
library(readr)

rm(list=ls());cat('\f');gc()

# DIRS ----
wd        <- list()
wd$data   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/data"
wd$R      <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/R"
wd$output <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/output"
wd$home   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution"

# FUNS ----
best_open.date <- function(yr.opened, opened){
  require(lubridate)
  v1 <- yr.opened
  v2 <- opened
  
  # create df
  temp.df <- data.frame(v1op       = v1, 
                        v2op       = v2, 
                        asnumbw_v1 = NA, 
                        asnumbw_v2 = NA, 
                        ymdyrbw_v1 = NA, 
                        ymdyrbw_v2 = NA, 
                        n_v1op     = NA, 
                        n_v2op     = NA,
                        v1op.yr    = NA, 
                        v2op.yr    = NA) %>% 
    as_tibble()
  
  # iterate
  for(i in 1:nrow(temp.df)){
    # convert 8-dig values that end in '0000' to '1111'
    temp.df$v1op[i] <- temp.df$v1op[i] %>% gsub(pattern = "0000$", replacement = "1111", x = .)
    temp.df$v2op[i] <- temp.df$v2op[i] %>% gsub(pattern = "0000$", replacement = "1111", x = .)
    
    # convert both to numeric, look for value between 1850 and 2029
    temp.df$asnumbw_v1[i] <- data.table::between(as.numeric(temp.df$v1op[i]),1850,2030)
    temp.df$asnumbw_v2[i] <- data.table::between(as.numeric(temp.df$v2op[i]),1850,2030)
    
    # ymd() %>% year() both, look for value between 1850 and 2029
    temp.df$ymdyrbw_v1[i] <- data.table::between(year(ymd(temp.df$v1op[i])),1850,2030) %>% ifelse(is.na(.), F, .)
    temp.df$ymdyrbw_v2[i] <- data.table::between(year(ymd(temp.df$v2op[i])),1850,2030) %>% ifelse(is.na(.), F, .)
    
    # generate year value for both v1 and v2
    temp.df$v1op.yr[i] <- ifelse(temp.df$asnumbw_v1[i], as.numeric(temp.df$v1op[i]), year(ymd(temp.df$v1op[i])))
    temp.df$v2op.yr[i] <- ifelse(temp.df$asnumbw_v2[i], as.numeric(temp.df$v2op[i]), year(ymd(temp.df$v2op[i])))
    
  }
  
  temp.df <- mutate(temp.df, 
                    n_v1op = asnumbw_v1 + ymdyrbw_v1,
                    n_v2op = asnumbw_v2 + ymdyrbw_v2)
  
  
  temp.df$best <- NA
  temp.df$best <- ifelse(temp.df$v1op.yr == temp.df$v2op.yr, temp.df$v1op.yr, temp.df$best)
  temp.df$best <- ifelse(temp.df$v1op.yr > temp.df$v2op.yr, temp.df$v1op.yr, temp.df$best) # son of beast fails here
  temp.df$best <- ifelse(temp.df$v1op.yr < temp.df$v2op.yr, temp.df$v2op.yr, temp.df$best)
  
  v.out <- temp.df$best
  return(v.out)
}

# IMPORT DATA ----

setwd(wd$data)

dat_files <- list.files(pattern = "park|ride")
for(i in dat_files){
  # format variable name
  temp.varname <- gsub(pattern = "\\.csv", replacement = "", x = i)
  # assign i to variable
  assign(temp.varname,read_csv(i))
  # cleanup
  rm(temp.varname)
}
rm(i,dat_files)


# Use Cedar Fair Parks As Baseline to Gauge Data Accuracy----
setwd(wd$data)

cw_cedarfair <- data.frame(operator_name = c("Cedar Fair"), 
                           operator_url = "https://rcdb.com/12487.htm", 
                           #park_name = c(NA), 
                           park_url = c("4541", 
                                        #"4539",  # canada's wonderland - not in dataset bc outside usa 
                                        4542, 4529, 4588, 4544,
                                        4540,4546,4578,4552,4533)) %>%
  as_tibble()

cw_cedarfair$park_url <- cw_cedarfair$park_url %>%
  paste(., ".htm", sep = "") %>%
  paste("https://rcdb.com/", ., 
        sep = "")

cw_cedarfair <- left_join(cw_cedarfair, 
          park_inventory[,c("park_name", "park_url")] ) %>%
  .[!duplicated(.),]


#cw_cedarfair$park_name[cw_cedarfair$park_url == "https://rcdb.com/4539.htm"] <- "Canada's Wonderland"

cf_park_inventory <- park_inventory %>%
  .[.$park_url %in% cw_cedarfair$park_url,]

cf_ride_specs <- ride_specs %>%
  .[.$ride_url %in% cf_park_inventory$ride_url,]

# Confirm parks are in the base data
cf_park_inventory %>%
  group_by(park_url) %>%
  summarise(n_rides = n_distinct(Name, na.rm = T)) %>%
  full_join(., 
            cw_cedarfair)




# QA ride dates----

a.park <- "Kings Island"

qa_ride.dates <- full_join(cf_park_inventory[!colnames(cf_park_inventory) %in% "park_name"], 
          cw_cedarfair) %>%
  .[!duplicated(.),] %>%
  .[.$park_name == a.park,]

qa_ride.dates$yro_best <- best_open.date(yr.opened = qa_ride.dates$yr_opened, 
               opened = qa_ride.dates$Opened)
qa_ride.dates$yrc_best <- best_open.date(yr.opened = qa_ride.dates$yr_closed, 
                                         opened = qa_ride.dates$Closed)


qa_ride.dates$Name_f <- factor(qa_ride.dates$Name, 
                               levels = unique(qa_ride.dates$Name[order(qa_ride.dates$yro_best)]))

View(qa_ride.dates)

ggplot() + 
  geom_point(data = qa_ride.dates, 
             aes(x = yro_best, y = Name_f))+
  geom_point(data = qa_ride.dates, 
             aes(x = yrc_best, y = Name_f)) +
  geom_segment(data = qa_ride.dates, 
               aes(x = yro_best, xend =  yrc_best, 
                   y = Name_f, yend = Name_f)) +
  facet_grid(ride_status~., space = "free_y", scales = "free_y")

# TIMELINES----

quantify_parks <- park_inventory %>%
  group_by(park_url, ride_status) %>%
  summarise(n = n_distinct(ride_url))

quantify_parks %>%
  mutate(., 
         ride_status = gsub(" {0,1}Roller Coasters {0,1}", "", 
                            ride_status)) %>%
  as.data.table() %>% 
  dcast(., 
        park_url ~ ride_status, 
        value.var = "n", 
        fun.aggregate = sum, 
        fill = 0, drop = F) %>%
  clean_names() %>%
  as.data.frame(.) %>%
  mutate(., pct_operating = operating / (defunct + operating + sbno + under_construction)) %>%
  as_tibble() %>%
  ggplot(data = .) + 
  geom_density(aes(x = pct_operating))+
  labs(title = "<add title>")


ride_specs2 <- ride_specs

ride_specs2$Length <- gsub(" ft$", "", ride_specs2$Length) %>%
  as.numeric()
ride_specs2$Height <- gsub(" ft$", "", ride_specs2$Height) %>%
  as.numeric()
ride_specs2$Speed <- gsub(" mph$", "", ride_specs2$Speed) %>%
  as.numeric()
ride_specs2 <- clean_names(ride_specs2)

full_ridelist <- full_join(ride_specs2, park_inventory[,c("ride_url", "Type", "Design", "ride_status", 
                  "yr_opened", "yr_closed")] %>%
  .[!duplicated(.),]) %>%
  .[!duplicated(.),]


# fix yearclosed

# remove NAs 
full_ridelist <- full_ridelist 

full_ridelist <- full_ridelist[!full_ridelist$yr_opened == 0,]
full_ridelist$yr_closed[is.na(full_ridelist$yr_closed) | 
                          full_ridelist$yr_closed == 0] <- 0
full_ridelist[full_ridelist$yr_closed != 0,]

# remove defunct rides with is.na(yr_closed) | yr_closed == 0 
full_ridelist[full_ridelist$ride_status == "Defunct Roller Coasters",]


full_ridelist[full_ridelist$ride_status == "Operating Roller Coasters",]$yr_closed %>% range()
full_ridelist[full_ridelist$ride_status == "Defunct Roller Coasters",]$yr_closed %>% range()
full_ridelist[full_ridelist$ride_status == "SBNO Roller Coasters",]$yr_closed %>% range()
full_ridelist[full_ridelist$ride_status == "Roller Coasters Under Construction",]$yr_closed %>% range()

full_ridelist %>%
  .[.$park_name %in% c("Kings Island"),] %>%
  group_by(yr_opened, yr_closed) %>%
  summarise(n_sbno = sum(ride_status == "SBNO Roller Coasters"), 
            n_defu = sum(ride_status == "Defunct Roller Coasters"), 
            n_undc = sum(ride_status == "Roller Coasters Under Construction"), 
            n_oper = sum(ride_status == "Operating Roller Coasters")) 



full_ridelist$yr_closed %>% .[!grepl("\\d{4,4}", .)]


ggplot(data = full_ridelist[full_ridelist$yr_opened != 0 & 
                              !is.na(full_ridelist$yr_opened),], 
       aes(x = yr_opened, y = height)) + 
  geom_point()+
  geom_smooth()

full_ridelist %>%
  clean_names() %>%
  .[c("length","height","speed","type","design","ride_status")] %>%
  plot()

full_ridelist %>%
  .[.$park_name == "Cedar Point" & 
      .$ride_name == "Super Coaster" & 
      !is.na(.$ride_name),] %>%
  ggplot(data = .) +
  geom_segment(aes(y = ride_name, yend = ride_name, 
                 x = yr_opened, xend = yr_closed)) +
  facet_grid(ride_status~., scales = "free_y", space = "free_y")





# NETWORK----
park_relations <- park_inventory %>%
  group_by(park_url, ride_url) %>%
  summarise() %>%
  ungroup() %>%
  mutate(., 
         park_suffix = gsub("^.*\\.com/|\\.htm$", "", park_url), 
         ride_suffix = gsub("^.*\\.com/|\\.htm$", "", ride_url))

grid_pr <- park_relations[,c("park_suffix", "ride_suffix")] %>%
  igraph::graph_from_data_frame(., directed = F)


asp <- igraph::all_shortest_paths(grid_pr, 
                           from = unique(park_relations$park_suffix)) 

View(asp)

data.frame(park_suffix = park_relations$park_suffix, 
           asp = asp$nrgeo)

igraph::distance_table(grid_pr)
mean_distance(graph = grid_pr) 
#plot(grid_pr)





