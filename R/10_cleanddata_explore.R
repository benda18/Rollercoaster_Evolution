# Build Data
library(dplyr)
library(janitor)
library(readr)
library(ggplot2)
library(lubridate)

rm(list=ls());cat('\f');gc()

# FUNS ----
build_the_year <- function(yr, df.rides){
  df.rides$yrc_best[is.na(df.rides$yrc_best) & 
                      df.rides$ride_status != "under_construction"] <- year(Sys.Date())
  # remove under construction rides
  df.rides <- df.rides[!df.rides$ride_status %in% 
                         c("under_construction"),]# "sbno"),]
  
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
rm(i,dat_files)


# superlative  Roller Coasters in USA throughout the years----

ride_specs_url_years <- ride_specs[,c("ride_url", "height.ft", "length.ft", "speed.mph")] %>%
  left_join(., 
            park_inventory[,c("ride_url", "yro_best", "yrc_best", 
                              "type", "ride_status")]) %>%
  # remove rides under construction
  .[.$ride_status != "under_constr",]

# set yrc_best for rides still operating----
ride_specs_url_years$yrc_best[ride_specs_url_years$ride_status %in% 
                                c("operating", "sbno")] <- year(Sys.Date())

# remove rides missing yro_best
ride_specs_url_years <- ride_specs_url_years[!is.na(ride_specs_url_years$yro_best),]

# remove rides missing yrc_best that are defunct
ride_specs_url_years <- ride_specs_url_years[!(ride_specs_url_years$ride_status == "defunct" & 
                                                   is.na(ride_specs_url_years$yrc_best)),]


# PREPPED DATA----
# rollercoasters by-year
rcby <- NULL
for(i in min(range(c(ride_specs_url_years$yrc_best, 
                     ride_specs_url_years$yro_best),
                   na.rm = T)):max(range(c(ride_specs_url_years$yrc_best, 
                                           ride_specs_url_years$yro_best), 
                                         na.rm = T))){
  rcby <- rbind(rcby, 
                build_the_year(yr = i, 
                               df.rides = ride_specs_url_years))
}

rcby <- left_join(rcby,
                  ride_specs_url_years[,c("ride_url", "height.ft", 
                                          "length.ft", "speed.mph", 
                                          "type", "ride_status")]) %>%
  as_tibble() %>%
  left_join(., 
            ungroup(summarise(group_by(park_inventory,park_url, ride_url))))

  
rcby.bu <- rcby


rcby <- rcby.bu

rcby.summary <- rcby %>%
  .[.$year_active >= 1920,] %>%
  group_by(year = year_active) %>%
  summarise(n_rides = n_distinct(ride_url), 
            n_parks = n_distinct(park_url), 
            max_height = max(height.ft, na.rm = T), 
            max_length = max(length.ft, na.rm = T), 
            max_speed = max(speed.mph, na.rm = T)) 

plot(rcby)
