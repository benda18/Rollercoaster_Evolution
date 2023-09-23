# Build Data
library(dplyr)
library(janitor)
library(readr)
library(ggplot2)
library(lubridate)

rm(list=ls());cat('\f');gc()

# FUNS ----


# pi <- read_csv("park_inventory.csv") %>%
#   .[.$park_name == "kings_island",] %>%
#   .[!.$ride_status == "under_constr",]
# 
# pi <- ride_specs_url_years %>%
#   left_join(.,
#             summarise(group_by_all(park_inventory[,c("park_name", "ride_url", "ride_name")]))) %>%
#   .[.$park_name == "kings_island",] %>%
#   .[!.$ride_status == "under_constr",]






build_the_year2 <- function(yr1 = 1992, 
                            df1 = pi){
  require(data.table)
  require(dplyr)
  bw.out <- NULL
  for(i in 1:nrow(df1)){
    bw.out <- c(bw.out, 
                    data.table::between(x     = yr1, 
                                        lower = df1$yro_best[i],
                                        upper = df1$yrc_best[i], 
                                        NAbounds = NA))
  }
  bw.out <- ifelse(is.na(bw.out), F, bw.out)
  
  df1$year_active <- yr1
  out <- df1[!colnames(df1) %in% c("yro_best", "yrc_best")] %>%
    .[bw.out,] 
 
  return(out)
}


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
rcby2 <- NULL
for(i in 1920:year(Sys.Date())){
  rcby <- rbind(rcby, 
                build_the_year(yr = i, 
                               df.rides = ride_specs_url_years))
  rcby2 <- rbind(rcby2, 
                 build_the_year2(i, 
                                 ride_specs_url_years))
}
rcby
rcby2


rcby <- left_join(rcby,
                  ride_specs_url_years[,c("ride_url", "height.ft", 
                                          "length.ft", "speed.mph", 
                                          "type", "ride_status")]) %>%
  as_tibble() %>%
  left_join(., 
            ungroup(summarise(group_by(park_inventory,park_url, ride_url))))

rcby2 <- rcby2 %>%
  left_join(., 
            ungroup(summarise(group_by(park_inventory,park_url, ride_url))))


rcby
rcby2
  
rcby.bu <- rcby


rcby <- rcby.bu

rcby.summary <- rcby %>%
  #.[.$year_active >= 1920,] %>%
  group_by(year = year_active) %>%
  summarise(n_rides = n_distinct(ride_url), 
            n_parks = n_distinct(park_url), 
            max_height = max(height.ft, na.rm = T), 
            max_length = max(length.ft, na.rm = T), 
            max_speed = max(speed.mph, na.rm = T)) 


breakpts <- data.frame(bp_year = c(1972, 1979, 2000, 2009), 
                       ride = c("racer", "beast", "son_of_beast", "son_of_beast"))


ggplot() + 
  geom_line(data = rcby.summary, 
            aes(x = year, y = max_height), 
            linewidth = 1.2)+
  scale_y_continuous(limits = c(0,NA))+
  labs(title = "Max Height")+
  scale_x_continuous(limits = c(NA, 2030), 
                     breaks = seq(0,3000,by= 10))+
  geom_vline(aes(xintercept = breakpts$bp_year), 
             linetype = 2232, color = "black")


ggplot() + 
  geom_line(data = rcby.summary, 
            aes(x = year, y = max_length), 
            linewidth = 1.2)+
  scale_y_continuous(limits = c(0,NA))+
  labs(title = "Max Length")+
  scale_x_continuous(limits = c(NA, 2030), 
                     breaks = seq(0,3000,by= 10))+
  geom_vline(aes(xintercept = breakpts$bp_year), 
             linetype = 2232, color = "black")


ggplot() + 
  geom_line(data = rcby.summary, 
            aes(x = year, y = max_speed), 
            linewidth = 1.2)+
  scale_y_continuous(limits = c(0,NA))+
  labs(title = "Max Speed")+
  scale_x_continuous(limits = c(NA, 2030), 
                     breaks = seq(0,3000,by= 10))+
  geom_vline(aes(xintercept = breakpts$bp_year), 
             linetype = 2232, color = "black")



# choose a park----

rcby
jb_ridestuff <- park_inventory[,c("ride_name", 
                                  #"type",
                                  "design", "park_name", 
                                  #scale,
                                  #"ride_status", 
                                  "ride_url", "park_url")] %>%
  group_by_all() %>%
  summarise()

master <- left_join(rcby2, jb_ridestuff)
# shiny output 4----
# average length by year by design
SHINY_avg.length_by.design_by.yr <-  master %>%
  group_by(year = year_active, 
           design) %>%
  summarise(avg_length = mean(length.ft, na.rm = T)) 
setwd(wd$output)
write_csv(SHINY_avg.length_by.design_by.yr, 
          "SHINY_avg.length_by.design_by.yr.csv")
setwd(wd$data)

  +ggplot(data = ., aes(x = year, y = avg_length, color = design)) + 
  geom_line()

# shiny output 3----
# wood vs steel

SHINY_wood.vs.steel <- master %>%
  group_by(year =  year_active, 
           type) %>%
  summarise(n_rides = n_distinct(ride_url), 
            n_parks = n_distinct(park_url)) 
setwd(wd$output)
write_csv(SHINY_wood.vs.steel, 
          "SHINY_wood.vs.steel.csv")
setwd(wd$data)
# shiny output 2----
# avg specs by year

SHINY_ride.specs_by.year <- master %>%
  group_by(year = year_active) %>%
  summarise(n_parks = n_distinct(park_url), 
            n_rides = n_distinct(ride_url), 
            rides_w.height = sum(!is.na(height.ft)), 
            rides_w.length = sum(!is.na(length.ft)), 
            rides_w.speed  = sum(!is.na(speed.mph)), 
            avg_height = mean(height.ft, na.rm = T), 
            avg_length = mean(length.ft, na.rm = T), 
            avg_speed = mean(speed.mph, na.rm = T), 
            max_height = max(height.ft, na.rm = T), 
            max_length = max(length.ft, na.rm = T), 
            max_speed = max(speed.mph, na.rm = T), 
            total_length = sum(length.ft, na.rm = T))
setwd(wd$output)
write_csv(SHINY_ride.specs_by.year,"SHINY_ride.specs_by.year.csv")
setwd(wd$data)

ggplot(data = SHINY_ride.specs_by.year, 
       aes(x = year, y = avg_length)) +
  geom_col(aes(fill = avg_length))

# Shiny output 1----
a.park <- "kings_island"

SHINY_ride.design_by.year_by.park <- master %>%
  #.[.$park_name == a.park,] %>%
  group_by(year = year_active, 
           #ride_url, ride_name, 
           #type, 
           design,
           park_url, 
           park_name) %>%
  summarise(n_rides = n_distinct(ride_url)) 

# write to output
setwd(wd$output)
write_csv(x = SHINY_ride.design_by.year_by.park, 
          file = "SHINY_ride.design_by.year_by.park.csv")
setwd(wd$data)

ggplot(data = SHINY_ride.design_by.year_by.park[SHINY_ride.design_by.year_by.park$park_name == a.park,], 
       aes(x = year, y = n_rides, fill = design)) + 
  geom_col(color = "black", 
            position = "stack")
