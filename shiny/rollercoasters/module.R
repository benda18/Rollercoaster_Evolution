# Build Data
library(dplyr)
library(janitor)
library(readr)
library(ggplot2)
library(lubridate)

rm(list=ls());cat('\f');gc()

# FUNS ----

# Vars----
var_size.factor <- 1.7

plot.height <- floor(750/var_size.factor)
plot.width  <- floor(1100/1)
text.size   <- 16 #18

# DIRS ----
wd        <- list()
wd$data   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/data"
wd$R      <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/R"
wd$output <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/output"
wd$home   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution"
wd$shiny  <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/shiny/rollercoasters"

setwd(wd$home)

setwd(wd$output)

# load SHINY_xxx.csv data----
setwd(wd$output)

shiny.files <- list.files(pattern = "^SHINY_")
for(i in shiny.files){
  assign(gsub(pattern = ".csv", "", i),read_csv(i))
}
rm(i,shiny.files)

# Build dataset of park names one time----

# SHINY_avg.length_by.design_by.yr
# SHINY_ride.specs_by.year
# SHINY_avg.length_by.design_by.yr

ref.park.names <- SHINY_ride.design_by.year_by.park %>%
  group_by(park_name, year) %>%
  summarise(t_rides = sum(n_rides)) %>%
  ungroup()%>%
  group_by(park_name) %>%
  slice_max(., 
            order_by = year, n = 1)


ref.park.names$Park_Name <- ref.park.names$park_name %>%
  gsub("_", " ", .) 

for(i in 1:nrow(ref.park.names)){
  temp.name <- ref.park.names$Park_Name[i] %>%
    strsplit(., " ") %>%
    unlist()
  temp.words <- temp.name %>%
    strsplit(., "")
  
  for(i2 in 1:length(temp.words)){
    temp.words[[i2]][1] <- toupper(temp.words[[i2]][1])
  }
  
  ref.park.names$Park_Name[i] <- lapply(temp.words, paste, sep = "", collapse = "") %>%
    unlist() %>%
    paste(., sep = " ", collapse = " ")
  rm(temp.name,temp.words)
  
}
rm(i,i2)

# manual fixes
ref.park.names$Park_Name <- gsub(pattern = " of ", 
                                 replacement = " of ", 
                                 x = ref.park.names$Park_Name, 
                                 ignore.case = T)

ref.park.names$Park_Name  <- ref.park.names$Park_Name %>%
  gsub("Californias", "California's", .) %>%
  gsub(" And ", " and ", .) %>%
  gsub("Knoebels", "Knoebel's", .) %>%
  gsub("Moreys", "Morey's", .) %>%
  gsub("Usa", "USA", .)

setwd(wd$data)
# load park operator info
park_operator.parital <- read_csv("cf_6f_parkridehistory.csv") %>%
  group_by(park_name, park_operator) %>%
  summarise()
setwd(wd$shiny)

ref.park.names <- left_join(ref.park.names, 
                            park_operator.parital) %>%
  mutate(., 
         park_operator = ifelse(is.na(park_operator), "other", 
                                park_operator))
ref.park.names$park_operator[grepl("^Busch ", x = ref.park.names$Park_Name)] <- "Busch Gardens"
ref.park.names$park_operator[grepl("Legoland ", x = ref.park.names$Park_Name)] <- "LegoLand"
ref.park.names$park_operator[grepl("^Nickelodeon ", x = ref.park.names$Park_Name)] <- "Nickelodeon"
ref.park.names$park_operator[grepl("^Fun Spot ", x = ref.park.names$Park_Name)] <- "Fun Spot"
ref.park.names$park_operator[grepl("^Universal ", x = ref.park.names$Park_Name)] <- "Universal Studios"
ref.park.names$park_operator[grepl("Seaworld", x = ref.park.names$Park_Name)] <- "SeaWorld"

# substr(x = ref.park.names$park_name, 1, 6) %>%
#   table() %>% 
#   sort()

# grep("^nick", x = ref.park.names$park_name, ignore.case = T, value = T)

rm(park_operator.parital)


ref.park.names$park_operator <- ref.park.names$park_operator %>%
  gsub(" ", "_", .) %>%
  tolower()

# make Operator_Name
ref.park.names$Operator_Name <- ref.park.names$park_operator 
ref.park.names$Operator_Name <- ref.park.names$Operator_Name %>%
  gsub("_", " ", .) 

for(i in 1:nrow(ref.park.names)){
  temp.name <- ref.park.names$Operator_Name[i] %>%
    strsplit(., " ") %>%
    unlist()
  temp.words <- temp.name %>%
    strsplit(., "")
  
  for(i2 in 1:length(temp.words)){
    temp.words[[i2]][1] <- toupper(temp.words[[i2]][1])
  }
  
  ref.park.names$Operator_Name[i] <- lapply(temp.words, paste, sep = "", collapse = "") %>%
    unlist() %>%
    paste(., sep = " ", collapse = " ")
  rm(temp.name,temp.words)
  
}
rm(i,i2)

# add Operator_Name in parentheses to park_name for ease of selecting 
ref.park.names$Operator_Name %>% unique()

ref.park.names$Park_Name <- paste(ref.park.names$Park_Name, 
      " (",
      ref.park.names$Operator_Name,
      ")", 
      sep = "") %>%
   gsub(" \\(Other\\)$", "", .) %>%
  gsub(" \\(Universal Studios\\)$", "", .) %>%
  gsub(" \\(Seaworld\\)$", "", .) %>%
  gsub(" \\(Legoland\\)$", "", .) %>%
  gsub(" \\(Fun Spot\\)$", "", .) %>%
  gsub(" \\(Nickelodeon\\)$", "", .)%>%
  gsub(" \\(Busch Gardens\\)$", "", .) %>%
  gsub(" \\(Six Flags\\)$", "", .)

ref.park.names$Park_Name.facet <- ref.park.names$Park_Name %>% 
#"Dorney Park and Wildwater Kingdom (Cedar Fair)" %>%
strwrap(x = ., 
        width = 22, 
        prefix = "", 
        initial = "", 
        simplify = F) %>%
  lapply(., paste, sep = "\n", collapse = "\n") %>%
  unlist() %>% trimws

ref.park.names$Park_Name.facet <- gsub(" \\(Cedar\nFair\\)$", "\n(Cedar Fair)", 
     ref.park.names$Park_Name.facet)

# make list of park names
park.names.list <- as.list(ref.park.names$park_name)
names(park.names.list) <- ref.park.names$Park_Name

# secondary plot ideas----
SHINY_avg.length_by.design_by.yr
SHINY_ride.specs_by.year
SHINY_avg.length_by.design_by.yr
SHINY_wood.vs.steel

# create design_f in SHINY_ride.design_by.year_by.park
# SHINY_ride.design_by.year_by.park$design_f <- SHINY_ride.design_by.year_by.park$design %>% 
#   as.factor()

SHINY_ride.design_by.year_by.park$design_f <- factor(SHINY_ride.design_by.year_by.park$design, 
                                                     levels = rev(unique(SHINY_ride.design_by.year_by.park$design[order(SHINY_ride.design_by.year_by.park$year)])))

# SHINY_ride.design_by.year_by.park$design_f <- relevel(SHINY_ride.design_by.year_by.park$design_f, 
#         ref = "sit_down")



# park trackage by material 
# top year by design

# ride x year_constructed x height

# ride_lifespans x park as segment

setwd(wd$data)
SHINY_park_inventory <- read_csv("park_inventory.csv")
setwd(wd$shiny)

 

# add end_year for rides that are still open
SHINY_park_inventory$yrc_best[is.na(SHINY_park_inventory$yrc_best) & 
                                SHINY_park_inventory$ride_status %in% 
                                c("operating", "sbno")] <- year(Sys.Date())

# factorize----
# ride_url_f
SHINY_park_inventory$ride_url_f <- factor(SHINY_park_inventory$ride_url, 
                                          levels = unique(SHINY_park_inventory$ride_url[order(SHINY_park_inventory$yro_best, 
                                                                                              decreasing = T)]))
SHINY_park_inventory$design_f <- factor(SHINY_park_inventory$design, 
                                        levels = unique(SHINY_park_inventory$design[order(SHINY_park_inventory$yro_best, 
                                                                                            decreasing = T)]))
SHINY_park_inventory$scale_f <- factor(SHINY_park_inventory$scale, 
                                       levels = c(NA, "kiddie", "family", "thrill", "extreme"))

SHINY_park_inventory$type_f <- factor(SHINY_park_inventory$type, 
                                      levels = c("wood", "steel"))
# remove rides under construction----
SHINY_park_inventory <- SHINY_park_inventory[!SHINY_park_inventory$ride_status == "under_constr",]



# # plot2----
# SHINY_park_inventory %>%
#   #.[.$park_name == a.park,] %>%
#   group_by(park_url, park_name, ride_url, ride_url_f,
#            ride_name, ride_status,
#            type, type_f,
#            design,design_f,
#            scale, scale_f,
#            yro_best, yrc_best) %>%
#   summarise() %>%
#   ungroup() %>%
#   ggplot(data = ., 
#          aes(color = scale_f)) + 
#   geom_segment(aes(y = ride_url_f, yend = ride_url_f, 
#                    x = yro_best, xend = yrc_best))+
#   geom_point(aes(x = yro_best, y = ride_url_f))+
#   scale_y_discrete(breaks = SHINY_park_inventory$ride_url,#[SHINY_park_inventory$park_name %in% a.park],
#                    labels = SHINY_park_inventory$ride_name)#[SHINY_park_inventory$park_name %in% a.park,]$ride_name)
# 





# geom_segment showing range of height of rides installed in a given year
# add ride specs
setwd(wd$data)
SHINY_ride_specs <-read_csv("ride_specs.csv")
setwd(wd$shiny)

SHINY_park_inventory %>%
  .[.$park_name %in% c("kings_island"),] %>%
  inner_join(., 
            SHINY_ride_specs[!is.na(SHINY_ride_specs$height.ft),
                             c("ride_url", "height.ft")]) %>%
  #.[complete.cases(.),] %>%
  .[.$yro_best >= 1960,] %>%
  group_by(yro_best) %>%
  summarise(max_ht = max(height.ft, na.rm = T), 
            #min_ht = min(height.ft, na.rm = T), 
            n = n_distinct(ride_url)) %>%
  ggplot(data = .) +
  geom_col(aes(x = yro_best, y = max_ht, 
               fill = n))+
  # geom_segment(aes(x = yro_best, xend = yro_best, 
  #                  y = min_ht, yend = max_ht, 
  #                  color = n),
  #              linewidth =3)+
  scale_fill_viridis_c(option = "C")
  