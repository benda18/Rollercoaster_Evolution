# Build Data
library(dplyr)
library(janitor)
library(readr)
library(ggplot2)
library(lubridate)

rm(list=ls());cat('\f');gc()

# FUNS ----

# Vars----
plot.height <- 750
plot.width  <- 1100
text.size   <- 18

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

substr(x = ref.park.names$park_name, 1, 6) %>%
  table() %>% 
  sort()

grep("^nick", x = ref.park.names$park_name, ignore.case = T, value = T)

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

ref.park.names  



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

# park trackage by material 
# top year by design

# ride x year_constructed x height

# ride_lifespans x park as segment

