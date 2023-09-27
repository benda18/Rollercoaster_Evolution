# Build Data
library(dplyr)
library(janitor)
library(readr)
library(ggplot2)
library(lubridate)

rm(list=ls());cat('\f');gc()

# FUNS ----
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

# plot03----
setwd(wd$data)
SHINY.ride_specs <- read_csv("ride_specs.csv")
setwd(wd$shiny)

yearly.specs <- ungroup(summarise(group_by(SHINY_park_inventory[SHINY_park_inventory$park_name %in% 
                                                c("kings_island",#, 
                                                  "carowinds",#, 
                                                  "kings_dominion"#,
                                                  #"cedar_point"
                                                  ),], 
                           #input$park_name01,], 
                           park_url, park_name, ride_url, ride_url_f,
                           ride_name, ride_status,
                           type, type_f,
                           design,design_f,
                           scale, scale_f,
                           yro_best, yrc_best))) %>%
  left_join(., 
            ref.park.names[,c("park_name", "Park_Name.facet", "Park_Name")]) %>%
  left_join(., 
            SHINY.ride_specs) %>%
  .[!colnames(.) %in% c("park_url", "ride_url", "type", "design", "scale", 
                        "park_name")] 

yearly.specs2 <- NULL  
for(i in min(c(yearly.specs$yro_best, 
      yearly.specs$yrc_best), 
    na.rm = T):max(c(yearly.specs$yro_best, 
                     yearly.specs$yrc_best), 
                   na.rm = T)){
  yearly.specs2 <- rbind(yearly.specs2, 
                         build_the_year2(yr1 = i, 
                  df1 = yearly.specs))
}

yearly.specs <- full_join(yearly.specs, yearly.specs2)
rm(yearly.specs2)

#yearly.specs$age.yrs <- yearly.specs$year_active - yearly.specs$yro_best

# TODO use this as plot2;
# when just 1 park selected show ride_names, 
# when 2 or more show park_names
yearly.specs %>%
  as.data.table() %>%
  melt(., 
       measure.vars = c("length.ft", "height.ft", "speed.mph")) %>%
  as.data.frame() %>%
  as_tibble() %>%
  group_by(year_active, variable) %>%
  slice_max(order_by = value, n = 1) %>%
  ggplot(data = ., 
         aes(x = year_active, y = value)) +
  #geom_col(aes(fill = Park_Name), position = "dodge")+
  geom_col(aes(fill = ride_name), position = "dodge")+
  facet_grid(variable~., scales = "free_y")+
  scale_y_continuous(name = NULL)+
  theme(text = element_text(size = text.size),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        plot.background = element_rect(color = "black"))+
  labs(title = "Tallest, Longest & Fastest Park by Year")+
  #facet_wrap(~Park_Name.facet, scales = "free_y")+
  scale_fill_discrete(name = "Park Name")+
  scale_x_continuous(name = "Year")

yearly.specs2 <- yearly.specs %>%
  as.data.table() %>%
  melt(., 
       measure.vars = c("length.ft", "height.ft", "speed.mph")) %>%
  as.data.frame() %>%
  as_tibble() %>%
  group_by(year_active, variable) %>%
  slice_max(order_by = value, n = 1) %>%
  group_by(ride_url_f, 
           ride_name, 
           Park_Name, 
           Park_Name.facet, 
           variable) %>%
  summarise(XMIN = min(year_active), 
            XMAX = max(year_active), 
            YMIN = 0, 
            YMAX = max(value))


yearly.specs3 <- NULL
for(i in 1:nrow(yearly.specs2)){
  yearly.specs3 <- rbind(yearly.specs3, 
                         data.frame(shape_id        = i, 
                                    ride_url_f      = yearly.specs2$ride_url_f[i],
                                    ride_name       = yearly.specs2$ride_name[i], 
                                    Park_Name       = yearly.specs2$Park_Name[i],
                                    Park_Name.facet = yearly.specs2$Park_Name.facet[i],
                                    variable        = yearly.specs2$variable[i], 
                                    x1 = c(yearly.specs2$XMIN[i], 
                                           yearly.specs2$XMIN[i], 
                                           yearly.specs2$XMAX[i]+1, 
                                           yearly.specs2$XMAX[i]+1, 
                                           yearly.specs2$XMIN[i]),
                                    y1 = c(yearly.specs2$YMIN[i], 
                                           yearly.specs2$YMAX[i], 
                                           yearly.specs2$YMAX[i], 
                                           yearly.specs2$YMIN[i], 
                                           yearly.specs2$YMIN[i])) ) %>% as_tibble()
}
yearly.specs3




ggplot(data = yearly.specs3) + 
  geom_polygon(aes(x = x1, y = y1, 
                   group = shape_id, 
                   #fill = ride_name)) +
                   fill = Park_Name)) +
  facet_grid(variable~., scales = "free_y") +
  scale_y_continuous(name = NULL)+
  theme(text = element_text(size = text.size),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        plot.background = element_rect(color = "black"))+
  labs(title = "Tallest, Longest & Fastest Park by Year")+
  scale_fill_discrete(name = "Park Name")+
  scale_x_continuous(name = "Year")


# PARK RIDE HEIGHT PLOT EXPLORATION----
SHINY_ride_heights$ride_height_f <- factor(SHINY_ride_heights$ride_height)



temp <- as.data.table(SHINY_ride_heights) %>%
  dcast(., 
        park_name ~ ride_height_f, 
        value.var = "ride_height", fun.aggregate = length, 
        fill = 0) %>%
  melt(., 
       id.vars = "park_name", 
       value.name = "count", 
       variable.name = "height") %>%
  as.data.frame()

# for heights 35:72 what % of rides can you ride at each park? 

master_ride_height <- NULL
for(i_park in unique(SHINY_ride_heights$park_name)){
  for(i_height in seq(36,54,by=2)){
    # how many total rides at park? 
    temp.total_park_rides <- SHINY_ride_heights[SHINY_ride_heights$park_name == i_park,]$ride_url %>%
      unique() %>%
      length()
    # how many rides can be ridden at height i_height? 
    temp.ride_park_rides  <- sum(SHINY_ride_heights[SHINY_ride_heights$park_name == i_park,]$ride_height <= i_height)
    
    master_ride_height <- rbind(master_ride_height, 
                                data.frame(park_name = i_park, 
                                           rider_height = i_height, 
                                           t_park_rides = temp.total_park_rides, 
                                           n_rideable = temp.ride_park_rides))
    
  }
}

master_ride_height <- master_ride_height %>%
  mutate(., 
         pct_rideable = n_rideable / t_park_rides)

optimal_park_by.n_rides <- master_ride_height %>%
  as_tibble() %>%
  group_by(rider_height_f = factor(rider_height)) %>%
  slice_max(., 
            #order_by = n_rideable, 
            order_by = n_rideable,
            n = 1)
optimal_park_by.pct_rides <- master_ride_height %>%
  as_tibble() %>%
  group_by(rider_height_f = factor(rider_height)) %>%
  slice_max(., 
            order_by = pct_rideable, 
            #order_by = pct_rideable,
            n = 1)

ggplot(data = optimal_park_by.pct_rides[optimal_park_by.pct_rides$rider_height < 54,], 
       aes(x = rider_height_f, y = pct_rideable)) + 
  
  geom_col(aes(fill = park_name), position = "dodge", 
           color = "black")+
  theme(legend.position = "bottom", 
        legend.direction = "vertical")+
  scale_y_continuous(name = "Percent of Rides", 
                     labels = scales::percent, 
                     breaks = seq(0,1,by=0.1), 
                     minor_breaks = seq(0,1,by=0.05))+
  scale_x_discrete(name = "Rider Height (inches)")+
  labs(title = "Optimal Park by Rider Height", 
       subtitle = "Percentage of Rides that can be ridden by riders ranging in height from 36 to 54 inches")

ggplot(data = optimal_park_by.n_rides[optimal_park_by.n_rides$rider_height < 54,], 
       aes(x = rider_height_f, y = n_rideable)) + 
  
  geom_col(aes(fill = park_name), position = "dodge", 
           color = "black")+
  theme(legend.position = "bottom", 
        legend.direction = "vertical")+
  scale_y_continuous(name = "Number of Rides", 
                     labels = scales::comma, 
                     breaks = seq(0,100,by=5), 
                     minor_breaks = seq(0,100,by=1))+
  scale_x_discrete(name = "Rider Height (inches)")+
  labs(title = "Optimal Park by Rider Height", 
       subtitle = "Number of Rides that can be ridden by riders ranging in height from 36 to 54 inches")


SHINY_ride_heights %>%
  group_by(park_name, ride_height, ride_height_f) %>%
  summarise(n_rides = n_distinct(ride_url)) %>%
  ggplot(data = ., 
         aes(y = park_name, x = n_rides, fill = ride_height)) + 
  geom_col(position = "fill", color = "white")+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")+
  scale_fill_viridis_c(name = "Rider Height (inches)", 
                       breaks = seq(36,54,by=6), 
                       option = "D")

# SHINY_ride_heights %>%
#   # group_by(park_name) %>%
#   # summarise(avg_ht = mean(ride_height), 
#   #           sd_ht = sd(ride_height)) %>%
#   ggplot(data = ., 
#          aes(x = park_name, y = ride_height, group = park_name)) + 
#   geom_violin(scale = "width", draw_quantiles = 0.5)+
#   #geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
#   scale_y_continuous(breaks = seq(0,100,by=2))

# SHINY_ride_heights %>%
#   group_by(park_name, ride_height) %>%
#   summarise(n_rides = n_distinct(ride_url)) %>%
#   ggplot(data = ., 
#          aes(y = park_name, size = n_rides, x = ride_height)) + 
#   geom_point()+
#   theme(legend.position = "bottom", 
#         legend.direction = "vertical")+
#   scale_size_area()

SHINY_ride_heights2 <- SHINY_ride_heights[SHINY_ride_heights$park_name %in%
                                            c("kings_dominion", 
                                              "kings_island"),]

SHINY_ride_heights2$ride_name_f <- factor(SHINY_ride_heights2$ride_name, 
                                          levels = unique(SHINY_ride_heights2$ride_name[order(SHINY_ride_heights2$park_name)]))
ggplot() + 
  geom_col(data = SHINY_ride_heights2, 
           aes(y = ride_name_f, 
               x = ride_height, 
               fill = park_name), 
           position = "dodge")+
  facet_grid(ride_height_f~., 
             scales = "free_y", 
             space = "free_y")

table(SHINY_ride_heights$ride_height)

SHINY_ride_heights$ride_height_f
View(SHINY_ride_heights)
