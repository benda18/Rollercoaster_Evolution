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

rm(list=ls());cat('\f');gc()

# DIRS ----
wd        <- list()
wd$data   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/data"
wd$R      <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/R"
wd$output <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/output"
wd$home   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution"

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





