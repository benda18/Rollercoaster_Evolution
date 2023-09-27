# Build Data
library(dplyr)
library(janitor)
#library(here)  # project-oriented workflow // '??here'
library(rvest)
library(xml2)
library(glue)
library(lubridate)
#library(ggplot2)
library(readr)


rm(list=ls());cat('\f');gc()

# DIRS ----
wd        <- list()
wd$data   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/data"
wd$R      <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/R"
wd$output <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/output"
wd$home   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution"
wd$shiny  <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/shiny/rollercoasters"

# cedar fair ride experiences urls----
setwd(wd$data)


cf_ride_exp <- data.frame(park_name = c("kings_island", 
                                        "cedar_point", 
                                        "carowinds", 
                                        "kings_dominion", 
                                        "michigans_adventure", 
                                        "worlds_of_fun", 
                                        "californias_great_america", 
                                        "knotts_berry_farm", 
                                        "valleyfair", 
                                        "dorney_park_and_wildwater_kingdom"), 
                          park_web = c("https://www.visitkingsisland.com/rides-experiences", 
                                       "https://www.cedarpoint.com/rides-experiences", 
                                       "https://www.carowinds.com/rides-experiences", 
                                       "https://www.kingsdominion.com/rides-experiences", 
                                       "https://www.miadventure.com/rides-experiences", 
                                       "https://www.worldsoffun.com/rides-experiences", 
                                       "https://www.cagreatamerica.com/rides-experiences", 
                                       "https://www.knotts.com/rides-experiences", 
                                       "https://www.valleyfair.com/rides-experiences", 
                                       "https://www.dorneypark.com/rides-experiences")) %>%
  as_tibble()

# read_csv("cf_6f_parkridehistory.csv")$park_name %>% unique() %>%
#   .[!grepl("^six", .)] %>%
#   .[!. %in% cf_ride_exp$park_name]

park_inventory <- read_csv("park_inventory.csv") %>%
  .[.$park_name %in% cf_ride_exp$park_name,] %>%
  .[.$ride_status == "operating",] %>%
  .[,c("park_name", "ride_name", "ride_url", "park_url")]

cf_ride_exp <- full_join(cf_ride_exp, park_inventory ) %>%
  mutate(., Ride_Name = NA)

#https://stackoverflow.com/questions/11672050/how-to-convert-not-camel-case-to-camelcase-in-r
cf_ride_exp$Ride_Name <- cf_ride_exp$ride_name %>%
  gsub("_", " ", .) %>%
  sub('^(\\w?)', '\\U\\1', ., perl=T) %>%
  gsub('\\ (\\w?)', '\\U \\1', ., perl=T)

rm(park_inventory)

# assign ride height----
cf_ride_exp$ride_height <- NA

# 36 inch----
cf_ride_exp[cf_ride_exp$ride_name %in% 
              c("lucys_crabbie_cabbies",
                "great_pumpkin_coaster", 
                "the_great_pumpkin_coaster"),]$ride_height <- 36

# 40 inch----
# cf_ride_exp[cf_ride_exp$ride_name %in% 
#               c(""),]$ride_height <- 40

# 42 inch----
# cf_ride_exp[cf_ride_exp$ride_name %in% 
#               c(""),]$ride_height <- 42

# 44 inch----
cf_ride_exp[cf_ride_exp$ride_name %in% 
              c("flying_ace_aerial_chase", 
                "kiddy_hawk", 
                "ricochet"),]$ride_height <- 44

# 46 inch----
cf_ride_exp[cf_ride_exp$ride_name %in% 
              c("reptilian", 
                "zachs_zoomer"),]$ride_height <- 46

# 48 inch----
cf_ride_exp[cf_ride_exp$ride_name %in% 
              c("anaconda",
                "corkscrew", "excalibur", "high_roller",  "renegade", "wild_thing",
                "demon", "gold_striker", "psycho_mouse", "railblazer", "grizzly", "the_grizzly",
                "boomerang", "mamba", "prowler", "spinning_dragons", "timber_wolf", "zambezi_zinger",
                "corkscrew", "shivering_timbers", "wolverine_wildcat",
                "backlot_stunt_coaster", 
                "grizzly", 
                "racer_75", 
                "tumbili", 
                "twisted_timbers", 
                "adventure_express", 
                "mystic_timbers", 
                "surf_dog", 
                "bat", 
                "beast", 
                "racer", 
                "carolina_cyclone", 
                "carolina_goldrusher", 
                "hurler", 
                "flying_cobras", 
                "blue_streak", 
                "cedar_creek_mine_ride", 
                "corkscrew", 
                "gemini", 
                "iron_dragon", 
                "magnum_xl-200", 
                "magnum_xl_200", 
                "steel_force",
               # "thunderhawk",
                "magnum_xl200", 
                "ghostrider",
                "hangtime",
                "jaguar",
                "pony_express",
                "sierra_sidewinder",
                "millennium_force", 
                "wild_mouse"),]$ride_height <- 48

# 52 inch----
cf_ride_exp[cf_ride_exp$ride_name %in% 
              c("banshee", 
                "possessed",
                "steel_venom",
                #"thunderhawk",
                "copperhead_strike", 
                "gatekeeper", 
                "maverick", 
                "steel_vengeance", 
                "xcelerator_the_ride",
                "valravn"),]$ride_height <- 52
#54 inch----
cf_ride_exp[cf_ride_exp$ride_name %in% 
              c("apple_zapple", 
                "hydra_the_revenge", "talon",
                "coast_rider",
                "flight_deck",
                "silver_bullet",
                "dominator", 
                "flight_of_fear",
                "intimidator_305", 
                "diamondback", 
                "invertigo", 
                "orion", 
                "afterburn", 
                "fury_325", 
                "intimidator", 
                "nighthawk", 
                "vortex", 
                "raptor", 
                "patriot", # both mi and ca
                "rougarou"),]$ride_height <- 54


# shared names----
woodstock.xp <- c(ki = 40, kd = 46, car = 46, cp = 36, ma = 36, cga = 40)
wildernessrun.xp <- c(car = 40, cp = 36)



cf_ride_exp[cf_ride_exp$ride_name == "thunderhawk" & 
              cf_ride_exp$park_name == "dorney_park_and_wildwater_kingdom",]$ride_height <- 48
cf_ride_exp[cf_ride_exp$ride_name == "thunderhawk" & 
              cf_ride_exp$park_name == "michigans_adventure",]$ride_height <- 52 # max = 78


cf_ride_exp[cf_ride_exp$ride_name == "mad_mouse" & 
              cf_ride_exp$park_name == "michigans_adventure",]$ride_height <- 44
cf_ride_exp[cf_ride_exp$ride_name == "mad_mouse" & 
              cf_ride_exp$park_name == "valleyfair",]$ride_height <- 44

cf_ride_exp[cf_ride_exp$ride_name == "cosmic_coaster" & 
              cf_ride_exp$park_name == "valleyfair",]$ride_height <- 36
cf_ride_exp[cf_ride_exp$ride_name == "cosmic_coaster" & 
              cf_ride_exp$park_name == "worlds_of_fun",]$ride_height <- 42


cf_ride_exp[cf_ride_exp$ride_name == "wild_mouse" & 
              cf_ride_exp$park_name == "dorney_park_and_wildwater_kingdom",]$ride_height <- 44
cf_ride_exp[cf_ride_exp$ride_name == "woodstock_express" & 
              cf_ride_exp$park_name == "dorney_park_and_wildwater_kingdom",]$ride_height <- 36
cf_ride_exp[cf_ride_exp$ride_name == "woodstock_express" & 
              cf_ride_exp$park_name == "kings_island",]$ride_height <- 40
cf_ride_exp[cf_ride_exp$ride_name == "woodstock_express" & 
              cf_ride_exp$park_name == "kings_dominion",]$ride_height <- 46
cf_ride_exp[cf_ride_exp$ride_name == "woodstock_express" & 
              cf_ride_exp$park_name == "carowinds",]$ride_height <- 46
cf_ride_exp[cf_ride_exp$ride_name == "woodstock_express" & 
              cf_ride_exp$park_name == "cedar_point",]$ride_height <- 36
cf_ride_exp[cf_ride_exp$ride_name == "woodstock_express" & 
              cf_ride_exp$park_name == "michigans_adventure",]$ride_height <- 36
cf_ride_exp[cf_ride_exp$ride_name == "woodstock_express" & 
              cf_ride_exp$park_name == "californias_great_america",]$ride_height <- 40
cf_ride_exp[cf_ride_exp$ride_name == "wilderness_run" & 
              cf_ride_exp$park_name == "carowinds",]$ride_height <- 40
cf_ride_exp[cf_ride_exp$ride_name == "wilderness_run" & 
              cf_ride_exp$park_name == "cedar_point",]$ride_height <- 36


cf_ride_exp$ride_height[cf_ride_exp$ride_name %in% c("woodstock_express")] <- 36

cf_ride_exp[is.na(cf_ride_exp$ride_height),]

dup.ride.names <- cf_ride_exp %>%
  group_by(ride_name) %>%
  summarise(n_park = n_distinct(park_name)) %>%
  .[.$n_park > 1,] %>%
  .$ride_name

cf_ride_exp[cf_ride_exp$ride_name %in% dup.ride.names,] %>%
  group_by(ride_name, park_name, ride_height) %>%
  summarise() %>% as.data.frame()


# save to file
setwd(wd$output)
write_csv(cf_ride_exp, 
          "SHINY_ride_heights.csv")
setwd(wd$R)
