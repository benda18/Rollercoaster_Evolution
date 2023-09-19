# Build Data
library(dplyr)
library(janitor)
library(readr)
library(igraph)



rm(list=ls());cat('\f');gc()

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

# BUILD DATASET OF FILE STRUCTURES ----

# for(i in dat_files){
#   # format variable names
#   temp.varname_long  <- gsub(pattern = "\\.csv", replacement = "", x = i)
#   temp.varname_short <- temp.varname_long %>%
#     strsplit(., "_") %>%
#     unlist() %>%
#     strsplit(., "") %>%
#     lapply(., first) %>%
#     unlist() %>%
#     paste(., sep = "", collapse = "")
#   temp.varname_short <- paste(temp.varname_short, 
#                               "_str", sep = "")
#   # assign to variables
#   assign(temp.varname_short, get(temp.varname_long))
#   # cleanup
#   rm(temp.varname_long,temp.varname_short)
# }

# restructure 
for(i in dat_files){
    temp.varname_long  <- gsub(pattern = "\\.csv", replacement = "", x = i)
    temp.varname_short <- temp.varname_long %>%
      strsplit(., "_") %>%
      unlist() %>%
      strsplit(., "") %>%
      lapply(., first) %>%
      unlist() %>%
      paste(., sep = "", collapse = "")
    temp.varname_short <- paste(temp.varname_short,
                                "_str", sep = "")
  # get var
  temp.var <- data.frame(file_name = i, 
                         colname = names(read_csv(i)))
  assign(temp.varname_short, temp.var)
  # cleanup
  
}

park_inventory
