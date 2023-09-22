# data_exceptions

library(dplyr)
library(data.table)
library(janitor)
library(readr)

#rm(list=ls());cat('\f');gc()

# DIRS ----
wd        <- list()
wd$data   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/data"
wd$R      <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/R"
wd$output <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/output"
wd$home   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution"


# park_inventory.csv fixes----
setwd(wd$data)

gen_re <- function(r.url, 
                   ex.type,
                   ex.cat = "ride_exception"){
  as_tibble(data.frame(ride_url = r.url, 
             exception_cat = ex.cat, 
             exception_type = ex.type))
}



ride_exceptions <- rbind(gen_re(c("https://rcdb.com/1834.htm", 
                                  "https://rcdb.com/4003.htm",
                                  "https://rcdb.com/2379.htm"), 
                              "never_operated"), 
                         gen_re(c("https://rcdb.com/675.htm", 
                                  "https://rcdb.com/12735.htm"), 
                                "open_date_unknown"), 
                         gen_re("https://rcdb.com/2379.htm", 
                                "parts_donor"), 
                         gen_re("https://rcdb.com/12735.htm", 
                                "closed_date_unknown"))

write_csv(x = ride_exceptions, 
          file = "ride_exceptions.csv")
