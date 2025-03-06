################################################################################
#' @description Compile indicator data
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(haven)
library(tidyverse)
library(survey)
#' Inputs
source("./src/util.R")

################################################################################

all_indicators <- list.files(path = "./src/indicators", pattern = "*.R") %>%
  map(tools::file_path_sans_ext)

all_indicators <- all_indicators[-1]

all_data = NULL
for(indicator_name in all_indicators) {
  print(str_glue("Appending {indicator_name}"))
  
  indicator_data <- read.csv(
    str_glue("./gen/indicators/output/{indicator_name}_data.csv")
  )
  
  indicator_metadata <- jsonlite::read_json(str_glue("./gen/indicators/output/{indicator_name}_metadata.json"))
  indicator_data$variable <- paste(indicator_data$indicator)
  indicator_data <- indicator_data %>% relocate(any_of(c("variable")), .after = indicator)
  
  all_data <- bind_rows(all_data, indicator_data)
}

# merge on region names from BD shape files


# Save --------------------------------------------------------------------

write.csv(all_data, paste0("./gen/indicator-compilation/output/indicators_data_",format(Sys.Date(), format="%Y%m%d"),".csv"), 
          row.names = FALSE)
