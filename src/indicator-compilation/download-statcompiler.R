################################################################################
#' @description Download statcompiler
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/util.R")
dat_filename <- list.files("./gen/indicator-compilation/output")
dat_filename <- dat_filename[grepl("indicators_data", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename), 1)
all_dat <- read.csv(paste0("./gen/indicator-compilation/output/", dat_filename, sep = ""))
################################################################################

int_data_subfolder("dhs-statcompiler")

all_dhs_indicator_code <- unique(all_dat$dhs_indicator_code)
all_dhs_indicator_code <- all_dhs_indicator_code[!is.na(all_dhs_indicator_code)]

# download indicators via API
for(dhs_indicator_code in all_dhs_indicator_code) {
  print(" ")
  print(str_glue("==========================================================="))
  print(str_glue("Processing {dhs_indicator_code}"))
  print(str_glue("==========================================================="))
  read_dhs(str_glue("{dhs_indicator_code}"))
}

# append data from all indicators 
all_dat = NULL
for(dhs_indicator_code in all_dhs_indicator_code) {
  print(str_glue("Appending {dhs_indicator_code}"))
  
  indicator_data <- read.csv(
    str_glue("./data/dhs-statcompiler/{dhs_indicator_code}.csv")
  )
  
  indicator_data <- indicator_data[,c("SurveyId", "IndicatorId", "Indicator", "Value", "ByVariableLabel")]
  names(indicator_data) <- c("survey_id", "dhs_indicator_code", "dhs_indicator_description", "value", "ByVariableLabel")
  
  all_dat <- bind_rows(all_dat, indicator_data)
  
}
all_dat <- all_dat[order(all_dat$dhs_indicator_code, all_dat$survey_id, all_dat$ByVariableLabel),]

all_dat <- subset(all_dat, survey_id == "BD2022DHS")

# Save --------------------------------------------------------------------

write.csv(all_dat, paste("./gen/indicator-compilation/audit/statcompiler_",format(Sys.Date(), format="%Y%m%d"),".csv",sep = ""), row.names=FALSE)


