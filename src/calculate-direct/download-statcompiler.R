################################################################################
#' @description Download statcompiler compiled DHS indicator statistics
#' @return Spreadsheet with national-level values for indicators of interest
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/util.R")
direct <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
################################################################################

all_dhs_indicator_code <- unique(direct$dhs_indicator_code)
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

# only keep BD2022DHS
all_dat <- subset(all_dat, survey_id == "BD2022DHS")

# only keep one of each indicator
all_dat %>%
  group_by(dhs_indicator_code) %>%
  summarise(n = n()) %>% filter(n > 1)
# Drop
all_dat_samp <- all_dat %>%
  filter(!(dhs_indicator_code == "CH_DIAT_C_ORS" & ByVariableLabel != "Five years preceding the survey")) %>%
  filter(!(dhs_indicator_code == "CH_DIAT_C_ZNC" & ByVariableLabel != "Five years preceding the survey")) %>%
  filter(!(dhs_indicator_code == "RH_ANCN_W_N4P" & ByVariableLabel != "Three years preceding the survey")) %>% # my indicator is for 5 years
  filter(!(dhs_indicator_code == "RH_ANCS_W_BLP" & (ByVariableLabel != "Three years preceding the survey" |
             dhs_indicator_description != "Antenatal care content (of ANC): Blood pressure measured"))) %>%
  filter(!(dhs_indicator_code == "RH_ANCS_W_BLS" & (ByVariableLabel != "Three years preceding the survey" |
            dhs_indicator_description != "Antenatal care content (of ANC): Blood sample taken"))) %>%
  filter(!(dhs_indicator_code == "RH_ANCS_W_URN" & (ByVariableLabel != "Three years preceding the survey" |
            dhs_indicator_description != "Antenatal care content (of ANC): Urine sample taken"))) %>% 
  filter(!(dhs_indicator_code == "RH_ANCS_W_WGT" & (ByVariableLabel != "Three years preceding the survey" |
            dhs_indicator_description != "Antenatal care content (of ANC): Weight measured"))) %>% 
  filter(!(dhs_indicator_code == "RH_DELA_C_SKP" & ByVariableLabel != "Three years preceding the survey")) %>%
  filter(!(dhs_indicator_code == "RH_DELP_C_DHF" & ByVariableLabel != "Three years preceding the survey"))
all_dat_samp %>%
  group_by(dhs_indicator_code) %>%
  summarise(n = n()) %>% filter(n > 1)

# subset(all_dat, dhs_indicator_code == "RH_PCMN_W_MOT")
# RH_PCMN_W_MOT

# Save --------------------------------------------------------------------

write.csv(all_dat_samp, paste("./gen/calculate-direct/audit/statcompiler_", format(Sys.Date(), format="%Y%m%d"),".csv",sep = ""), row.names=FALSE)


