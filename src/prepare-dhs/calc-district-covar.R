################################################################################
#' @description Calculate district level covariates
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(srvyr)
#' Inputs
source("./src/util.R")
ir <- readRDS("./gen/prepare-dhs/temp/variables-ir.rds")
hr <- readRDS("./gen/prepare-dhs/temp/variables-hr.rds")
################################################################################

ir_svy <- ir %>% as_survey_design(ids = "v001", # psu
                                    strata = "v023", # strata for sampling
                                    weights = "wt",
                                    nest = TRUE)

ir_covar <- ir_svy %>% 
  group_by(ADM2_EN) %>% 
  mutate(mother_edu_binary = case_when(
    mother_edu %in% c(2, 3) ~ 1,
    !is.na(mother_edu) ~ 0,
    TRUE ~ NA_real_ 
    ),
    residence_binary = case_when(
      residence %in% "rural" ~ 1,
      residence != "rural" ~ 0
    )
  ) %>% 
  summarise(mother_edu = survey_mean(mother_edu_binary, na.rm = TRUE, vartype = "var"),
            residence = survey_mean(residence_binary, na.rm = TRUE, vartype = "var")) 


hr_svy <- hr %>% as_survey_design(ids = "hv001", # psu
                                  strata = "hv023", # strata for sampling
                                  weights = "wt",
                                  nest = TRUE)

hr_covar <- hr_svy %>% 
  group_by(ADM2_EN) %>% 
  mutate(wealth_index_binary = case_when(
    wealth_index %in% c(4, 5) ~ 1,
    !is.na(wealth_index) ~ 0,
    TRUE ~ NA_real_ 
  )) %>% 
  summarise(wealth_index = survey_mean(wealth_index_binary, na.rm = TRUE, vartype = "var"), 
            hhd_under5 = survey_mean(hhd_under5, na.rm = TRUE, vartype = "var")) 

dat <- merge(ir_covar, hr_covar, by = "ADM2_EN")

# Save --------------------------------------------------------------------

write.csv(dat, file = "./gen/prepare-dhs/output/covar-district.csv", row.names = FALSE)


