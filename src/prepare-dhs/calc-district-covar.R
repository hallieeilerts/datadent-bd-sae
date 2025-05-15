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
kr <- readRDS("./gen/prepare-dhs/temp/variables-kr.rds")
################################################################################

ir_prep <- ir %>% 
  mutate(mother_edu_binary = case_when(
    mother_edu %in% c("secondary", "higher") ~ 1,
    !is.na(mother_edu) ~ 0,
    TRUE ~ NA_real_ 
    ),
    residence_binary = case_when(
      residence %in% "urban" ~ 0,
      residence == "rural" ~ 1,
      TRUE ~ NA_real_ 
    )
  ) %>% 
  mutate(mother_age = as.numeric(mother_age))

ir_covar <- ir_prep %>% as_survey_design(ids = "v001", # psu
                                  strata = "v023", # strata for sampling
                                  weights = "wt",
                                  nest = TRUE) %>% 
  group_by(ADM2_EN) %>% 
  summarise(mother_edu = survey_mean(mother_edu_binary, na.rm = TRUE, vartype = "var"),
            mother_age = survey_mean(mother_age, na.rm = TRUE, vartype = "var"),
            residence = survey_mean(residence_binary, na.rm = TRUE, vartype = "var")) 


hr_prep <- hr %>% 
  mutate(wealth_index_binary = case_when(
    wealth_index %in% c("richer", "richest") ~ 1,
    !is.na(wealth_index) ~ 0,
    TRUE ~ NA_real_ 
    ),
    hhd_head_sex_binary = case_when(
      hhd_head_sex %in% "male" ~ 0,
      hhd_head_sex %in% "female" ~ 1,
      TRUE ~ NA_real_ 
    ),
    hhd_head_age = case_when(
      hhd_head_age %in% "97+" ~ "97",
      hhd_head_age %in% "98" ~ "98",
      TRUE ~ hhd_head_age
    )
  ) %>% 
  mutate(hhd_head_age = as.numeric(hhd_head_age))
  
  
hr_covar <- hr_prep %>% as_survey_design(ids = "hv001", # psu
                                    strata = "hv023", # strata for sampling
                                    weights = "wt",
                                    nest = TRUE) %>%
  group_by(ADM2_EN) %>% 
  summarise(wealth_index = survey_mean(wealth_index_binary, na.rm = TRUE, vartype = "var"), 
            hhd_under5 = survey_mean(hhd_under5, na.rm = TRUE, vartype = "var"),
            hhd_head_age = survey_mean(hhd_head_age, na.rm = TRUE, vartype = "var"),
            hhd_head_sex = survey_mean(hhd_head_sex_binary, na.rm = TRUE, vartype = "var")) 


kr_prep <- kr %>% 
  mutate(child_age = as.numeric(child_age))

kr_covar <- kr_prep %>% as_survey_design(ids = "v001", # psu
                             strata = "v023", # strata for sampling
                             weights = "wt",
                             nest = TRUE) %>%
  group_by(ADM2_EN) %>% 
  summarise(child_age = survey_mean(child_age, na.rm = TRUE, vartype = "var"),
            child_mdd = survey_mean(nt_mdd, na.rm = TRUE, vartype = "var")) 

dat <- merge(ir_covar, hr_covar, by = "ADM2_EN")
dat <- merge(dat, kr_covar, by = "ADM2_EN")

# Save --------------------------------------------------------------------

write.csv(dat, file = "./gen/prepare-dhs/output/covar-district.csv", row.names = FALSE)


