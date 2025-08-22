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
ir2014 <- readRDS("./gen/prepare-dhs/temp/variables-ir-2014.rds")
ir2017 <- readRDS("./gen/prepare-dhs/temp/variables-ir-2017.rds")
hr2014 <- readRDS("./gen/prepare-dhs/temp/variables-hr-2014.rds")
hr2017 <- readRDS("./gen/prepare-dhs/temp/variables-hr-2017.rds")
kr2017 <- readRDS("./gen/prepare-dhs/temp/variables-kr-2017.rds")
# indicator info for which covariates to use
ind <- read_excel("./data/ind-info.xlsx", sheet = "indicators")
################################################################################

# IR covar ----------------------------------------------------------------

ir_prep2022 <- ir %>% 
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

ir_covar2022 <- ir_prep2022 %>% as_survey_design(ids = "v001", # psu
                                  strata = "v023", # strata for sampling
                                  weights = "wt",
                                  nest = TRUE) %>% 
  group_by(ADM2_EN) %>% 
  summarise(mother_edu = survey_mean(mother_edu_binary, na.rm = TRUE, vartype = "var"),
            mother_age = survey_mean(mother_age, na.rm = TRUE, vartype = "var"),
            residence = survey_mean(residence_binary, na.rm = TRUE, vartype = "var")) 


ir_prep2014 <- ir2014 %>% 
  mutate(mother_edu_binary = case_when(
    mother_edu %in% c("secondary", "higher") ~ 1,
    !is.na(mother_edu) ~ 0,
    TRUE ~ NA_real_),
  residence_binary = case_when(
    residence %in% "urban" ~ 0,
    residence == "rural" ~ 1,
    TRUE ~ NA_real_)) %>% 
  mutate(mother_age = as.numeric(mother_age))
ir_covar2014 <- ir_prep2014 %>% as_survey_design(ids = "v001", strata = "v023", weights = "wt", nest = TRUE) %>% 
  group_by(ADM2_EN) %>% 
  summarise(mother_edu = survey_mean(mother_edu_binary, na.rm = TRUE, vartype = "var"),
            mother_age = survey_mean(mother_age, na.rm = TRUE, vartype = "var"),
            residence = survey_mean(residence_binary, na.rm = TRUE, vartype = "var")) 
ir_prep2017 <- ir2017 %>% 
  mutate(mother_edu_binary = case_when(
    mother_edu %in% c("secondary", "higher") ~ 1,
    !is.na(mother_edu) ~ 0,
    TRUE ~ NA_real_),
    residence_binary = case_when(
      residence %in% "urban" ~ 0,
      residence == "rural" ~ 1,
      TRUE ~ NA_real_)) %>% 
  mutate(mother_age = as.numeric(mother_age))
ir_covar2017 <- ir_prep2017 %>% as_survey_design(ids = "v001", strata = "v023", weights = "wt", nest = TRUE) %>% 
  group_by(ADM2_EN) %>% 
  summarise(mother_edu = survey_mean(mother_edu_binary, na.rm = TRUE, vartype = "var"),
            mother_age = survey_mean(mother_age, na.rm = TRUE, vartype = "var"),
            residence = survey_mean(residence_binary, na.rm = TRUE, vartype = "var")) 


ind2014 <- ind %>% filter(dhs_svyyr == 2014) %>% select(variable)
ind2017 <- ind %>% filter(dhs_svyyr == 2017) %>% select(variable)
ind2022 <- ind %>% filter(dhs_svyyr == 2022) %>% select(variable)

df_ir2014 <- ind2014 %>%
  crossing(ir_covar2014)
df_ir2017 <- ind2017 %>%
  crossing(ir_covar2017)
df_ir2022 <- ind2022 %>%
  crossing(ir_covar2022)

ir_covar <- rbind(df_ir2014, df_ir2017, df_ir2022)

# hr ----------------------------------------------------------------------

hr_prep2022 <- hr %>% 
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
  
hr_covar2022 <- hr_prep2022 %>% as_survey_design(ids = "hv001", # psu
                                    strata = "hv023", # strata for sampling
                                    weights = "wt",
                                    nest = TRUE) %>%
  group_by(ADM2_EN) %>% 
  summarise(wealth_index = survey_mean(wealth_index_binary, na.rm = TRUE, vartype = "var"), 
            hhd_under5 = survey_mean(hhd_under5, na.rm = TRUE, vartype = "var"),
            hhd_head_age = survey_mean(hhd_head_age, na.rm = TRUE, vartype = "var"),
            hhd_head_sex = survey_mean(hhd_head_sex_binary, na.rm = TRUE, vartype = "var")) 

hr_prep2014 <- hr2014 %>% 
  mutate(wealth_index_binary = case_when(
    wealth_index %in% c("richer", "richest") ~ 1,
    !is.na(wealth_index) ~ 0,
    TRUE ~ NA_real_),
  hhd_head_sex_binary = case_when(
    hhd_head_sex %in% "male" ~ 0,
    hhd_head_sex %in% "female" ~ 1,
    TRUE ~ NA_real_),
  hhd_head_age = case_when(
    hhd_head_age %in% "97+" ~ "97",
    hhd_head_age %in% "98" ~ "98",
    TRUE ~ hhd_head_age)) %>% 
  mutate(hhd_head_age = as.numeric(hhd_head_age))
hr_covar2014 <- hr_prep2014 %>% as_survey_design(ids = "hv001", strata = "hv023", weights = "wt", nest = TRUE) %>%
  group_by(ADM2_EN) %>% 
  summarise(wealth_index = survey_mean(wealth_index_binary, na.rm = TRUE, vartype = "var"), 
            hhd_under5 = survey_mean(hhd_under5, na.rm = TRUE, vartype = "var"),
            hhd_head_age = survey_mean(hhd_head_age, na.rm = TRUE, vartype = "var"),
            hhd_head_sex = survey_mean(hhd_head_sex_binary, na.rm = TRUE, vartype = "var")) 


hr_prep2017 <- hr2017 %>% 
  mutate(wealth_index_binary = case_when(
    wealth_index %in% c("richer", "richest") ~ 1,
    !is.na(wealth_index) ~ 0,
    TRUE ~ NA_real_),
    hhd_head_sex_binary = case_when(
      hhd_head_sex %in% "male" ~ 0,
      hhd_head_sex %in% "female" ~ 1,
      TRUE ~ NA_real_),
    hhd_head_age = case_when(
      hhd_head_age %in% "97+" ~ "97",
      hhd_head_age %in% "98" ~ "98",
      TRUE ~ hhd_head_age)) %>% 
  mutate(hhd_head_age = as.numeric(hhd_head_age))
hr_covar2017 <- hr_prep2017 %>% as_survey_design(ids = "hv001", strata = "hv023", weights = "wt", nest = TRUE) %>%
  group_by(ADM2_EN) %>% 
  summarise(wealth_index = survey_mean(wealth_index_binary, na.rm = TRUE, vartype = "var"), 
            hhd_under5 = survey_mean(hhd_under5, na.rm = TRUE, vartype = "var"),
            hhd_head_age = survey_mean(hhd_head_age, na.rm = TRUE, vartype = "var"),
            hhd_head_sex = survey_mean(hhd_head_sex_binary, na.rm = TRUE, vartype = "var")) 


ind2014 <- ind %>% filter(dhs_svyyr == 2014) %>% select(variable)
ind2017 <- ind %>% filter(dhs_svyyr == 2017) %>% select(variable)
ind2022 <- ind %>% filter(dhs_svyyr == 2022) %>% select(variable)

df_hr2014 <- ind2014 %>%
  crossing(hr_covar2014)
df_hr2017 <- ind2017 %>%
  crossing(hr_covar2017)
df_hr2022 <- ind2022 %>%
  crossing(hr_covar2022)

hr_covar <- rbind(df_hr2014, df_hr2017, df_hr2022)


# kr ----------------------------------------------------------------------

kr_prep2022 <- kr %>% 
  mutate(child_age = as.numeric(child_age))

kr_covar2022 <- kr_prep2022 %>% as_survey_design(ids = "v001", # psu
                             strata = "v023", # strata for sampling
                             weights = "wt",
                             nest = TRUE) %>%
  group_by(ADM2_EN) %>% 
  summarise(child_age = survey_mean(child_age, na.rm = TRUE, vartype = "var")) 

kr_prep2017 <- kr2017 %>% 
  mutate(child_age = as.numeric(child_age))
kr_covar2017 <- kr_prep2017 %>% as_survey_design(ids = "v001", strata = "v023", weights = "wt", nest = TRUE) %>%
  group_by(ADM2_EN) %>% 
  summarise(child_age = survey_mean(child_age, na.rm = TRUE, vartype = "var")) 

df_kr2017 <- ind2017 %>%
  crossing(kr_covar2017)
df_kr2022 <- ind2022 %>%
  crossing(kr_covar2022)

kr_covar <- rbind(df_kr2017, df_kr2022)

# combine -----------------------------------------------------------------

# all variables (across dhs 2014, 2017, and 2022) have covariates in ir and hr
dat <- merge(ir_covar, hr_covar, by = c("ADM2_EN", "variable"))
# for the one variable coming from dhs 2014 (nt_wm_ppvita), there is no need for kr covariates
# the only kr covariate is child_age, which doesn't apply to "Women with a birth in the past five years who received a vitamin A dose in the first two months after delivery"
# for this reason, adding all.x = TRUE
dat <- merge(dat, kr_covar, by = c("ADM2_EN", "variable"), all.x = TRUE)

# check that there are slightly different covariates for indicators coming from different dhs
subset(dat, variable %in% c("ch_allvac_either","nt_wm_ppvita", "ch_diar_ors") & ADM2_EN == "Bagerhat")

# Save --------------------------------------------------------------------

write.csv(dat, file = "./gen/prepare-dhs/output/covar-district.csv", row.names = FALSE)


