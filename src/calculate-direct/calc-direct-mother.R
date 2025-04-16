################################################################################
#' @description Calculate direct estimates of mother-level outcomes
#' @return Data frame with naive, weighted direct estimates, variance, degrees of freedom
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(survey)
#' Inputs
source("./src/util.R")
dhs <- read.csv("./gen/prepare-dhs/output/dat-mother.csv")
################################################################################

# CALCULATE NAIVE ESTIMATES AT THE ADM2 LEVEL

naive <- dhs %>% 
  group_by(ADM2_EN) %>% 
  summarise(nt_wm_micro_iron = mean(nt_wm_micro_iron, na.rm = TRUE),
            rh_anc_4vs = mean(rh_anc_4vs, na.rm = TRUE),
            rh_anc_1tri = mean(rh_anc_1tri, na.rm = TRUE)) %>%
  pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "naive")

naive_var <- dhs %>% 
  group_by(ADM2_EN) %>% 
  summarise(nt_wm_micro_iron = var(nt_wm_micro_iron, na.rm = TRUE),
            rh_anc_4vs = var(rh_anc_4vs, na.rm = TRUE),
            rh_anc_1tri = var(rh_anc_1tri, na.rm = TRUE)) %>%
  pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "naive_var")

# CALCULATE DESIGN-BASED (DIRECT) ESTIMATES AT ADM2 LEVEL

# dhs_svy <- dhs %>% as_survey_design(ids = c("v001", "v002"), # cluster, household
#                                     strata = "region_name", # division
#                                     weights = "wt",
#                                     nest = TRUE)
dhs_svy <- dhs %>% as_survey_design(ids = "v001", # psu
                                    strata = "v023", # strata for sampling
                                    weights = "wt",
                                    nest = TRUE)

dir <- dhs_svy %>% 
  group_by(ADM2_EN) %>% 
  summarise(nt_wm_micro_iron = survey_mean(nt_wm_micro_iron, na.rm = TRUE, vartype = "var"),
            rh_anc_4vs = survey_mean(rh_anc_4vs, na.rm = TRUE, vartype = "var"),
            rh_anc_1tri = survey_mean(rh_anc_1tri, na.rm = TRUE, vartype = "var")) 
v_var <- names(dir)[grepl("_var", names(dir))]
v_dir <- names(dir)[!grepl("_var", names(dir))]
dir_var <- dir[,c("ADM2_EN", v_var)]
names(dir_var) <- gsub("_var", "", names(dir_var))
dir <- dir[,v_dir]
dir <- dir %>%
  pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "dir")
dir_var <- dir_var %>%
  pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "dir_var")

# calculate degrees of freedom
dhs_degf <- dhs %>%
  group_by(ADM2_EN) %>%
  summarise(n_obs = n_distinct(v001), # clusters v001? households v002? individuals?
            degf = n_obs - 1)

# MERGE

est_adm2 <- naive %>%
  left_join(naive_var, by = c("ADM2_EN", "variable")) %>%
  left_join(dir, by = c("ADM2_EN", "variable")) %>%
  left_join(dir_var, by = c("ADM2_EN", "variable")) %>%
  left_join(dhs_degf, by = c("ADM2_EN"))

# Save --------------------------------------------------------------------

write.csv(est_adm2, file = "./gen/calculate-direct/temp/direct-mother.csv", row.names = FALSE)
