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

# SET DHS INDICATOR CODES

dhs_codes <- data.frame(dhs_indicator_code = c(NA, NA,
                                               "RH_ANCN_W_N4P", NA, 
                                               "RH_ANCS_W_BLP", "RH_ANCS_W_URN", "RH_ANCS_W_BLS", "RH_ANCS_W_WGT", 
                                               "RH_PCMN_W_MOT","RH_PCMN_W_NBR","RH_CPCN_C_BRF"),
                        variable = c("nt_wm_micro_iron", "nt_wm_micro_iron_any", 
                                     "rh_anc_4vs", "rh_anc_1tri", 
                                     "rh_anc_bldpres", "rh_anc_urine", "rh_anc_bldsamp", "rh_anc_wgt", 
                                     "rh_pnc_wm_2days", "rh_pnc_nb_2days", "rh_pnc_wm_bfcounsel"))

# Not in statcompiler
# nt_wm_micro_iron - not in statcompiler because i changed it to a binary indicator for <90 days or 90+ days
# rh_anc_1tri

# Not in survey
# nt_wm_micro_iod AN_MIAM_W_IOD

#55.2 for RH_PCMN_W_MOT rh_pnc_wm_2days

# adm2 --------------------------------------------------------------------

# CALCULATE NAIVE ESTIMATES AT THE ADM2 LEVEL

naive <- dhs %>% 
  group_by(ADM2_EN) %>% 
  summarise(nt_wm_micro_iron = mean(nt_wm_micro_iron, na.rm = TRUE),
            nt_wm_micro_iron_any = mean(nt_wm_micro_iron_any, na.rm = TRUE),
            rh_anc_4vs = mean(rh_anc_4vs, na.rm = TRUE),
            rh_anc_1tri = mean(rh_anc_1tri, na.rm = TRUE),
            rh_anc_bldpres = mean(rh_anc_bldpres, na.rm = TRUE),
            rh_anc_urine = mean(rh_anc_urine, na.rm = TRUE),
            rh_anc_bldsamp = mean(rh_anc_bldsamp, na.rm = TRUE),
            rh_anc_wgt = mean(rh_anc_wgt, na.rm = TRUE),
            rh_pnc_wm_2days = mean(rh_pnc_wm_2days, na.rm = TRUE),
            rh_pnc_nb_2days = mean(rh_pnc_nb_2days, na.rm = TRUE),
            rh_pnc_wm_bfcounsel = mean(rh_pnc_wm_bfcounsel, na.rm = TRUE)) %>%
  pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "naive")

naive_var <- dhs %>% 
  group_by(ADM2_EN) %>% 
  summarise(nt_wm_micro_iron = var(nt_wm_micro_iron, na.rm = TRUE),
            nt_wm_micro_iron_any = var(nt_wm_micro_iron_any, na.rm = TRUE),
            rh_anc_4vs = var(rh_anc_4vs, na.rm = TRUE),
            rh_anc_1tri = var(rh_anc_1tri, na.rm = TRUE),
            rh_anc_bldpres = var(rh_anc_bldpres, na.rm = TRUE),
            rh_anc_urine = var(rh_anc_urine, na.rm = TRUE),
            rh_anc_bldsamp = var(rh_anc_bldsamp, na.rm = TRUE),
            rh_anc_wgt = var(rh_anc_wgt, na.rm = TRUE),
            rh_pnc_wm_2days = var(rh_pnc_wm_2days, na.rm = TRUE),
            rh_pnc_nb_2days = var(rh_pnc_nb_2days, na.rm = TRUE),
            rh_pnc_wm_bfcounsel = var(rh_pnc_wm_bfcounsel, na.rm = TRUE)) %>%
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
            nt_wm_micro_iron_any = survey_mean(nt_wm_micro_iron_any, na.rm = TRUE, vartype = "var"),
            rh_anc_4vs = survey_mean(rh_anc_4vs, na.rm = TRUE, vartype = "var"),
            rh_anc_1tri = survey_mean(rh_anc_1tri, na.rm = TRUE, vartype = "var"),
            rh_anc_bldpres = survey_mean(rh_anc_bldpres, na.rm = TRUE, vartype = "var"),
            rh_anc_urine = survey_mean(rh_anc_urine, na.rm = TRUE, vartype = "var"),
            rh_anc_bldsamp = survey_mean(rh_anc_bldsamp, na.rm = TRUE, vartype = "var"),
            rh_anc_wgt = survey_mean(rh_anc_wgt, na.rm = TRUE, vartype = "var"),
            rh_pnc_wm_2days = survey_mean(rh_pnc_wm_2days, na.rm = TRUE, vartype = "var"),
            rh_pnc_nb_2days = survey_mean(rh_pnc_nb_2days, na.rm = TRUE, vartype = "var"),
            rh_pnc_wm_bfcounsel = survey_mean(rh_pnc_wm_bfcounsel, na.rm = TRUE, vartype = "var")) 
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
  group_by(ADM1_EN, ADM2_EN) %>%
  summarise(n_obs = n_distinct(v001), # clusters v001? households v002? individuals?
            degf = n_obs - 1,
            sum_wgt = sum(wt))

# MERGE

est_adm2 <- dhs_codes %>%
  left_join(naive, by = c("variable")) %>%
  left_join(naive_var, by = c("ADM2_EN", "variable")) %>%
  left_join(dir, by = c("ADM2_EN", "variable")) %>%
  left_join(dir_var, by = c("ADM2_EN", "variable")) %>%
  left_join(dhs_degf, by = c("ADM2_EN"))

# adm1 --------------------------------------------------------------------


# CALCULATE DESIGN-BASED (DIRECT) ESTIMATES AT ADM1 LEVEL FOR VALIDATION

dhs_svy <- dhs %>% as_survey_design(ids = "v001", # psu
                                    strata = "v023", # strata for sampling
                                    weights = "wt",
                                    nest = TRUE)

dir <- dhs_svy %>% 
  group_by(ADM1_EN) %>% 
  summarise(nt_wm_micro_iron = survey_mean(nt_wm_micro_iron, na.rm = TRUE, vartype = "var"),
            nt_wm_micro_iron_any = survey_mean(nt_wm_micro_iron_any, na.rm = TRUE, vartype = "var"),
            rh_anc_4vs = survey_mean(rh_anc_4vs, na.rm = TRUE, vartype = "var"),
            rh_anc_1tri = survey_mean(rh_anc_1tri, na.rm = TRUE, vartype = "var"),
            rh_anc_bldpres = survey_mean(rh_anc_bldpres, na.rm = TRUE, vartype = "var"),
            rh_anc_urine = survey_mean(rh_anc_urine, na.rm = TRUE, vartype = "var"),
            rh_anc_bldsamp = survey_mean(rh_anc_bldsamp, na.rm = TRUE, vartype = "var"),
            rh_anc_wgt = survey_mean(rh_anc_wgt, na.rm = TRUE, vartype = "var"),
            rh_pnc_wm_2days = survey_mean(rh_pnc_wm_2days, na.rm = TRUE, vartype = "var"),
            rh_pnc_nb_2days = survey_mean(rh_pnc_nb_2days, na.rm = TRUE, vartype = "var"),
            rh_pnc_wm_bfcounsel = survey_mean(rh_pnc_wm_bfcounsel, na.rm = TRUE, vartype = "var")) 
v_var <- names(dir)[grepl("_var", names(dir))]
v_dir <- names(dir)[!grepl("_var", names(dir))]
dir_var <- dir[,c("ADM1_EN", v_var)]
names(dir_var) <- gsub("_var", "", names(dir_var))
dir <- dir[,v_dir]
dir <- dir %>%
  pivot_longer(cols = -ADM1_EN, names_to = "variable", values_to = "dir")
dir_var <- dir_var %>%
  pivot_longer(cols = -ADM1_EN, names_to = "variable", values_to = "dir_var")

est_adm1 <- dhs_codes %>%
  left_join(dir, by = c("variable")) %>%
  left_join(dir_var, by = c("ADM1_EN", "variable"))

# adm0 --------------------------------------------------------------------


# CALCULATE DESIGN-BASED (DIRECT) ESTIMATES AT ADM0 LEVEL FOR AUDIT WITH DHS STATCOMPILER

dhs_svy <- dhs %>% as_survey_design(ids = "v001", # psu
                                    strata = "v023", # strata for sampling
                                    weights = "wt",
                                    nest = TRUE)

dir <- dhs_svy %>% 
  summarise(nt_wm_micro_iron = survey_mean(nt_wm_micro_iron, na.rm = TRUE, vartype = "var"),
            nt_wm_micro_iron_any = survey_mean(nt_wm_micro_iron_any, na.rm = TRUE, vartype = "var"),
            rh_anc_4vs = survey_mean(rh_anc_4vs, na.rm = TRUE, vartype = "var"),
            rh_anc_1tri = survey_mean(rh_anc_1tri, na.rm = TRUE, vartype = "var"),
            rh_anc_bldpres = survey_mean(rh_anc_bldpres, na.rm = TRUE, vartype = "var"),
            rh_anc_urine = survey_mean(rh_anc_urine, na.rm = TRUE, vartype = "var"),
            rh_anc_bldsamp = survey_mean(rh_anc_bldsamp, na.rm = TRUE, vartype = "var"),
            rh_anc_wgt = survey_mean(rh_anc_wgt, na.rm = TRUE, vartype = "var"),
            rh_pnc_wm_2days = survey_mean(rh_pnc_wm_2days, na.rm = TRUE, vartype = "var"),
            rh_pnc_nb_2days = survey_mean(rh_pnc_nb_2days, na.rm = TRUE, vartype = "var"),
            rh_pnc_wm_bfcounsel = survey_mean(rh_pnc_wm_bfcounsel, na.rm = TRUE, vartype = "var")) 
v_var <- names(dir)[grepl("_var", names(dir))]
v_dir <- names(dir)[!grepl("_var", names(dir))]
dir_var <- dir[, v_var]
names(dir_var) <- gsub("_var", "", names(dir_var))
dir <- dir[, v_dir]
dir <- dir %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "dir")
dir_var <- dir_var %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "dir_var")

est_adm0 <- dhs_codes %>%
  left_join(dir, by = c("variable")) %>%
  left_join(dir_var, by = c("variable"))

# Save --------------------------------------------------------------------

write.csv(est_adm2, file = "./gen/calculate-direct/temp/direct-mother.csv", row.names = FALSE)
write.csv(est_adm1, file = "./gen/calculate-direct/temp/direct-mother-adm1.csv", row.names = FALSE)
write.csv(est_adm0, file = "./gen/calculate-direct/temp/direct-mother-adm0.csv", row.names = FALSE)
