################################################################################
#' @description Calculate direct estimates of household-level outcomes
#' @return Data frame with naive, weighted direct estimates, variance, degrees of freedom
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(srvyr)
#' Inputs
source("./src/util.R")
dhs <- read.csv("./gen/prepare-dhs/output/dat-household.csv")
ind_info <- read_excel("./data/ind-info.xlsx")
################################################################################

# SET DHS INDICATOR CODES
dhs_codes <- ind_info %>%
  filter(status == "include" & dhs_dataset == "hr") %>%
  select(dhs_indicator_code, variable)
# dhs_codes <- data.frame(dhs_indicator_code = c("WS_SRCE_H_IOP", "WS_WTRT_H_APP",
#                                                "WS_TLET_H_IMP", "WS_HNDW_H_BAS"),
#                         variable = c("ph_wtr_improve", "ph_wtr_trt_appr",
#                                      "ph_sani_improve", "ph_hndwsh_basic"))

# NA in survey
# nt_salt_iod CN_IODZ_H_SLT
# nt_salt_15ppm CN_IODZ_H_IOD
# ph_wtr_basic WS_SRCE_H_BAS

# adm2 naive --------------------------------------------------------------------

# CALCULATE NAIVE ESTIMATES AT THE ADM2 LEVEL

naive <- dhs %>% 
  group_by(ADM2_EN) %>% 
  summarise(ph_wtr_improve = mean(ph_wtr_improve, na.rm = TRUE),
            ph_wtr_trt_appr = mean(ph_wtr_trt_appr, na.rm = TRUE),
            ph_sani_improve = mean(ph_sani_improve, na.rm = TRUE),
            ph_hndwsh_basic = mean(ph_hndwsh_basic, na.rm = TRUE)) %>%
  pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "naive")

naive_var <- dhs %>% 
  group_by(ADM2_EN) %>% 
  summarise(ph_wtr_improve = mean(ph_wtr_improve, na.rm = TRUE),
            ph_wtr_trt_appr = mean(ph_wtr_trt_appr, na.rm = TRUE),
            ph_sani_improve = mean(ph_sani_improve, na.rm = TRUE),
            ph_hndwsh_basic = mean(ph_hndwsh_basic, na.rm = TRUE)) %>%
  pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "naive_var")

# tabulate number of observations (weighted and unweighted)
obs_n <- data.frame()
for(i in 1:length(dhs_codes$variable)){
  myvar <- dhs_codes$variable[i]
  df_crosstab <- dhs %>%
    group_by(ADM2_EN) %>%
    filter(!is.na(get(myvar))) %>%
    summarise(obs_un = n(),
              obs_wn = sum(wt)) %>%
    mutate(variable = myvar)
  obs_n <- rbind(obs_n, df_crosstab)
}

# adm2 direct -------------------------------------------------------------

# CALCULATE DESIGN-BASED (DIRECT) ESTIMATES AT ADM2 LEVEL

# dhs_svy <- dhs %>% as_survey_design(ids = c("v001", "v002"), # cluster, household
#                                     strata = "region_name", # division
#                                     weights = "wt",
#                                     nest = TRUE)
dhs_svy <- dhs %>% as_survey_design(ids = "hv001", # psu
                                    strata = "hv023", # strata for sampling
                                    weights = "wt",
                                    nest = TRUE)

dir <- dhs_svy %>% 
  group_by(ADM2_EN) %>% 
  summarise(ph_wtr_improve = survey_mean(ph_wtr_improve, na.rm = TRUE, vartype = "var"),
            ph_wtr_trt_appr = survey_mean(ph_wtr_trt_appr, na.rm = TRUE, vartype = "var"),
            ph_sani_improve = survey_mean(ph_sani_improve, na.rm = TRUE, vartype = "var"),
            ph_hndwsh_basic = survey_mean(ph_hndwsh_basic, na.rm = TRUE, vartype = "var")) 
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
  summarise(n_obs = n_distinct(hv001),
            degf = n_obs - 1) # clusters v001? households v002? individuals?

# MERGE

est_adm2 <- dhs_codes %>%
  left_join(naive, by = c("variable")) %>%
  left_join(naive_var, by = c("ADM2_EN", "variable")) %>%
  left_join(dir, by = c("ADM2_EN", "variable")) %>%
  left_join(dir_var, by = c("ADM2_EN", "variable")) %>%
  left_join(obs_n, by = c("ADM2_EN", "variable")) %>%
  left_join(dhs_degf, by = c("ADM2_EN"))

# adm1 --------------------------------------------------------------------


# CALCULATE DESIGN-BASED (DIRECT) ESTIMATES AT ADM1 LEVEL FOR VALIDATION

dhs_svy <- dhs %>% as_survey_design(ids = "hv001", # psu
                                    strata = "hv023", # strata for sampling
                                    weights = "wt",
                                    nest = TRUE)

dir <- dhs_svy %>% 
  group_by(ADM1_EN) %>% 
  summarise(ph_wtr_improve = survey_mean(ph_wtr_improve, na.rm = TRUE, vartype = "var"),
            ph_wtr_trt_appr = survey_mean(ph_wtr_trt_appr, na.rm = TRUE, vartype = "var"),
            ph_sani_improve = survey_mean(ph_sani_improve, na.rm = TRUE, vartype = "var"),
            ph_hndwsh_basic = survey_mean(ph_hndwsh_basic, na.rm = TRUE, vartype = "var")) 
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

dhs_svy <- dhs %>% as_survey_design(ids = "hv001", # psu
                                    strata = "hv023", # strata for sampling
                                    weights = "wt",
                                    nest = TRUE)

dir <- dhs_svy %>% 
  summarise(ph_wtr_improve = survey_mean(ph_wtr_improve, na.rm = TRUE, vartype = "var"),
            ph_wtr_trt_appr = survey_mean(ph_wtr_trt_appr, na.rm = TRUE, vartype = "var"),
            ph_sani_improve = survey_mean(ph_sani_improve, na.rm = TRUE, vartype = "var"),
            ph_hndwsh_basic = survey_mean(ph_hndwsh_basic, na.rm = TRUE, vartype = "var")) 
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

write.csv(est_adm2, file = "./gen/calculate-direct/temp/direct-household.csv", row.names = FALSE)
write.csv(est_adm1, file = "./gen/calculate-direct/temp/direct-household-adm1.csv", row.names = FALSE)
write.csv(est_adm0, file = "./gen/calculate-direct/temp/direct-household-adm0.csv", row.names = FALSE)
