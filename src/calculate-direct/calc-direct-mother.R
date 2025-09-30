################################################################################
#' @description Calculate direct estimates of mother-level outcomes
#' @return Data frame with naive, weighted direct estimates, variance, degrees of freedom
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(srvyr) # formerly had "survey"
library(readxl)
#' Inputs
source("./src/util.R")
dhs <- read.csv("./gen/prepare-dhs/output/dat-mother.csv")
ind_info <- read_excel("./data/ind-info.xlsx")
################################################################################

# SET DHS INDICATOR CODES
dhs_codes <- ind_info %>%
  filter(status == "include" & dhs_dataset == "ir") %>%
  select(dhs_indicator_code, variable)

# adm2 naive --------------------------------------------------------------------

# CALCULATE NAIVE ESTIMATES AT THE ADM2 LEVEL

naive <- dhs %>% 
  group_by(ADM2_EN) %>% 
  summarise(nt_wm_micro_iron = mean(nt_wm_micro_iron, na.rm = TRUE),
            nt_wm_micro_iron_any = mean(nt_wm_micro_iron_any, na.rm = TRUE),
            nt_wm_ppvita = mean(nt_wm_ppvita , na.rm = TRUE),
            rh_anc_4vs = mean(rh_anc_4vs, na.rm = TRUE),
            rh_anc_1vs = mean(rh_anc_1vs, na.rm = TRUE),
            rh_anc_1tri = mean(rh_anc_1tri, na.rm = TRUE),
            rh_anc_iron = mean(rh_anc_iron, na.rm = TRUE),
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
            nt_wm_ppvita = var(nt_wm_ppvita , na.rm = TRUE),
            rh_anc_4vs = var(rh_anc_4vs, na.rm = TRUE),
            rh_anc_1vs = var(rh_anc_1vs, na.rm = TRUE),
            rh_anc_1tri = var(rh_anc_1tri, na.rm = TRUE),
            rh_anc_iron = var(rh_anc_iron, na.rm = TRUE),
            rh_anc_bldpres = var(rh_anc_bldpres, na.rm = TRUE),
            rh_anc_urine = var(rh_anc_urine, na.rm = TRUE),
            rh_anc_bldsamp = var(rh_anc_bldsamp, na.rm = TRUE),
            rh_anc_wgt = var(rh_anc_wgt, na.rm = TRUE),
            rh_pnc_wm_2days = var(rh_pnc_wm_2days, na.rm = TRUE),
            rh_pnc_nb_2days = var(rh_pnc_nb_2days, na.rm = TRUE),
            rh_pnc_wm_bfcounsel = var(rh_pnc_wm_bfcounsel, na.rm = TRUE)) %>%
  pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "naive_var")

# adm2 direct -------------------------------------------------------------

# CALCULATE DESIGN-BASED (DIRECT) ESTIMATES AT ADM2 LEVEL

dhs_svy <- dhs %>% as_survey_design(ids = "v001", # psu
                                    strata = "v023", # strata for sampling
                                    weights = "wt",
                                    nest = TRUE)

dir <- dhs_svy %>% 
  group_by(ADM2_EN) %>% 
  summarise(nt_wm_micro_iron = survey_mean(nt_wm_micro_iron, na.rm = TRUE, vartype = "var"),
            nt_wm_micro_iron_any = survey_mean(nt_wm_micro_iron_any, na.rm = TRUE, vartype = "var"),
            nt_wm_ppvita = survey_mean(nt_wm_ppvita, na.rm = TRUE, vartype = "var"),
            rh_anc_4vs = survey_mean(rh_anc_4vs, na.rm = TRUE, vartype = "var"),
            rh_anc_1vs = survey_mean(rh_anc_1vs, na.rm = TRUE, vartype = "var"),
            rh_anc_1tri = survey_mean(rh_anc_1tri, na.rm = TRUE, vartype = "var"),
            rh_anc_iron = survey_mean(rh_anc_iron, na.rm = TRUE, vartype = "var"),
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

# by indicator and adm2, tabulate the number of non-missing observations (weighted and unweighted)
# then tabulate averages observations for adm1
obs_n <- data.frame()
for(i in 1:length(dhs_codes$variable)){
  
  myvar <- dhs_codes$variable[i]
  
  df_crosstab <- dhs %>%
    group_by(ADM1_EN, ADM2_EN) %>%
    mutate(obs_ind = ifelse(!is.na(get(myvar)), 1, 0),
           obs_wt = ifelse(!is.na(get(myvar)), wt, 0)) %>%
    summarise(obs_un = sum(obs_ind),
              obs_wn = sum(obs_wt)) %>%
    group_by(ADM1_EN) %>%
    mutate(obs_un_adm1_avg = mean(obs_un, na.rm = TRUE),
           obs_wn_adm1_avg = mean(obs_wn, na.rm = TRUE),
           variable = myvar)
  
  obs_n <- rbind(obs_n, df_crosstab)
}

# by district, calculate degrees of freedom (clusters - 1)
dhs_degf <- dhs %>%
  group_by(ADM2_EN) %>%
  summarise(degf = n_distinct(v001) - 1) # psu/clusters v001? households v002?

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

dhs_svy <- dhs %>% as_survey_design(ids = "v001", # psu
                                    strata = "v023", # strata for sampling
                                    weights = "wt",
                                    nest = TRUE)

dir <- dhs_svy %>% 
  group_by(ADM1_EN) %>% 
  summarise(nt_wm_micro_iron = survey_mean(nt_wm_micro_iron, na.rm = TRUE, vartype = "var"),
            nt_wm_micro_iron_any = survey_mean(nt_wm_micro_iron_any, na.rm = TRUE, vartype = "var"),
            nt_wm_ppvita = survey_mean(nt_wm_ppvita, na.rm = TRUE, vartype = "var"),
            rh_anc_4vs = survey_mean(rh_anc_4vs, na.rm = TRUE, vartype = "var"),
            rh_anc_1vs = survey_mean(rh_anc_1vs, na.rm = TRUE, vartype = "var"),
            rh_anc_1tri = survey_mean(rh_anc_1tri, na.rm = TRUE, vartype = "var"),
            rh_anc_iron = survey_mean(rh_anc_iron, na.rm = TRUE, vartype = "var"),
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

# by indicator and adm1, tabulate the number of non-missing observations (weighted and unweighted)
obs_n <- data.frame()
for(i in 1:length(dhs_codes$variable)){
  
  myvar <- dhs_codes$variable[i]
  
  df_crosstab <- dhs %>%
    group_by(ADM1_EN) %>%
    mutate(obs_ind = ifelse(!is.na(get(myvar)), 1, 0),
           obs_wt = ifelse(!is.na(get(myvar)), wt, 0)) %>%
    summarise(obs_un = sum(obs_ind),
              obs_wn = sum(obs_wt)) %>%
    mutate(variable = myvar)
  
  obs_n <- rbind(obs_n, df_crosstab)
  
}

# by adm1, calculate degrees of freedom (clusters - 1)
dhs_degf <- dhs %>%
  group_by(ADM1_EN) %>%
  summarise(degf = n_distinct(v001) - 1) # psu/clusters v001? households v002?


est_adm1 <- dhs_codes %>%
  left_join(dir, by = c("variable")) %>%
  left_join(dir_var, by = c("ADM1_EN", "variable")) %>%
  left_join(obs_n, by = c("ADM1_EN", "variable")) %>%
  left_join(dhs_degf, by = c("ADM1_EN"))

# adm0 --------------------------------------------------------------------

# CALCULATE DESIGN-BASED (DIRECT) ESTIMATES AT ADM0 LEVEL FOR AUDIT WITH DHS STATCOMPILER

dhs_svy <- dhs %>% as_survey_design(ids = "v001", # psu
                                    strata = "v023", # strata for sampling
                                    weights = "wt",
                                    nest = TRUE)

dir <- dhs_svy %>% 
  summarise(nt_wm_micro_iron = survey_mean(nt_wm_micro_iron, na.rm = TRUE, vartype = "var"),
            nt_wm_micro_iron_any = survey_mean(nt_wm_micro_iron_any, na.rm = TRUE, vartype = "var"),
            nt_wm_ppvita = survey_mean(nt_wm_ppvita, na.rm = TRUE, vartype = "var"),
            rh_anc_4vs = survey_mean(rh_anc_4vs, na.rm = TRUE, vartype = "var"),
            rh_anc_1vs = survey_mean(rh_anc_1vs, na.rm = TRUE, vartype = "var"),
            rh_anc_1tri = survey_mean(rh_anc_1tri, na.rm = TRUE, vartype = "var"),
            rh_anc_iron = survey_mean(rh_anc_iron, na.rm = TRUE, vartype = "var"),
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

# by indicator, tabulate the number of non-missing observations (weighted and unweighted)
obs_n <- data.frame()
for(i in 1:length(dhs_codes$variable)){
  
  myvar <- dhs_codes$variable[i]
  
  df_crosstab <- dhs %>%
    mutate(obs_ind = ifelse(!is.na(get(myvar)), 1, 0),
           obs_wt = ifelse(!is.na(get(myvar)), wt, 0)) %>%
    summarise(obs_un = sum(obs_ind),
              obs_wn = sum(obs_wt)) %>%
    mutate(variable = myvar)
  
  obs_n <- rbind(obs_n, df_crosstab)
  
}

# calculate degrees of freedom (clusters - 1)
dhs_degf <- dhs %>%
  summarise(degf = n_distinct(v001) - 1)

est_adm0 <- dhs_codes %>%
  left_join(dir, by = c("variable")) %>%
  left_join(dir_var, by = c("variable")) %>%
  left_join(obs_n, by = c("variable")) %>%
  crossing(dhs_degf)

# Save --------------------------------------------------------------------

write.csv(est_adm2, file = "./gen/calculate-direct/temp/direct-mother.csv", row.names = FALSE)
write.csv(est_adm1, file = "./gen/calculate-direct/temp/direct-mother-adm1.csv", row.names = FALSE)
write.csv(est_adm0, file = "./gen/calculate-direct/temp/direct-mother-adm0.csv", row.names = FALSE)
