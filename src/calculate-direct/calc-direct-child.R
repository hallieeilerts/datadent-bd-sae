################################################################################
#' @description Calculate direct estimates of child-level outcomes
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
dhs <- read.csv("./gen/prepare-dhs/output/dat-child.csv")
################################################################################

# SET DHS INDICATOR CODES

dhs_codes <- data.frame(dhs_indicator_code = c("CN_MIAC_C_VAS", "CN_MIAC_C_DWM", "CN_BFSS_C_EBF"),
                        variable = c("nt_ch_micro_vas", "nt_ch_micro_dwm", "nt_ebf"))

# Not in survey
# nt_counsel_iycf CN_IYCC_W_COU
# ch_rotav3_either CH_VACC_C_RT2
# ch_meas_either CH_VACC_C_MSL


# adm2 naive --------------------------------------------------------------------


# CALCULATE NAIVE ESTIMATES AT THE ADM2 LEVEL

naive <- dhs %>% 
  group_by(ADM2_EN) %>% 
  summarise(nt_ch_micro_vas = mean(nt_ch_micro_vas, na.rm = TRUE),
            nt_ch_micro_dwm = mean(nt_ch_micro_dwm, na.rm = TRUE),
            nt_ebf = mean(nt_ebf, na.rm = TRUE)) %>%
  pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "naive")

naive_var <- dhs %>% 
  group_by(ADM2_EN) %>% 
  summarise(nt_ch_micro_vas = var(nt_ch_micro_vas, na.rm = TRUE),
            nt_ch_micro_dwm = var(nt_ch_micro_dwm, na.rm = TRUE),
            nt_ebf = var(nt_ebf, na.rm = TRUE)) %>%
  pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "naive_var")


# adm2 direct (without synthetic household) -------------------------------

# CALCULATE DESIGN-BASED (DIRECT) ESTIMATES AT ADM2 LEVEL
# After fixing bug in nt_ebf, need synthetic household because some districts have all same value and can't calculate variance
# so rewrote this code below to calculate synthetic household only for indicators that need it.
# worth doing because about to add more indicators to analysis.

# dhs_svy <- dhs %>% as_survey_design(ids = "v001", # psu
#                                     strata = "v023", # strata for sampling
#                                     weights = "wt",
#                                     nest = TRUE)
# 
# dir <- dhs_svy %>%
#   group_by(ADM2_EN) %>%
#   summarise(nt_ch_micro_vas = survey_mean(nt_ch_micro_vas, na.rm = TRUE, vartype = "var"),
#             nt_ch_micro_dwm = survey_mean(nt_ch_micro_dwm, na.rm = TRUE, vartype = "var"),
#             nt_ebf = survey_mean(nt_ebf, na.rm = TRUE, vartype = "var"))
# v_var <- names(dir)[grepl("_var", names(dir))]
# v_dir <- names(dir)[!grepl("_var", names(dir))]
# dir_var <- dir[,c("ADM2_EN", v_var)]
# names(dir_var) <- gsub("_var", "", names(dir_var))
# dir <- dir[,v_dir]
# dir <- dir %>%
#   pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "dir")
# dir_var <- dir_var %>%
#   pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "dir_var")
# 
# # calculate degrees of freedom
# dhs_degf <- dhs %>%
#   group_by(ADM1_EN, ADM2_EN) %>%
#   summarise(n_obs = n_distinct(v001), # psu/clusters v001? households v002?
#             degf = n_obs - 1,
#             sum_wgt = sum(wt))
# 
# # MERGE
# 
# est_adm2 <- dhs_codes %>%
#   left_join(naive, by = c("variable")) %>%
#   left_join(naive_var, by = c("ADM2_EN", "variable")) %>%
#   left_join(dir, by = c("ADM2_EN", "variable")) %>%
#   left_join(dir_var, by = c("ADM2_EN", "variable")) %>%
#   left_join(dhs_degf, by = c("ADM2_EN"))

# adm2 direct --------------------------------------------------------------------

# For indicators that require synthetic household
l_res <- list()
v_allonecat <- unique(subset(naive, naive %in% c(0,1))$variable)
v_rest <- unique(subset(naive, !variable %in% v_allonecat)$variable)
if (length(v_allonecat) > 0) {
  
  set.seed(1234)
  
  # For each indicator which requires a synthetic household 
  for (i in seq_along(v_allonecat)) {
    
    # Grab the variable name as a symbol
    var_sym <- sym(v_allonecat[i])
    
    # Identify districts needing synthetic household for this variable
    v_dist_01 <- naive %>%
      filter(variable %in% v_allonecat[i], naive %in% c(0, 1)) %>%
      pull(ADM2_EN)
    # Hallie addition: identify other districts that also need it because their clusters all the have the same mean
    # and within each cluster the weights are all the same.
    # Not happy with the variance for these though. Seems too small. c("Meherpur", "Narail")
    v_dist_01_aug <- dhs %>% 
          filter(!is.na(!!var_sym)) %>%
          group_by(ADM2_EN, v001) %>%
          mutate(var_avg = mean(!!var_sym),
                 n_uniq_wt = n_distinct(wt)) %>%
          group_by(ADM2_EN) %>%
          mutate(n_uniq_var = n_distinct(var_avg)) %>%
          filter(n_uniq_wt == 1 & n_uniq_var == 1) %>% 
          select(ADM2_EN) %>% unique() %>% pull(ADM2_EN)
    v_dist_01 <- sort(unique(c(v_dist_01, v_dist_01_aug)))
    
    # Subset districts
    dist_01 <- dhs %>% filter(ADM2_EN %in% v_dist_01)
    dist_rest <- dhs %>% filter(!ADM2_EN %in% v_dist_01)
    
    #View(subset(dhs, ADM2_EN %in% c("Khagrachhari", "Meherpur", "Narail")))
    # Khagrachhar is in dist_01
    # the other two are in dist_rest
    # View(subset(dhs, ADM2_EN == "Khagrachhari"))
    # table(subset(dhs, ADM2_EN == "Khagrachhari")$nt_ebf)
    # table(subset(dhs, ADM2_EN == "Meherpur")$nt_ebf)
    # table(subset(dhs, ADM2_EN == "Narail")$nt_ebf)
    # View(subset(dhs, ADM2_EN == "Meherpur" & !is.na(nt_ebf)))
    
    # For each district
    for (j in seq_along(v_dist_01)) {
      
      df_tmp <- dist_01 %>%
        filter(ADM2_EN == v_dist_01[j], !is.na(!!var_sym))
      if (nrow(df_tmp) == 0){
       stop("sparse data") 
      }
      df_synthetic <- df_tmp %>%
        slice_sample(n = 1) %>%
        mutate(!!var_sym := abs(!!var_sym - 1),
               v001 = j)
        
        dist_01 <- bind_rows(dist_01, df_synthetic)
    }
    
    # Combine
    dhs_syn <- bind_rows(dist_rest, dist_01)
    
    dhs_svy <- dhs_syn %>% as_survey_design(ids = "v001", # psu
                                        strata = "v023", # strata for sampling
                                        weights = "wt",
                                        nest = TRUE)
    
    dir <- dhs_svy %>%
      group_by(ADM2_EN) %>%
      summarise(!!var_sym := survey_mean({{var_sym}}, na.rm = TRUE, vartype = "var"))
    v_var <- names(dir)[grepl("_var", names(dir))]
    v_dir <- names(dir)[!grepl("_var", names(dir))]
    dir_var <- dir[,c("ADM2_EN", v_var)]
    names(dir_var) <- gsub("_var", "", names(dir_var))
    dir <- dir[,v_dir]
    dir <- dir %>%
      pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "dir")
    dir_var <- dir_var %>%
      pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "dir_var")
    
    res <- dir %>%
      left_join(dir_var, by = c("ADM2_EN", "variable"))
    l_res[[i]] <- res
  }
}
df_res_syn <- do.call(rbind, l_res)

# For each indicator that does not require a synthetic household 
l_res <- list()
for (i in seq_along(v_rest)) {

  # Grab the variable name as a symbol
  var_sym <- sym(v_rest[i])
  
  dhs_svy <- dhs_syn %>% as_survey_design(ids = "v001", # psu
                                          strata = "v023", # strata for sampling
                                          weights = "wt",
                                          nest = TRUE)
  dir <- dhs_svy %>%
    group_by(ADM2_EN) %>%
    summarise(!!var_sym := survey_mean({{var_sym}}, na.rm = TRUE, vartype = "var"))
  v_var <- names(dir)[grepl("_var", names(dir))]
  v_dir <- names(dir)[!grepl("_var", names(dir))]
  dir_var <- dir[,c("ADM2_EN", v_var)]
  names(dir_var) <- gsub("_var", "", names(dir_var))
  dir <- dir[,v_dir]
  dir <- dir %>%
    pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "dir")
  dir_var <- dir_var %>%
    pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "dir_var")
  
  res <- dir %>%
    left_join(dir_var, by = c("ADM2_EN", "variable"))
  l_res[[i]] <- res
  
}
df_res_rest <- do.call(rbind, l_res)

# combine
df_res <- rbind(df_res_rest, df_res_syn)
df_res <- df_res[order(df_res$variable, df_res$ADM2_EN),]

# calculate degrees of freedom
dhs_degf <- dhs %>%
  group_by(ADM1_EN, ADM2_EN) %>%
  summarise(n_obs = n_distinct(v001), # psu/clusters v001? households v002?
            degf = n_obs - 1,
            sum_wgt = sum(wt))

est_adm2 <- dhs_codes %>%
  left_join(naive, by = c("variable")) %>%
  left_join(naive_var, by = c("ADM2_EN", "variable")) %>%
  left_join(df_res, by = c("ADM2_EN", "variable")) %>%
  left_join(dhs_degf, by = c("ADM2_EN"))

#View(subset(est_adm2, variable == "nt_ebf" & ADM2_EN %in% c("Meherpur", "Narail")))

# adm1 --------------------------------------------------------------------


# CALCULATE DESIGN-BASED (DIRECT) ESTIMATES AT ADM1 LEVEL FOR VALIDATION

dhs_svy <- dhs %>% as_survey_design(ids = "v001", # psu
                                    strata = "v023", # strata for sampling
                                    weights = "wt",
                                    nest = TRUE)

dir <- dhs_svy %>% 
  group_by(ADM1_EN) %>% 
  summarise(nt_ch_micro_vas = survey_mean(nt_ch_micro_vas, na.rm = TRUE, vartype = "var"),
            nt_ch_micro_dwm = survey_mean(nt_ch_micro_dwm, na.rm = TRUE, vartype = "var"),
            nt_ebf = survey_mean(nt_ebf, na.rm = TRUE, vartype = "var")) 
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
  summarise(nt_ch_micro_vas = survey_mean(nt_ch_micro_vas, na.rm = TRUE, vartype = "var"),
            nt_ch_micro_dwm = survey_mean(nt_ch_micro_dwm, na.rm = TRUE, vartype = "var"),
            nt_ebf = survey_mean(nt_ebf, na.rm = TRUE, vartype = "var")) 
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

write.csv(est_adm2, file = "./gen/calculate-direct/temp/direct-child.csv", row.names = FALSE)
write.csv(est_adm1, file = "./gen/calculate-direct/temp/direct-child-adm1.csv", row.names = FALSE)
write.csv(est_adm0, file = "./gen/calculate-direct/temp/direct-child-adm0.csv", row.names = FALSE)
