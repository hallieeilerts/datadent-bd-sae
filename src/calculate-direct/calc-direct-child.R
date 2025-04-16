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
  summarise(nt_ch_micro_vas = survey_mean(nt_ch_micro_vas, na.rm = TRUE, vartype = "var"),
            nt_ch_micro_dwm = survey_mean(nt_ch_micro_dwm, na.rm = TRUE, vartype = "var"),
            nt_ebf = survey_mean(nt_ebf, na.rm = TRUE, vartype = "var")) 
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
  


# x_modified <- subset(dhs, !is.na(nt_ch_micro_vas))
# survey_design <- svydesign(
#   id = ~v001,       # Primary Sampling Unit (PSU)
#   strata = ~v023,   # Stratification variable
#   weights = ~wt,    # Survey weights
#   data = x_modified,
#   nest = TRUE       # Accounts for stratification
# )
# taylor_results <- svyby(
#   formula = ~nt_ch_micro_vas,  # variable of interest
#   by = ~ADM2_EN,  # grouping
#   design = survey_design,
#   FUN = svymean,               # Compute mean (proportion)
#   vartype = "se"               # Get standard error (SE)
# ) %>%
#   mutate(variance = as.numeric(se)^2) %>%
#   rename(value = nt_ch_micro_vas)
# taylor_results 


# Save --------------------------------------------------------------------

write.csv(est_adm2, file = "./gen/calculate-direct/temp/direct-child.csv", row.names = FALSE)

