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
library(survey)
#' Inputs
source("./src/util.R")
dhs <- read.csv("./gen/prepare-dhs/output/dat-child.csv")
ind_info <- read_excel("./data/ind-info.xlsx")
################################################################################

# SET DHS INDICATOR CODES
dhs_codes <- ind_info %>%
  filter(status == "include" & dhs_dataset == "kr") %>%
  select(dhs_indicator_code, variable)

# adm2 naive --------------------------------------------------------------------

# CALCULATE NAIVE ESTIMATES AT THE ADM2 LEVEL

naive <- dhs %>% 
  group_by(ADM2_EN) %>% 
  summarise(nt_ch_micro_vas = mean(nt_ch_micro_vas, na.rm = TRUE),
            nt_ch_micro_dwm = mean(nt_ch_micro_dwm, na.rm = TRUE),
            nt_ebf = mean(nt_ebf, na.rm = TRUE),
            nt_ch_micro_mp = mean(nt_ch_micro_mp, na.rm = TRUE),
            ch_diar_zinc = mean(ch_diar_zinc, na.rm = TRUE),
            ch_diar_ors = mean(ch_diar_ors, na.rm = TRUE),
            ch_allvac_either = mean(ch_allvac_either, na.rm = TRUE),
            nt_ch_gwmt_any = mean(nt_ch_gwmt_any, na.rm = TRUE),
            nt_ch_micro_iron = mean(nt_ch_micro_iron, na.rm = TRUE)) %>%
  pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "naive")

naive_var <- dhs %>% 
  group_by(ADM2_EN) %>% 
  summarise(nt_ch_micro_vas = var(nt_ch_micro_vas, na.rm = TRUE),
            nt_ch_micro_dwm = var(nt_ch_micro_dwm, na.rm = TRUE),
            nt_ebf = var(nt_ebf, na.rm = TRUE),
            nt_ch_micro_mp = var(nt_ch_micro_mp, na.rm = TRUE),
            ch_diar_zinc = var(ch_diar_zinc, na.rm = TRUE),
            ch_diar_ors = var(ch_diar_ors, na.rm = TRUE),
            ch_allvac_either = var(ch_allvac_either, na.rm = TRUE),
            nt_ch_gwmt_any = var(nt_ch_gwmt_any, na.rm = TRUE),
            nt_ch_micro_iron = var(nt_ch_micro_iron, na.rm = TRUE)) %>%
  pivot_longer(cols = -ADM2_EN, names_to = "variable", values_to = "naive_var")


# adm2 direct for plot ----------------------------------------------------

# CALCULATE DESIGN-BASED (DIRECT) ESTIMATES AT ADM2 LEVEL

# these are used for plotting the direct values of child estimates
# sometimes the data is sparse for these indicators and need to add a synthetic household
# (have not yet encountered this situation with mother- and household-level indicators)
# therefore i create one set of direct estimates that can be used in the model in which a synthetic household is sometimes added.
# i create another set for plotting the unaltered direct estimates.

dhs_svy <- dhs %>% as_survey_design(ids = "v001", # psu
                                    strata = "v023", # strata for sampling
                                    weights = "wt",
                                    nest = TRUE)

dir <- dhs_svy %>%
  group_by(ADM2_EN) %>%
  summarise(nt_ch_micro_vas = survey_mean(nt_ch_micro_vas, na.rm = TRUE, vartype = "var"),
            nt_ch_micro_dwm = survey_mean(nt_ch_micro_dwm, na.rm = TRUE, vartype = "var"),
            nt_ebf = survey_mean(nt_ebf, na.rm = TRUE, vartype = "var"),
            nt_ch_micro_mp = survey_mean(nt_ch_micro_mp, na.rm = TRUE, vartype = "var"),
            ch_diar_zinc = survey_mean(ch_diar_zinc, na.rm = TRUE, vartype = "var"),
            ch_diar_ors = survey_mean(ch_diar_ors, na.rm = TRUE, vartype = "var"),
            ch_allvac_either = survey_mean(ch_allvac_either, na.rm = TRUE, vartype = "var"),
            nt_ch_gwmt_any = survey_mean(nt_ch_gwmt_any, na.rm = TRUE, vartype = "var"),
            nt_ch_micro_iron = survey_mean(nt_ch_micro_iron, na.rm = TRUE, vartype = "var"))
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

est_adm2_forplot <- dhs_codes %>%
  left_join(naive, by = c("variable")) %>%
  left_join(naive_var, by = c("ADM2_EN", "variable")) %>%
  left_join(dir, by = c("ADM2_EN", "variable")) %>%
  left_join(dir_var, by = c("ADM2_EN", "variable")) %>%
  left_join(obs_n, by = c("ADM2_EN", "variable")) %>%
  left_join(dhs_degf, by = c("ADM2_EN"))

# If naive and naive_var are NA, this means there were no non-missing observations in the adm2 unit
# recode dir and dir_var to NA so that it is not plotted
est_adm2_forplot$dir[is.na(est_adm2_forplot$naive)] <- NA
est_adm2_forplot$dir_var[is.na(est_adm2_forplot$naive_var)] <- NA

# adm2 phantom household --------------------------------------------------

# vector of variables
v_var <- unique(naive$variable)

ll_res <- list()
for(i in 1:length(v_var)){
  
  # select var
  myvar <- v_var[i]
  print(myvar)
  # Grab the variable name as a symbol
  var_sym <- sym(v_var[i])
  
  ll_res[[i]] <- sae_df(df = dhs, est_orig = est_adm2_forplot, 
                  geo_level = "ADM2_EN", strata = "v023")
}
l_res <- lapply(ll_res, function(x) x[[1]])
est_adm2_phantom <- do.call(rbind, l_res)


# adm2 synthetic household ------------------------------------------------

# this adds a synthetic household when all values are 0 or 1
# perturbs values if prevalence is exactly 0.5 (and thus variance is 0)

# For indicators that require synthetic household
l_res <- list()
v_allonecat <- unique(subset(naive, naive %in% c(0,1))$variable)
v_rest <- unique(subset(naive, !variable %in% v_allonecat)$variable)

if (length(v_allonecat) > 0) {
  
  set.seed(123)
  
  # For each indicator which requires a synthetic household 
  for (i in seq_along(v_allonecat)) {
    
    # Grab the variable name as a symbol
    var_sym <- sym(v_allonecat[i])
    
    # Identify districts with all households in one category (prevalence of 0 or 1). This includes those where all values are NA.
    # for adding a synthetic household
    v_dist_01 <- naive %>%
      filter(variable %in% v_allonecat[i], naive %in% c(0, 1)) %>%
      pull(ADM2_EN)
    
    # Identify districts where all clusters have a mean of 0.5
    # Not happy with the variance for these though. Seems too small. c("Meherpur", "Narail")
    v_dist_0bwclustvar <- dhs %>% 
          filter(!is.na(!!var_sym)) %>%                # filter non-missing values (treating var_sym as a variable)
          group_by(ADM2_EN, v001) %>%                  # group by cluster
          mutate(var_avg = mean(!!var_sym)) %>%        # calculate average by cluster
          group_by(ADM2_EN) %>%                        # group by district
          mutate(n_uniq_val = n_distinct(var_avg)) %>% # count number of unique averages in district
          filter(n_uniq_val == 1) %>%                  # filter if all clusters in district have same prevalence (used to also include var_avg == 0.5 &)
          select(ADM2_EN) %>% unique() %>% pull(ADM2_EN)
    
    # Subset districts
    dist_01 <- dhs %>% filter(ADM2_EN %in% v_dist_01)
    dist_0bwclustvar <- dhs %>% filter(ADM2_EN %in% v_dist_0bwclustvar)
    dist_rest <- dhs %>% filter(!(ADM2_EN %in% c(v_dist_01, v_dist_0bwclustvar)))
    
    #View(subset(dhs, ADM2_EN %in% c("Khagrachhari", "Meherpur", "Narail")))
    # Khagrachhar is in dist_01
    # the other two are in dist_rest
    # View(subset(dhs, ADM2_EN == "Khagrachhari"))
    # table(subset(dhs, ADM2_EN == "Khagrachhari")$nt_ebf)
    # table(subset(dhs, ADM2_EN == "Meherpur")$nt_ebf)
    # table(subset(dhs, ADM2_EN == "Narail")$nt_ebf)
    # View(subset(dhs, ADM2_EN == "Meherpur" & !is.na(nt_ebf)))
    # View(subset(dhs, ADM2_EN == "Meherpur" & is.na(nt_ebf)))
    
    # When prevalence is 0 or 1 in a district
    # If less than 10 total observations, recode reported values as NA (to avoid corrupting data)
    # If more than 10 total observations, add a synthetic observations with opposite value
    # (The synthetic household is added to the first cluster in the district)
    for (j in seq_along(v_dist_01)) {
      
      df_tmp <- dist_01 %>%
        filter(ADM2_EN == v_dist_01[j], !is.na(!!var_sym))
      
      if(nrow(df_tmp) < 10){
        dist_01 <- dist_01 %>%
          mutate(!!var_sym := case_when(  # using := to support tidy evaluation of left side of equation
            ADM2_EN == v_dist_01[j] & !is.na(!!var_sym) ~ NA,
            TRUE ~ !!var_sym
          )) 
      }
      
      if(nrow(df_tmp) >= 10){
        df_synthetic <- df_tmp %>%
          slice_sample(n = 1) %>%
          mutate(!!var_sym := abs(!!var_sym - 1))
        dist_01 <- bind_rows(dist_01, df_synthetic)
      }
    }
    
    # When all clusters in a given district have the same prevalence (no between cluster variance)
    # (for all clusters, there are an equal number of households with 0's and 1's)
    # (example, indicator exbf, Meherpur and Narail districts)
    # Perturb each individual value slightly away from 0 or 1
    for (j in seq_along(v_dist_0bwclustvar)) {
      
      noise <- runif(nrow(dist_0bwclustvar), -0.01, 0.01)
      dist_0bwclustvar <- dist_0bwclustvar %>%
        mutate(!!var_sym := if_else(ADM2_EN == v_dist_0bwclustvar[j] & !is.na(!!var_sym),
                                  as.numeric(!!var_sym) + noise,
                                  as.numeric(!!var_sym))
        )
      
    }
    
    
    # Combine
    dhs_syn <- bind_rows(dist_rest, dist_01, dist_0bwclustvar)
    
    # Set survey design
    dhs_svy <- dhs_syn %>% as_survey_design(ids = "v001", # psu
                                        strata = "v023", # strata for sampling
                                        weights = "wt",
                                        nest = TRUE)
    # Calculate mean and variance
    dir <- dhs_svy %>%
      group_by(ADM2_EN) %>%
      summarise(!!var_sym := survey_mean({{var_sym}}, na.rm = TRUE, vartype = "var"))
    
    # If all values were NA in a district, recode as NA rather than 0
    dir <- dir %>%
      mutate(
        !!var_sym := if_else(
          (!!var_sym == 0) & (!!sym(paste0(quo_name(var_sym), "_var")) == 0),
          NA_real_,
          !!var_sym
        ),
        !!sym(paste0(quo_name(var_sym), "_var")) := if_else(
          (!!var_sym == 0) & (!!sym(paste0(quo_name(var_sym), "_var")) == 0),
          NA_real_,
          !!sym(paste0(quo_name(var_sym), "_var"))
        )
      )
    
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

est_adm2 <- dhs_codes %>%
  left_join(naive, by = c("variable")) %>%
  left_join(naive_var, by = c("ADM2_EN", "variable")) %>%
  left_join(df_res, by = c("ADM2_EN", "variable")) %>%
  left_join(obs_n, by = c("ADM2_EN", "variable")) %>%
  left_join(dhs_degf, by = c("ADM2_EN"))

# Combine adm2 direct and phantom -----------------------------------------

# merge adm2 direct and adm2 with phantom households
est_adm2_combined <- est_adm2 %>%
  left_join(est_adm2_phantom %>% 
              select(variable, ADM2_EN, design_based_ph_mean, design_based_ph_var, degf_ph, n_obs_ph),
            by = c("variable", "ADM2_EN"))

# replace direct with phantom household when...
# obs_un != 0 (direct should still be NA in such cases)
# obs_un != n_obs_ph (a phantom household was added)
# Note this results in some relatively large changes with obs_un was 1
## subset(est_adm2_combined, variable == "ch_diar_zinc" & obs_un == 1)
est_adm2_combined <- est_adm2_combined %>%
  mutate(dir = case_when(
    obs_un != 0 & obs_un != n_obs_ph ~ design_based_ph_mean,
    TRUE ~ dir),
  dir_var = case_when(
    obs_un != 0 & obs_un != n_obs_ph ~ design_based_ph_var,
    TRUE ~ dir_var),
  dir_replacedw_ph = ifelse(obs_un != 0 & obs_un != n_obs_ph, 1, 0)
  ) 

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
            nt_ebf = survey_mean(nt_ebf, na.rm = TRUE, vartype = "var"),
            nt_ch_micro_mp = survey_mean(nt_ch_micro_mp, na.rm = TRUE, vartype = "var"),
            ch_diar_zinc = survey_mean(ch_diar_zinc, na.rm = TRUE, vartype = "var"),
            ch_diar_ors = survey_mean(ch_diar_ors, na.rm = TRUE, vartype = "var"),
            ch_allvac_either = survey_mean(ch_allvac_either, na.rm = TRUE, vartype = "var"),
            nt_ch_gwmt_any = survey_mean(nt_ch_gwmt_any, na.rm = TRUE, vartype = "var"),
            nt_ch_micro_iron = survey_mean(nt_ch_micro_iron, na.rm = TRUE, vartype = "var")) 
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
  summarise(nt_ch_micro_vas = survey_mean(nt_ch_micro_vas, na.rm = TRUE, vartype = "var"),
            nt_ch_micro_dwm = survey_mean(nt_ch_micro_dwm, na.rm = TRUE, vartype = "var"),
            nt_ebf = survey_mean(nt_ebf, na.rm = TRUE, vartype = "var"),
            nt_ch_micro_mp = survey_mean(nt_ch_micro_mp, na.rm = TRUE, vartype = "var"),
            ch_diar_zinc = survey_mean(ch_diar_zinc, na.rm = TRUE, vartype = "var"),
            ch_diar_ors = survey_mean(ch_diar_ors, na.rm = TRUE, vartype = "var"),
            ch_allvac_either = survey_mean(ch_allvac_either, na.rm = TRUE, vartype = "var"),
            nt_ch_gwmt_any = survey_mean(nt_ch_gwmt_any, na.rm = TRUE, vartype = "var"),
            nt_ch_micro_iron = survey_mean(nt_ch_micro_iron, na.rm = TRUE, vartype = "var")) 
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
  
  # df_crosstab <- dhs %>%
  #   filter(!is.na(get(myvar))) %>%
  #   summarise(obs_un = n(),
  #             obs_wn = sum(wt)) %>%
  #   mutate(variable = myvar)
  # should do same as above, but has consistent syntax with adm2 approach
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

write.csv(est_adm2_forplot, file = "./gen/calculate-direct/temp/direct-child-forplot.csv", row.names = FALSE)
write.csv(est_adm2_combined, file = "./gen/calculate-direct/temp/direct-child.csv", row.names = FALSE)
write.csv(est_adm1, file = "./gen/calculate-direct/temp/direct-child-adm1.csv", row.names = FALSE)
write.csv(est_adm0, file = "./gen/calculate-direct/temp/direct-child-adm0.csv", row.names = FALSE)
