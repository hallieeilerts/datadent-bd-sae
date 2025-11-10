################################################################################
#' @description Test covariate importance by covariate group
#' @return Use to fill out "covariates" tab of data/ind-info by hand
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(dplyr)
library(tidyr)
library(here)
library(randomForest)
library(ggplot2)
library(readr)
library(readxl)
library(ggplot2)
library(car)
#' Inputs
source("./src/util.R")
# Direct estimates and variance of outcomes
est <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
# District-level covariates
covar <- read.csv("./gen/prepare-dhs/output/covar-district.csv")
# list of indicators
ind <- read_excel("./data/ind-info.xlsx", sheet = "indicators")
################################################################################

# merge outcome variables with covariates
dat <- merge(est, covar, by = c("ADM2_EN", "variable"))

# subset to included indicators
df_ind <- subset(ind, status == "include")
unique(df_ind$covar_grp)

# anc ---------------------------------------------------------------------

# Possible covar 
# Excluding child_age, because doesn't make sense for ANC covariates
# After running once, excluding wealth_index because collinear with mother_edu and residence (see notes below)
v_covar <- c("mother_edu",  "mother_age", "residence", "hhd_under5", "hhd_head_age", "hhd_head_sex")
# excluded: "wealth_index", "child_age"

# indicators in this group
v_ind <- subset(df_ind, covar_grp == "anc")$variable

df_res_all <- data.frame()
for(i in 1:length(v_ind)){
  
  indi <- v_ind[i]

  v_covarAux <- NULL
  df_res_ind <- data.frame()
  for(j in length(v_covar):1){
    
    if(is.null(v_covarAux)){
      v_covarj <- head(v_covar, j)
    }else{
      v_covarj <- head(v_covarAux, j)
    }
    
    df <- subset(dat, variable == indi)
    df <- df[complete.cases(df[, c("dir", v_covarj)]), ]
    
    # Test using random forest with v_covarj
    rf_model <- randomForest(as.formula(paste("dir ~", paste(v_covarj, collapse = " + "))),
                             data = df, importance = TRUE)
    df_importance <- as.data.frame(importance(rf_model, type = 1))
    names(df_importance)[1] <- "IncMSE"
    df_importance$covar <- row.names(df_importance)
    row.names(df_importance) <- NULL
    
    # order v_covarj so variable of least importance is dropped
    df_importance <- df_importance[order(-df_importance$IncMSE),]
    v_covarAux <- df_importance$covar
    
    # Add to results
    df_importance$ncovar <- j
    df_res_ind <- rbind(df_res_ind, df_importance)
    
  }
  
  df_res_ind$variable <- indi
  df_res_all <- rbind(df_res_all, df_res_ind)
  
}

# covariate importance for each indicator from full model to model with 1 covariate
df_res_all %>%
  ggplot() +
  geom_bar(aes(x=covar, y = IncMSE, fill = covar), stat = "identity") +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(title = "Covariate importance by indicator", y = "Importance") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(variable ~ ncovar)

# covariate importance collapse across indicators
# note that doesn't take into account that some will be colinear
# so it may show maternal_edu and wealth_index as having highest importance scores in model with 1 or 2 covariates, when it's really an either/or situation in each model
df_res_all %>% 
  group_by(covar, ncovar) %>%
  summarise(IncMSE = sum(IncMSE)) %>%
  ggplot() +
  geom_bar(aes(x=covar, y = IncMSE, fill = covar), stat = "identity") +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(title = "Covariate importance by covariate group", y = "Importance") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ncovar)

# ranked covariate importance by number of covariates in model, collapsed across indicators
# Can see the number of times a covariate was most important (rank 1) in model, by number of covariates in that model
df_res_all %>%
  arrange(variable, -ncovar, -IncMSE) %>%
  group_by(ncovar, variable) %>%
  mutate(rank = 1:n()) %>% 
  group_by(covar, ncovar, rank) %>% 
  summarise(nrank = n()) %>% 
  ggplot() +
  geom_bar(aes(x=rank, y = nrank, fill = covar), stat = "identity") +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(title = "Covariate importance rank by number of cov in model", y = "N", x = "Rank") +
  theme_bw() +
  facet_grid(covar ~ ncovar)

## 1st time running with all covariates except child_age
# mother_edu has the most number one rankings (plot 3)
# then hhd_under5
# then mother_age, residence, and wealth_index
## Check collinearity of these
# wealth_index is highly collinear with mother_edu and residence, so will exclude and run again
## Pick top three covariates (from plots 1 and 3)
# mother_edu, hhd_under5, mother_age
# could have gone with hhd_head_age, but mother's age seems better for ANC variables

# Define function to do same comparisons for each group -------------------

fn_imp_compare <- function(v_ind, v_covar){
  
  df_res_all <- data.frame()
  for(i in 1:length(v_ind)){
    
    indi <- v_ind[i]
    
    v_covarAux <- NULL
    df_res_ind <- data.frame()
    for(j in length(v_covar):1){
      
      if(is.null(v_covarAux)){
        v_covarj <- head(v_covar, j)
      }else{
        v_covarj <- head(v_covarAux, j)
      }
      
      df <- subset(dat, variable == indi)
      df <- df[complete.cases(df[, c("dir", v_covarj)]), ]
      
      # Test using random forest with v_covarj
      rf_model <- randomForest(as.formula(paste("dir ~", paste(v_covarj, collapse = " + "))),
                               data = df, importance = TRUE)
      df_importance <- as.data.frame(importance(rf_model, type = 1))
      names(df_importance)[1] <- "IncMSE"
      df_importance$covar <- row.names(df_importance)
      row.names(df_importance) <- NULL
      
      # order v_covarj so variable of least importance is dropped
      df_importance <- df_importance[order(-df_importance$IncMSE),]
      v_covarAux <- df_importance$covar
      
      # Add to results
      df_importance$ncovar <- j
      df_res_ind <- rbind(df_res_ind, df_importance)
      
    }
    
    df_res_ind$variable <- indi
    df_res_all <- rbind(df_res_all, df_res_ind)
    
  }
  return(df_res_all)
}

# ch_trtmnt ----------------------------------------------------------------

# Possible covar 
v_covar <- c("child_age",  "mother_edu", "mother_age", "residence", "hhd_head_sex", "wealth_index")
# excluded: "child_age",  "hhd_head_age", "hhd_under5",

# indicators in this group
v_ind <- subset(df_ind, covar_grp == "ch_trtmnt")$variable

df_res_all <- fn_imp_compare(v_ind, v_covar)

# covariate importance for each indicator from full model to model with 1 covariate
df_res_all %>%
  ggplot() +
  geom_bar(aes(x=covar, y = IncMSE, fill = covar), stat = "identity") +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(title = "Covariate importance by indicator", y = "Importance") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(variable ~ ncovar)

# ranked covariate importance by number of covariates in model, collapsed across indicators
# Can see the number of times a covariate was most important (rank 1) in model, by number of covariates in that model
df_res_all %>%
  arrange(variable, -ncovar, -IncMSE) %>%
  group_by(ncovar, variable) %>%
  mutate(rank = 1:n()) %>% 
  group_by(covar, ncovar, rank) %>% 
  summarise(nrank = n()) %>% 
  ggplot() +
  geom_bar(aes(x=rank, y = nrank, fill = covar), stat = "identity") +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(title = "Covariate importance rank by number of cov in model", y = "N", x = "Rank") +
  theme_bw() +
  facet_grid(covar ~ ncovar)

## 1st time running with all covariates
# child_age, hhd_head_sex, hhd_head_age, mother_age are important
## Dropping child_age and running again, because was not important at all in ors and zinc, and super important in allvac
# mother_age, hhd_head_age, hhd_head_sex, hhd_under5, wealth_index
## mother_age is highly correlated with hhd_under5, not hhd_head_age... going to drop it and see what happens though.
## hhd_head_age is only really important for ors. Try dropping that one instead of mother_age.
# looks better. mother_age important to allvac and zinc.
# hhd_head_sex, mother_age, hhd_under5, residence
## dropping hhd_under5 because highly correlated with mother_age
# hhd_head_sex, mother_age, wealth_index

## After I fit all the models, these covariates didn't work well for ch_diar_ors
# ch_allvac_either was also not fine in the model with 2 covariates. but this no big deal.
# wealth_index was third covariate. so seems to have been causing problems
# testing VIF and with wealth_index, variance for the three covar neared 1 or more
# i tried hhd_head_sex, mother_age, mother_edu (instead of wealth_index) - still high
# taking a different approach-- try hhd_head_sex, hhd_head_age, hhd_under5
# because would be nice to include number of kids under five, and this one is less correlated with mother_age
# nevermind = VIF is close to 1.5 for some indicators
# go back to hhd_head_sex, hhd_head_age, mother_edu (instead of wealth_index)
# looks good.

## Third time. These covariates didn't work well for all 3-- ch_diar_zinc, ch_diar_ors, ch_allvac_either.
# Including all covariates
# keep mother_age, drop hhd_head_age


vif_res <- data.frame()
for(i in 1:length(v_ind)){
  
  df <- subset(dat, variable == v_ind[i])
  df <- df[complete.cases(df[, c("dir", "child_age", "mother_age", "wealth_index")]), ]
  nrow(dat)
  nrow(df) # 64, all districts have values
  mod_vif <- car::vif(lm(dir ~ child_age + mother_age + wealth_index, data = df))
  df_vif <- as.data.frame(mod_vif)
  names(df_vif)[1] <- "VIF"
  df_vif$covar <- row.names(df_vif)
  df_vif$variable <- v_ind[i]
  vif_res <- rbind(df_vif, vif_res)
}
vif_res %>%
  ggplot() +
  geom_bar(aes(x=covar, y = VIF), stat = "identity") +
  labs(y = "VIF") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~variable)
# hhd_head_sex, mother_age, child_age increases variance for ch_allvac_either to 1.2
# changing child_age to residence doesn't help. still 1.25 at highest
# changing to mother_edu even worse
# hhd_head_sex + mother_age + wealth_index is ok. but all covar slightly over 1 for all outcome (except wealth index for all_vac)
# hhd_head_sex + hhd_under5 + wealth_index is very bad.
# hhd_head_sex + hhd_under5 + mother_edu is worse.
# hhd_head_sex + hhd_head_age + wealth_index is bad
# hhd_head_sex + hhd_head_age + mother_edu -- all are close to 1. this was what we just tested which didn't work great.
# hhd_head_sex + mother_age + wealth_index -- not bad
# child_age + mother_age + wealth_index -- best so far, all under 1
# child_age + hhd_head_age + wealth_index -- much worse


# ch_nut ----------------------------------------------------------------

# Possible covar 
v_covar <- c("mother_edu", "child_age",  "residence", "hhd_under5", "hhd_head_age", "hhd_head_sex", "wealth_index")
# excluded: "mother_age",

# indicators in this group
v_ind <- subset(df_ind, covar_grp == "ch_nut")$variable

df_res_all <- fn_imp_compare(v_ind, v_covar)

# covariate importance for each indicator from full model to model with 1 covariate
df_res_all %>%
  ggplot() +
  geom_bar(aes(x=covar, y = IncMSE, fill = covar), stat = "identity") +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(title = "Covariate importance by indicator", y = "Importance") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(variable ~ ncovar)

# ranked covariate importance by number of covariates in model, collapsed across indicators
# Can see the number of times a covariate was most important (rank 1) in model, by number of covariates in that model
df_res_all %>%
  arrange(variable, -ncovar, -IncMSE) %>%
  group_by(ncovar, variable) %>%
  mutate(rank = 1:n()) %>% 
  group_by(covar, ncovar, rank) %>% 
  summarise(nrank = n()) %>% 
  ggplot() +
  geom_bar(aes(x=rank, y = nrank, fill = covar), stat = "identity") +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(title = "Covariate importance rank by number of cov in model", y = "N", x = "Rank") +
  theme_bw() +
  facet_grid(covar ~ ncovar)

## 1st time running with all covariates
# interesting because there was no overlap in terms of the most important covariate for each indicator (plot 1)
# hhd_under5 and hhd_head_age seem to be slightly more important than others
## removing mother_age because of collinearity of hhd_under5 and only wanting one age-related covar (hhd_head_age)
# hhd_under5, hhd_head_age, wealth_index (second plot makes it seem like third in importance)

## After I fit all the models, these covariates didn't work well for nt_ebf and nt_ch_micro_mp
# seems like wealth_index was most problematic
# testing VIF and with wealth_index, variance for the three covar neared 1.25
# swapping for mothers_edu - tended to keep VIF below 1
# swapping for residence - high again
vif_res <- data.frame()
for(i in 1:length(v_ind)){
  
  df <- subset(dat, variable == v_ind[i])
  df <- df[complete.cases(df[, c("dir", "hhd_under5", "hhd_head_age","mother_edu")]), ]
  nrow(dat)
  nrow(df) # 64, all districts have values
  mod_vif <- car::vif(lm(dir ~ hhd_under5 + hhd_head_age + mother_edu, data = df))
  df_vif <- as.data.frame(mod_vif)
  names(df_vif)[1] <- "VIF"
  df_vif$covar <- row.names(df_vif)
  df_vif$variable <- v_ind[i]
  vif_res <- rbind(df_vif, vif_res)
}
vif_res %>%
  ggplot() +
  geom_bar(aes(x=covar, y = VIF), stat = "identity") +
  labs(y = "VIF") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~variable)

# del_pnc ----------------------------------------------------------------

# Possible covar 
# Excluding child_age, because doesn't make sense for this mother-level covariate
# After running once, excluding wealth_index because has high importance and is colinear with mother_edu
# After running twice, excluding mother_age because is colinear with hhd_under5
v_covar <- c("mother_edu",  "residence", "hhd_under5", "hhd_head_age", "hhd_head_sex")
# excluded: "child_age", "wealth_index", "mother_age",

# indicators in this group
v_ind <- subset(df_ind, covar_grp == "del_pnc")$variable

df_res_all <- fn_imp_compare(v_ind, v_covar)

# covariate importance for each indicator from full model to model with 1 covariate
df_res_all %>%
  ggplot() +
  geom_bar(aes(x=covar, y = IncMSE, fill = covar), stat = "identity") +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(title = "Covariate importance by indicator", y = "Importance") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(variable ~ ncovar)

# ranked covariate importance by number of covariates in model, collapsed across indicators
# Can see the number of times a covariate was most important (rank 1) in model, by number of covariates in that model
df_res_all %>%
  arrange(variable, -ncovar, -IncMSE) %>%
  group_by(ncovar, variable) %>%
  mutate(rank = 1:n()) %>% 
  group_by(covar, ncovar, rank) %>% 
  summarise(nrank = n()) %>% 
  ggplot() +
  geom_bar(aes(x=rank, y = nrank, fill = covar), stat = "identity") +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(title = "Covariate importance rank by number of cov in model", y = "N", x = "Rank") +
  theme_bw() +
  facet_grid(covar ~ ncovar)

## 1st time running with all covariates except child_age
# hhd_under5, mother_edu, wealth_index
## excluding wealth_index because collinearity with mother_edu. and mother_edu seems slightly more important to more models. pnc_nb and pnc_wm are essentially the same ind, and both can do wealth_index or mother_edu.
# mother_edu, hhd_head_under5
## excluding mother_age because is colinear with hhd_under5 and less important 
# mother_edu, hhd_under5, hhd_head_sex (based on second plot, choosing this over hhd_head_age)

# ph ----------------------------------------------------------------

# Possible covar 
# Excluding child_age and mother_age, because don't make sense for this household-level covariate
# After running once, excluding mother_edu because has high importance and is colinear with wealth_index
# also not really clear which mother
v_covar <- c("residence", "hhd_under5", "hhd_head_age", "hhd_head_sex", "wealth_index")
# excluded: "child_age",  "mother_age", "mother_edu", 

# indicators in this group
v_ind <- subset(df_ind, covar_grp == "ph")$variable

df_res_all <- fn_imp_compare(v_ind, v_covar)

# covariate importance for each indicator from full model to model with 1 covariate
df_res_all %>%
  ggplot() +
  geom_bar(aes(x=covar, y = IncMSE, fill = covar), stat = "identity") +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(title = "Covariate importance by indicator", y = "Importance") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(variable ~ ncovar)

# ranked covariate importance by number of covariates in model, collapsed across indicators
# Can see the number of times a covariate was most important (rank 1) in model, by number of covariates in that model
df_res_all %>%
  arrange(variable, -ncovar, -IncMSE) %>%
  group_by(ncovar, variable) %>%
  mutate(rank = 1:n()) %>% 
  group_by(covar, ncovar, rank) %>% 
  summarise(nrank = n()) %>% 
  ggplot() +
  geom_bar(aes(x=rank, y = nrank, fill = covar), stat = "identity") +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(title = "Covariate importance rank by number of cov in model", y = "N", x = "Rank") +
  theme_bw() +
  facet_grid(covar ~ ncovar)

## 1st time running with all covariates except child_age and mother_age
# wealth_index most important, then mother_edu and hhd_head_age
## exclude mother_edu due to collinearity
# Will go with...
# wealth_index, hhd_head_age, hhd_under5

## After I fit all the models, these covariates didn't work well for ph_wtr_improve (both second and third model)
# ph_wtr_trt_appr also not great but not terrible
# with wealth_index + hhd_head_age + hhd_under5 - VIF is close to 1.25
# adding in hhd_head_sex which was most important for ph_wtr_improve
# wealth_index + hhd_head_sex + hhd_under5 -- VIF looks better, around 1.25
# wealth_index + hhd_head_sex + residence -- VIF high again
# wealth_index + hhd_head_sex + hhd_head_age -- VIF <1.2 (best)

vif_res <- data.frame()
for(i in 1:length(v_ind)){
  
  df <- subset(dat, variable == v_ind[i])
  df <- df[complete.cases(df[, c("dir", "wealth_index", "hhd_head_sex", "hhd_head_age")]), ]
  nrow(dat)
  nrow(df) # 64, all districts have values
  mod_vif <- car::vif(lm(dir ~ wealth_index + hhd_head_sex + hhd_head_age, data = df))
  df_vif <- as.data.frame(mod_vif)
  names(df_vif)[1] <- "VIF"
  df_vif$covar <- row.names(df_vif)
  df_vif$variable <- v_ind[i]
  vif_res <- rbind(df_vif, vif_res)
}
vif_res %>%
  ggplot() +
  geom_bar(aes(x=covar, y = VIF), stat = "identity") +
  labs(y = "VIF") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~variable)

# wm_micro ----------------------------------------------------------------

# Possible covar 
# Excluding child_age, because doesn't make sense for these covariates
# After running once, excluding residence (see notes below)
v_covar <- c("mother_edu",  "hhd_under5", "hhd_head_age", "hhd_head_sex", "wealth_index")
# excluded: "child_age", "residence", "mother_age"
v_covar <- c("mother_edu",  "hhd_under5", "hhd_head_age", "hhd_head_sex", "wealth_index")
# excluded: "child_age",  "mother_age", "residence"

# indicators in this group
v_ind <- subset(df_ind, covar_grp == "wm_micro")$variable

df_res_all <- fn_imp_compare(v_ind, v_covar)

# covariate importance for each indicator from full model to model with 1 covariate
df_res_all %>%
  ggplot() +
  geom_bar(aes(x=covar, y = IncMSE, fill = covar), stat = "identity") +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(title = "Covariate importance by indicator", y = "Importance") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(variable ~ ncovar)

# ranked covariate importance by number of covariates in model, collapsed across indicators
# Can see the number of times a covariate was most important (rank 1) in model, by number of covariates in that model
df_res_all %>%
  arrange(variable, -ncovar, -IncMSE) %>%
  group_by(ncovar, variable) %>%
  mutate(rank = 1:n()) %>% 
  group_by(covar, ncovar, rank) %>% 
  summarise(nrank = n()) %>% 
  ggplot() +
  geom_bar(aes(x=rank, y = nrank, fill = covar), stat = "identity") +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(title = "Covariate importance rank by number of cov in model", y = "N", x = "Rank") +
  theme_bw() +
  facet_grid(covar ~ ncovar)

## 1st time running with all covariates except child_age
# note that nt_wm_micro_iron and nt_wm_micro_iron_any are essentially the same indicators
# hhd_under5, residence
## will remove mother_age due to collinearity with hhd_under5
# hhd_under5,
# then wealth_index from second plot
## exclude residence due to collinearity with wealth_index
# hhd_under5, hhd_head_age, wealth_index
# don't want to include both maternal_edu and wealth_index due to collinearity


