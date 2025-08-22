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
# Bangladesh district boundaries
#bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
# Adjacency matrix
#prep <- readRDS("./gen/prepare-shp/output/adjacency_matrix.rds")
# model info from previous models if it exists
audit_files <- dir("./gen/model/audit/")
if(sum(grepl("model-info", audit_files)) > 0){
  old_modinfo <- read.csv("./gen/model/audit/model-info.csv")
}else{
  old_modinfo <- data.frame()
}
# newly created list of indicators
ind <- read_excel("./data/ind-info.xlsx", sheet = "indicators")
################################################################################

# merge outcome variables with covariates
dat <- merge(est, covar, by = "ADM2_EN")

# subset to included indicators
df_ind <- subset(ind, status == "include")
unique(df_ind$covar_grp)

# anc ---------------------------------------------------------------------

# Possible covar 
# Excluding child_age, because doesn't make sense for these covariates
# After running once, excluding wealth_index because colinear with mother_edu and residence (see notes below)
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
v_covar <- c("mother_edu", "mother_age", "residence", "hhd_under5", "hhd_head_age", "hhd_head_sex", "wealth_index")
# excluded: "child_age", 

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
# Dropping child_age and running again, because was not important at all in ors and zinc, and super important in allvac
# wealth_index, hhd_head_age, hhd_head_sex, mother_age
# Will go with... (based on second plot)
# hhd_head_sex, hhd_head_age, wealth_index


# ch_nut ----------------------------------------------------------------

# Possible covar 
v_covar <- c("mother_edu", "child_age", "mother_age", "residence", "hhd_under5", "hhd_head_age", "hhd_head_sex", "wealth_index")
# excluded: none

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
# interesting because there was not much overlap in terms of the most important covariates for each indicator (plot 1)
# hhd_head_sex was important only for nt_ch_micro_mp, but then not important in the model with one covariate
# hhd_head_age is second most important for nt_ch_micro_vas, but not important at all for other two
# Will go with...
# mother_edu, hhd_under5, mother_age

# # ebf ----------------------------------------------------------------
# 
# # Possible covar 
# # Excluding child_age, because doesn't make sense for this mother-level covariate
# v_covar <- c("mother_edu", "mother_age", "residence", "hhd_under5", "hhd_head_age", "hhd_head_sex", "wealth_index")
# # excluded: "child_age"
# 
# # indicators in this group
# v_ind <- subset(df_ind, covar_grp == "ebf")$variable
# 
# df_res_all <- fn_imp_compare(v_ind, v_covar)
# 
# # covariate importance for each indicator from full model to model with 1 covariate
# df_res_all %>%
#   ggplot() +
#   geom_bar(aes(x=covar, y = IncMSE, fill = covar), stat = "identity") +
#   geom_hline(aes(yintercept = 0), color = "red") +
#   labs(title = "Covariate importance by indicator", y = "Importance") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   facet_grid(variable ~ ncovar)
# 
# # ranked covariate importance by number of covariates in model, collapsed across indicators
# # Can see the number of times a covariate was most important (rank 1) in model, by number of covariates in that model
# df_res_all %>%
#   arrange(variable, -ncovar, -IncMSE) %>%
#   group_by(ncovar, variable) %>%
#   mutate(rank = 1:n()) %>% 
#   group_by(covar, ncovar, rank) %>% 
#   summarise(nrank = n()) %>% 
#   ggplot() +
#   geom_bar(aes(x=rank, y = nrank, fill = covar), stat = "identity") +
#   geom_hline(aes(yintercept = 0), color = "red") +
#   labs(title = "Covariate importance rank by number of cov in model", y = "N", x = "Rank") +
#   theme_bw() +
#   facet_grid(covar ~ ncovar)
# 
# ## 1st time running with all covariates except child_age
# # clear cut case with just one indicator
# # Will go with...
# # hhd_under5, hhd_head_age, residence

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

# wm_micro ----------------------------------------------------------------

# Possible covar 
# Excluding child_age, because doesn't make sense for these covariates
# After running once, excluding residence (see notes below)
v_covar <- c("mother_edu",  "hhd_under5", "hhd_head_age", "hhd_head_sex", "wealth_index")
# excluded: "child_age", "residence", "mother_age"

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
# hhd_under5, wealth_index
# residence or mother_age
# residence and wealth_index are highly colinear though
## exclude residence as seems less important than wealth_index
# hhd_under5, mother_age are top two
## excluding mother_age because is colinear with hhd_under5 and less important 
# hhd_under5, hhd_head_sex, wealth_index




