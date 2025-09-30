################################################################################
#' @description Test covariate importance by covariate group
#' @return Use to fill out "covariates" tab of data/ind-info by hand
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)
#' Inputs
source("./src/util.R")
# list of indicators
ind <- read_excel("./data/ind-info.xlsx", sheet = "indicators")
indcovar <- read_excel("./data/ind-info.xlsx", sheet = "covariates")
################################################################################

head(ind)
head(indcovar)

# outcomes table ----------------------------------------------------------

# see latex doc for extra formatting

subset(ind, status == "include")
outtab <- ind %>%
  filter(status == "include") %>%
  select(covar_grp, description) %>%
  mutate(description = gsub(" \\(previous 5 years\\)", "", description)) %>%
  mutate(description = gsub(" \\(previous 3 months\\)", "", description)) %>%
  mutate(description = gsub(" \\(last birth in previous 24 months\\)", "", description)) %>%
  mutate(description = gsub(" during last pregnancy", "", description)) %>%
  mutate(description = gsub(" with a birth in the past five years", "", description)) %>%
  mutate(covar_grp = case_when(
    covar_grp  == "anc" ~ "ANC",
    covar_grp  == "ch_nut" ~ "Child nutrition",
    covar_grp  == "ch_trtmnt" ~ "Treatment/intervention",
    covar_grp  == "del_pnc" ~ "Delivery/PNC",
    covar_grp  == "ph" ~ "Household",
    covar_grp  == "wm_micro" ~ "Women micronutrients",
    TRUE ~ covar_grp
  ))
outtab1 <- outtab %>% filter(covar_grp %in% c("ANC", "Child nutrition"))
outtab2 <- outtab %>% filter(!(covar_grp %in% outtab1$covar_grp))

outtab1 %>%
  kable("latex", booktabs = TRUE, 
        align = "ll", row.names = FALSE, 
        col.names = c("Type", "Indicator")) %>%
  collapse_rows(1) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 4)
outtab2 %>%
  kable("latex", booktabs = TRUE, 
        align = "ll", row.names = FALSE, 
        col.names = c("Type", "Indicator")) %>%
  collapse_rows(1) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 4)

# covariate table ---------------------------------------------------------

v_covargrp_order <- unique(indcovar$covar_grp)

covartab <- indcovar %>%
  select(covar_grp, test3) %>%
  mutate(covar_grp = case_when(
    covar_grp  == "anc" ~ "ANC",
    covar_grp  == "ch_nut" ~ "Child nutrition",
    covar_grp  == "ch_trtmnt" ~ "Treatment/intervention",
    covar_grp  == "del_pnc" ~ "Delivery/PNC",
    covar_grp  == "ph" ~ "Household",
    covar_grp  == "wm_micro" ~ "Women micronutrients",
    TRUE ~ covar_grp
  )) %>%
  mutate(test3 = case_when(
    test3 == "mother_edu" ~ "Mother's education",
    test3 == "wealth_index" ~ "Household wealth index",
    test3 == "hhd_under5" ~ "Number of children under-5 in household",
    test3 == "hhd_head_age" ~ "Age of household head",
    test3 == "hhd_head_sex" ~ "Sex of household head",
    test3 == "mother_age" ~ "Mother's age",
    test3 == "child_age" ~ "Child's age",
    TRUE ~ test3
  )) %>%
  mutate(covar_grp = factor(covar_grp, 
                            levels = c("ANC", "Child nutrition", "Treatment/intervention", 
                            "Delivery/PNC", "Household", "Women micronutrients"))) %>%
  arrange(covar_grp, test3)

covartab1 <- covartab %>% filter(covar_grp %in% c("ANC", "Child nutrition",  "Treatment/intervention"))
covartab2 <- covartab %>% filter(!(covar_grp %in% covartab1$covar_grp))

covartab1 %>%
  kable("latex", booktabs = TRUE, 
        align = "ll", row.names = FALSE, 
        col.names = c("Type", "Covariates")) %>%
  collapse_rows(1) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 4)
covartab2 %>%
  kable("latex", booktabs = TRUE, 
        align = "ll", row.names = FALSE, 
        col.names = c("Type", "Covariates")) %>%
  collapse_rows(1) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 4)
