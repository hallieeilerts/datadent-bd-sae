################################################################################
#' @description Calculate household member covariates for model input
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(haven)
library(tidyverse)
library(labelled)   # used for Haven labeled variable creation
#' Inputs
source("./src/util.R")
## DHS household recode
dat <- read_dta("./data/BD_2022_DHS_03042025_2114_120781/BDPR81DT/BDPR81FL.DTA")
################################################################################

dat_prep <- fn_prepPR(dat)



# //Highest level of schooling attended or completed
dat_prep <- dat_prep %>%
  mutate(ph_highest_edu =
           case_when(hv103==1 & hv105>5 ~ hv109))
var_label(dat_prep$ph_highest_edu) <- "Highest level of schooling attended or completed among those age 6 or over"

## Household size

# keep only the variables we need
PR_temp <- select(dat_prep, hv001, hv002, hvidx, hv005, hv009, hv024, hv025,hv270,
                  starts_with("hv10"),starts_with("hv11"), starts_with("ph_"))

# Prepare a file of potential mothers
PR_temp_mothers <- PR_temp %>%
  filter(hv104!=1) %>%
  filter(!(hv105<15))
PR_temp_mothers <- select(PR_temp_mothers, hv001, hv002, hvidx, hv102)
PR_temp_mothers <- PR_temp_mothers %>% rename(hv102_mo=hv102, hv112=hvidx)  

# Prepare a file of potential fathers
PR_temp_fathers <- PR_temp %>%
  filter(hv104!=2) %>%
  filter(!(hv105<15))
PR_temp_fathers <- select(PR_temp_fathers,hv001, hv002, hvidx, hv102)
PR_temp_fathers <- PR_temp_fathers %>% rename(hv102_fa=hv102, hv114=hvidx)  

# Prepare file of children for merges
PR_temp_children <- PR_temp %>%
  filter(hv102!=0) %>%
  filter(!(hv105>17)) %>%
  mutate(in_children = 1)

# Merge children with potential mothers
PR_temp_children <- merge(PR_temp_children, PR_temp_mothers, by = c("hv001", "hv002", "hv112"), all.y = TRUE, all.x = TRUE)

# Merge children with potential fathers
PR_temp_children <- merge(PR_temp_children, PR_temp_fathers, by = c("hv001", "hv002", "hv114"), all.y = TRUE, all.x = TRUE)

# Code 99 of the mother or father is not de jure
PR_temp_children[["hv112r"]] <- ifelse(PR_temp_children[["hv112"]]>0 & (PR_temp_children[["hv102_mo"]]==0 & !is.na(PR_temp_children[["hv102_mo"]])), 99, PR_temp_children[["hv112"]]) 
PR_temp_children[["hv114r"]] <- ifelse(PR_temp_children[["hv114"]]>0 & (PR_temp_children[["hv102_fa"]]==0 & !is.na(PR_temp_children[["hv102_fa"]])), 99, PR_temp_children[["hv114"]]) 

PR_temp_children <- PR_temp_children %>%
  filter(in_children==1)

# *** Household characteristics *** 
PR_temp_wk <- PR_temp %>% 
  mutate(n=1) %>%  
  filter(hv102==1) 

PR_temp_wk <- select(PR_temp_wk, hv001, hv002, hvidx,n)

PR_temp_children0 <- merge(PR_temp_children, PR_temp_wk, by = c("hv001", "hv002", "hvidx"), all.y = TRUE)

# //Household size
HHSIZE <- PR_temp_children0 %>% 
  group_by(hv001, hv002) %>% 
  summarise(hhsize = sum(n, na.rm=TRUE)) 

PR_temp_children0 <- merge(PR_temp_children0, HHSIZE, by = c("hv001", "hv002"), all.y = TRUE)

dat_var <- PR_temp_children0 %>%
  select(hv001, hv002, hvidx, hv024, hv025, ph_wealth_quint, ph_highest_edu, ph_hhhead_sex, hhsize) %>%
  mutate(hhid = paste(hv001, hv002, sep = "_"))

# Save --------------------------------------------------------------------

saveRDS(dat_var, file = "./gen/prepare-model-input/temp/variables-pr.rds")

