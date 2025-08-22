################################################################################
#' @description Fit model using summer
#' @return Model fit
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(sae)
library(SUMMER)
library(survey)
library(srvyr)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
if (!isTRUE(requireNamespace("INLA", quietly = TRUE))) {
  install.packages("INLA", repos=c(getOption("repos"), 
                                   INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
}
#' Inputs
source("./src/util.R")
# DHS data for survey design
dhs_mth <- read.csv("./gen/prepare-dhs/output/dat-mother.csv")
dhs_chld <- read.csv("./gen/prepare-dhs/output/dat-child.csv")
dhs_hhd <- read.csv("./gen/prepare-dhs/output/dat-household.csv")
dhs_bth <- read.csv("./gen/prepare-dhs/output/dat-birth.csv")
# Direct estimates and variance
est <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
# District-level covariates
covar <- read.csv("./gen/prepare-dhs/output/covar-district.csv")
# Bangladesh district boundaries
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
# Adjacency matrix
prep <- readRDS("./gen/prepare-shp/output/adjacency_matrix.rds")
# Model info
audit_files <- dir("./gen/model/audit/")
if(sum(grepl("model-info-summer", audit_files)) > 0){
  old_modinfo <- read.csv("./gen/model/audit/model-info-summer.csv")
}else{
  old_modinfo <- data.frame()
}
# indicator info for which covariates to use
ind <- read_excel("./data/ind-info.xlsx", sheet = "indicators")
indcovar <- read_excel("./data/ind-info.xlsx", sheet = "covariates")
################################################################################

# set model
model_name <- "SummerArealBYM2"
model_file <- ""
v_cov <- c("wealth_index", "hhd_under5", "hhd_head_age", "hhd_head_sex", "mother_age", "child_age")
v_cov <-  v_cov[5]
v_cov <- 1

# adjacency matrix
mat <- getAmat(bangladesh_2, bangladesh_2$ADM2_EN)
# vector of outcomes
v_var <- unique(est$variable)
# empty dataframe for storing model info
modinfo <- data.frame()

v_var <- "rh_anc_1vs"

for(i in 1:length(v_var)){

  myoutcome <- v_var[i]
  print(myoutcome)
  # direct estimates
  direct <- subset(est, variable == myoutcome)
  # covariates
  # if a mother level covariate, do not include covariate for child age # BECAUSE IT IS THE AGE OF CHILD FOR OTHER INDICATORS... NOT SUBJECT OF THIS INDICATOR
  if(myoutcome %in% c("nt_wm_micro_iron", "rh_anc_4vs", "rh_anc_1vs", "rh_anc_1tri", "nt_ebf",
                      "nt_wm_micro_iron_any", "rh_anc_bldpres", "rh_anc_urine", "rh_anc_iron",
                      "rh_anc_bldsamp", "rh_anc_wgt", "rh_pnc_wm_2days", "rh_pnc_nb_2days", "rh_pnc_wm_bfcounsel",
                      "rh_del_inst", "rh_del_pvskill")){
    v_cov_mod <- v_cov[!(v_cov %in% "child_age")]
  }
  # if a child level covariate, do not include covariate for mother age # REVISIT THIS. I THINK FOR CHILD LEVEL COULD INCLUDE MOTHER AGE.
  if(myoutcome %in% c("nt_ch_micro_vas", "nt_ch_micro_dwm", "ch_diar_ors", "ch_diar_zinc","nt_ch_micro_mp")){
    v_cov_mod <- v_cov[!(v_cov %in% "mother_age")]
  }
  # if a household level covariate, don't include either
  if(myoutcome %in% c("ph_wtr_improve", "ph_wtr_trt_appr", "ph_sani_improve", "ph_hndwsh_basic")){
    v_cov_mod <-  v_cov[!(v_cov %in% c("child_age", "mother_age"))]
  }
  v_cov_mod <- sort(v_cov_mod)
  
  # check that there are any covariates. because there will be zero if the only covariate with child_age or mother_age.
  if(length(v_cov_mod) != 0){
    # survey design
    if(myoutcome %in% c("nt_ebf", "nt_ch_micro_vas", "nt_ch_micro_dwm", "ch_diar_ors", "ch_diar_zinc","nt_ch_micro_mp")){
      dhs <- dhs_chld
    }
    if(myoutcome %in% c("nt_wm_micro_iron", "rh_anc_4vs", "rh_anc_1vs", "rh_anc_1tri",  "nt_ebf",
                        "nt_wm_micro_iron_any", "rh_anc_bldpres", "rh_anc_urine", "rh_anc_iron",
                        "rh_anc_bldsamp", "rh_anc_wgt", "rh_pnc_wm_2days", "rh_pnc_nb_2days", "rh_pnc_wm_bfcounsel",
                        "rh_del_inst", "rh_del_pvskill")){
      dhs <- dhs_mth
    }
    if(myoutcome %in% c("rh_del_inst", "rh_del_pvskill")){
      dhs <- dhs_bth
    }
    if(myoutcome %in% c("ph_wtr_improve", "ph_wtr_trt_appr", "ph_sani_improve", "ph_hndwsh_basic")){
      dhs <- dhs_hhd
      dhs$v001 <- dhs$hv001
      dhs$v023 <- dhs$hv023
    }
    design <- dhs %>% as_survey_design(ids = "v001", # psu
                                       strata = "v023", # strata for sampling
                                       weights = "wt",
                                       nest = TRUE)
    #X_covar <- covar %>% select(ADM2_EN, all_of(v_cov_mod)) # Should have area name and covariates
    
    # model formula
    fmla <- as.formula(paste(myoutcome, "~", paste(v_cov_mod, collapse = " + ")))
    
    # Including area level covariates in continuous response model
    summer.brfss <- smoothArea(fmla,
                               design = design,
                               X.domain = covar,
                               domain= ~ADM2_EN,
                               adj.mat = mat, 
                               transform = "logit",
                               level = 0.95)
    
    # Save model info
    n_vers <- old_modinfo %>% 
      filter(outcome == myoutcome) %>%
      nrow() + 1
    filename <- paste0(model_name, "-", myoutcome, "-", n_vers)
    df_info <- data.frame(file = filename,
                          model_name = model_name,
                          vers = n_vers,
                          outcome = myoutcome,
                          cov = paste(v_cov_mod, collapse = ","),
                          date = format(Sys.Date(), "%Y%m%d"))
    modinfo <- rbind(modinfo, df_info)
    
    # Save model fit
    write.csv(summer.brfss$bym2.model.est, paste0("./gen/model/pred-summer/pred-", filename, ".csv"), row.names = FALSE)
  }
}


# if old model info has the same file name as in new, drop
if(nrow(old_modinfo) > 0){
  old_modinfo <- subset(old_modinfo, !(file %in% modinfo$file))
}
new_modinfo <- rbind(modinfo, old_modinfo)
new_modinfo <- new_modinfo[order(new_modinfo$outcome, new_modinfo$vers),]
write.csv(new_modinfo, paste0("./gen/model/audit/model-info-summer.csv"), row.names = FALSE)
#write.csv(df_modinfo, paste0("./gen/model/audit/model-info-summer.csv"), row.names = FALSE)


