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
library(readxl)
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

# set version of model
# the first version fit will be this plus one (j)
vers_start <- 100
test <- 1
model_name <- "SummerArealBYM2"
model_file <- ""

# subset to included indicators
df_ind <- subset(ind, status == "include")

# adjacency matrix
mat <- getAmat(bangladesh_2, bangladesh_2$ADM2_EN)
# vector of outcomes
v_var <- unique(df_ind$variable)

v_intmod <- subset(old_modinfo, vers == 100)$outcome
v_var <- unique(subset(old_modinfo, !(outcome %in% v_intmod))$outcome)

# empty dataframe for storing model info
modinfo <- data.frame()

# merge outcome variables with covariates
dat <- merge(est, covar, by = c("ADM2_EN", "variable"))

for(i in 1:length(v_var)){
  
  myoutcome <- v_var[i]
  print(myoutcome)
  
  # subset indicator direct estimate
  direct <- subset(dat, variable == myoutcome)
  
  # covariate group for outcome
  v_covar_grp <- subset(ind, variable == myoutcome)$covar_grp
  # covariates for this group
  df_covar_grp <- subset(indcovar, covar_grp == v_covar_grp)
  
  # set appropriate dataset for indicator
  mydhs <- subset(ind, variable == myoutcome)$dhs_dataset
  if(mydhs %in% "ir"){ dhs <- dhs_mth}
  if(mydhs %in% "kr"){ dhs <- dhs_chld}
  if(mydhs %in% "br"){ dhs <- dhs_bth}
  if(mydhs %in% "hr"){ dhs <- dhs_hhd
                       dhs$v001 <- dhs$hv001
                       dhs$v023 <- dhs$hv023 }
  
  # survey design
  design <- dhs %>% as_survey_design(ids = "v001", # psu
                                     strata = "v023", # strata for sampling
                                     weights = "wt",
                                     nest = TRUE)
  
  # for each set of covariates
  for(j in 1:(ncol(df_covar_grp)-1)){
    
    # file name
    vers <- vers_start + j
    file_name <- paste(model_name, myoutcome, vers, test, sep = "-")
    print(file_name)
    
    v_cov_mod <- df_covar_grp[,j+1]
    v_cov_mod <- v_cov_mod[!is.na(v_cov_mod)]
    # for fitting intercept model (also change j loop to only run once)
    #v_cov_mod <- 1
    print(v_cov_mod)
    
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
    df_info <- data.frame(file = file_name,
                          model_name = model_name,
                          vers = vers,
                          test = test,
                          outcome = myoutcome,
                          cov = paste(v_cov_mod, collapse = ","),
                          date = format(Sys.Date(), "%Y%m%d"))
    modinfo <- rbind(modinfo, df_info)
    
    # Save model fit
    write.csv(summer.brfss$bym2.model.est, paste0("./gen/model/pred-summer/pred-", file_name, ".csv"), row.names = FALSE)
  }
}


# if old model info has the same file name as in new, drop
if(nrow(old_modinfo) > 0){
  old_modinfo <- subset(old_modinfo, !(file %in% modinfo$file))
}
new_modinfo <- rbind(modinfo, old_modinfo)
new_modinfo <- new_modinfo[order(new_modinfo$outcome, new_modinfo$vers),]
write.csv(new_modinfo, paste0("./gen/model/audit/model-info-summer.csv"), row.names = FALSE)



