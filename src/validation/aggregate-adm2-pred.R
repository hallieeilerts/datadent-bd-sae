################################################################################
#' @description Aggregate adm2 predictions to adm0 and adm1 level
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(dplyr)
library(tidyr)
library(here)
library(cmdstanr)
library(readxl)
#' Inputs
source("./src/util.R")
ind <- read_excel("./data/ind-info.xlsx", sheet = "indicators")
info <- read.csv("./gen/model/audit/model-info.csv")
# direct estimates
direct_adm2 <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
direct_adm1 <- read.csv("./gen/calculate-direct/output/direct-estimates-adm1.csv")
direct_adm0 <- read.csv("./gen/calculate-direct/output/direct-estimates-adm0.csv")
################################################################################

# subset to included indicators
df_ind <- subset(ind, status == "include")

# vector of indicators
v_var <- unique(df_ind$variable)

dat <- data.frame()
for(i in 1:length(v_var)){
  
  myoutcome <- v_var[i]
  print(myoutcome)
  
  # selected model for indicator
  mymodel <- subset(df_ind, variable == myoutcome)$model
  
  # Load model info for our model
  myinfo <- subset(info, outcome == myoutcome)[,c("vers", "test", "cov")]
  myinfo$model <- paste(myinfo$vers, myinfo$test, sep = "-")
  
  # fill in missing weights at adm2 level with adm1 average prior to aggregation
  mywt_adm2 <- direct_adm2 %>%
    filter(variable == myoutcome) %>%
    mutate(obs_un = case_when(
      obs_un == 0 ~ obs_un_adm1_avg,
      TRUE ~ obs_un
    ),
    obs_wn = case_when(
      obs_wn == 0 ~ obs_wn_adm1_avg,
      TRUE ~ obs_wn
    )) %>%
    select(ADM2_EN, ADM1_EN, obs_un, obs_wn)
  
  # subset adm1 and adm0 weights
  mywt_adm1 <- direct_adm1 %>%
    filter(variable == myoutcome) %>%
    select(ADM1_EN, dir, dir_var, obs_un, obs_wn)
  mywt_adm0 <- direct_adm0 %>%
    filter(variable == myoutcome) %>%
    mutate(ADM0_EN = "Bangladesh") %>%
    select(ADM0_EN, dir, dir_var, obs_un, obs_wn)

  ## VARIANCE SMOOTHING MODEL
  
  # model file for this indicator
  filename_modfits <- list.files("./gen/model/fit")
  filename_modfits <- filename_modfits[grepl(".rds", filename_modfits)]
  filename_modfits <- filename_modfits[grepl(myoutcome, filename_modfits)]
  filename_modfits <- filename_modfits[grepl(mymodel, filename_modfits)]
  if(myoutcome == "nt_wm_micro_iron"){
    filename_modfits <- filename_modfits[!grepl("nt_wm_micro_iron_any", filename_modfits)]
  }
  
  # read rds file that contains names of stan csv files
  stan_csv_filenames <- readRDS(here("gen", "model", "fit", paste0(filename_modfits)))
  # read stan csv files as cmdstan object
  stanfit <- as_cmdstan_fit(stan_csv_filenames)
  
  # extract prevalence
  df_p = stanfit$draws(format = "df", variables=c('p'),  inc_warmup = F) |> mutate(chain=as.character(.chain)) 
  colnames(df_p) <- colnames(df_p) %>%
    gsub("\\[", "", .) %>%
    gsub("\\]", "", .) %>%
    gsub(",","_", .)
  df_p_t <- as.data.frame(t(df_p[,1:nrow(mywt_adm2)]))
  names(df_p_t) <- paste0("X", 1:ncol(df_p_t))
  
  # merge on adm2 weights
  df_post <- cbind(mywt_adm2, df_p_t)
  
  # Group by adm1/adm0 and aggregate weighted mean
  df_p_adm1 <- df_post |> as.data.frame() |>
    group_by(ADM1_EN) |>
    summarise(across(all_of(colnames(df_post)[grepl('X',colnames(df_post))]), ~ weighted.mean(., w = obs_wn)))
  df_p_adm0 <- df_post |> as.data.frame() |>
    mutate(ADM0_EN = "Bangladesh") %>%
    group_by(ADM0_EN) %>%
    summarise(across(all_of(colnames(df_post)[grepl('X',colnames(df_post))]), ~ weighted.mean(., w = obs_wn)))

  # compute mean and credible interval across all posterior samples
  alpha = 0.05
  # adm1
  df_p_adm1$post_mean <- df_p_adm1[,2:(nrow(df_p)+1)] |> rowMeans()
  df_p_adm1$post_var <- apply(df_p_adm1[, 2:(nrow(df_p))], 1, var)
  df_quantile =  apply(df_p_adm1[,2:nrow(df_p)], 1 , quantile , probs = c(alpha/2,1-alpha/2) , na.rm = TRUE ) |> t()
  df_p_adm1$qt_lb <- df_quantile[, 1]
  df_p_adm1$qt_ub <- df_quantile[, 2]
  df_p_adm1$length95 <- df_p_adm1$qt_ub - df_p_adm1$qt_lb
  df_p_adm1 <- df_p_adm1 |> select(-colnames(df_post)[grepl('X',colnames(df_post))])
  df_p_adm1$admin_level <- "adm1"
  df_p_adm1$ADM0_EN <- "Bangladesh"
  # merge on adm1 weights
  df_p_adm1 <- df_p_adm1 %>%
    left_join(mywt_adm1, by = "ADM1_EN")
  
  # adm0
  df_p_adm0$post_mean <- df_p_adm0[,2:(nrow(df_p)+1)] |> rowMeans()
  df_p_adm0$post_var <- apply(df_p_adm0[, 2:(nrow(df_p))], 1, var)
  df_quantile =  apply(df_p_adm0[,2:nrow(df_p)], 1 , quantile , probs = c(alpha/2,1-alpha/2) , na.rm = TRUE ) |> t()
  df_p_adm0$qt_lb <- df_quantile[, 1]
  df_p_adm0$qt_ub <- df_quantile[, 2]
  df_p_adm0$length95 <- df_p_adm0$qt_ub - df_p_adm0$qt_lb
  df_p_adm0 <- df_p_adm0 |> select(-colnames(df_post)[grepl('X',colnames(df_post))])
  df_p_adm0$admin_level <- "adm0"
  df_p_adm0$ADM1_EN <- NA
  # merge on adm0 weights
  df_p_adm0 <- df_p_adm0 %>%
    left_join(mywt_adm0, by = "ADM0_EN")
  
  # combine adm1 and adm0
  df_agg <- rbind(df_p_adm0, df_p_adm1)
  df_agg <-  df_agg %>%
    mutate(variable = myoutcome) %>%
    mutate(model = mymodel) %>%
    left_join(myinfo, by = "model") %>%
    select(variable, admin_level, ADM0_EN, ADM1_EN, 
           dir, dir_var,
           post_mean, post_var, qt_lb, qt_ub, 
           obs_un, obs_wn, 
           model, cov)
  
  dat <- rbind(df_agg, dat)
  
}

dat <- dat[order(dat$variable, dat$admin_level, dat$ADM0_EN, dat$ADM1_EN),]

# make sure no estimates are outside of intervals
nrow(subset(dat, post_mean > qt_ub | post_mean < qt_lb)) # 0

# Note:
# for indicator from dhs2014, Dhaka adm1 encompassed Mymensingh, which was broken out for later surveys
# however, i took cluster locations and matched them up with most recent adm1 and adm2 shape files
# so for me, all my estimates (even for data from old surveys) are for the present shape boundaries
# this includes both the direct estimates i calculated as well as those that were predicted with the model
# for the direct estimate for the indicator from 2014, the survey design was set through v023 (which wouldn't distinguish between dhaka and mymensingh),
# however the estimates would still be output for both
subset(df_ind, dhs_svyyr == 2014)$variable
# so in any case... my direct adm1 estimates for this indicator won't match what's in the dhs report for dhaka and mymensingh
# but i think it is ok to compare my aggregated modeled adm2 estimates and directly calculated adm1 estimates

# Save --------------------------------------------------------------------

write.csv(dat, file = "./gen/validation/output/pred-agg.csv", row.names = FALSE)
