################################################################################
#' @description Generate results file
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(dplyr)
library(tidyr)
#' Inputs
source("./src/util.R")
# indicator info
ind <- read_excel("./data/ind-info.xlsx", sheet = "indicators")
# model info
modinfo <- read.csv("./gen/model/audit/model-info.csv")
# minimum error model for adm1
minerror_adm1_mod <- read.csv("./gen/validation/output/agg-minerror-adm1.csv")
# aggregated predictions for adm1 and adm0
agg_adm1 <- read.csv("./gen/validation/output/agg-error-adm1.csv")
agg_adm0 <- read.csv("./gen/validation/output/agg-error-adm0.csv")
################################################################################

# Note 2025-10-08: 
# produce results for (i) full model (103), (ii) two covariates (102), 
# (iii) combo file that sometimes has 103 and sometimes 102 depending on the indicator group.
# This is the one I use in plotting. 
# to produce i or ii, manually set it in mymodel. otherwise set mymodel as NULL
mymodel <- "103-1" # "102-1", "103-1", NULL
if(!is.null(mymodel)){
  mymodsave <- sub("-.*", "", mymodel)
}else{
  mymodsave <- "ByCovarGrp"
}


# adm2 predictions --------------------------------------------------------

# subset to included indicators
df_ind <- subset(ind, status == "include")

# vector of indicators
v_var <- unique(df_ind$variable)

# indicators coming from kr
# which sometimes have a phantom household included in the direct estimate
# and thus, need to use dirplot (which doesn't)
v_var_ch <- subset(df_ind, dhs_dataset == "kr")$variable

dat <- data.frame()
for(i in 1:length(v_var)){
  
  myoutcome <- v_var[i]
  print(myoutcome)
  
  # # selected model for indicator (one with minimum aggregated adm1 error)
  # mymodel <- subset(minerror_adm1_mod, variable == myoutcome)$model
  # select model by covariate group. these were manually set in collaboration with Maiga after checking RMSE with adm1 estimate.
  if(is.null(mymodel)){
    assignmod <- subset(ind, variable == myoutcome)$model_covar_grp
  }else{
    assignmod <- mymodel
  }
  
  ## VARIANCE SMOOTHING MODEL
  
  # filename for posterior predictions from selected model for this indicator
  filenames <- list.files("./gen/model/pred/")
  filenames <- filenames[grepl("pred", filenames, ignore.case = TRUE)]
  filenames <- filenames[grepl(myoutcome, filenames)]
  filenames <- filenames[grepl(assignmod, filenames)]
  if(myoutcome == "nt_wm_micro_iron"){
    filenames <- filenames[!grepl("nt_wm_micro_iron_any", filenames)]
  }
  
  # load posterior prediction for first model in df
  filename <- filenames[1]
  postpred <- read.csv(paste0("./gen/model/pred/",filename))
  names(postpred)[which(names(postpred) == "post_mean")] <- "r"
  names(postpred)[which(names(postpred) == "post_var")] <- "var"
  names(postpred)[which(names(postpred) == "qt_lb")] <- "ll"
  names(postpred)[which(names(postpred) == "qt_ub")] <- "ul"
  names(postpred)[which(names(postpred) == "obs_un")] <- "un"
  names(postpred)[which(names(postpred) == "obs_wn")] <- "wn"
  names(postpred)[which(names(postpred) == "obs_un_adm1_avg")] <- "un_adm1_avg"
  names(postpred)[which(names(postpred) == "obs_wn_adm1_avg")] <- "wn_adm1_avg"
  
  # use dirplot for direct estimates for indicators coming from child recode 
  # (dirplot is the direct estimate without the phantom household)
  # use dir for other dhs data files (where the direct estimate didn't include a phantom household, and thus didn't need to be calculated separately for plotting)
  if(myoutcome %in% v_var_ch){
    postpred <- postpred %>%
      select(-c(dir, dir_var)) %>%
      rename(dir = dirplot,
             dir_var = dirplot_var)
  }
  
  # se of direct estimate
  postpred$dir_se <- sqrt(postpred$dir_var)
  # se of modeled estimate
  postpred$se <- sqrt(postpred$var)
  
  # Load model info for our model
  myinfo <- subset(modinfo, outcome == myoutcome)[,c("vers", "test", "cov")]
  myinfo$model <- paste(myinfo$vers, myinfo$test, sep = "-")
  
  postpred <- postpred %>%
    mutate(model = assignmod) %>%
    left_join(myinfo, by = "model") %>%
    select(variable, 
           ADM1_EN, ADM2_EN, 
           dir, dir_se, 
           r, se, ll, ul, un, wn, un_adm1_avg, wn_adm1_avg,
           model, cov)
  
  dat <- rbind(postpred, dat)
  
}

# make sure no estimates are outside of intervals
nrow(subset(dat, r > ul | r < ll)) # 0

# add coefficient of variation
dat <- dat %>%
  mutate(cv = se/r*100)

# if a variable is numeric, round
dat <- dat %>%
  mutate(across(where(is.numeric), ~ round(., 5)))

# add extra admin columns and arrange
dat <- dat %>%
  mutate(admin_level = "adm2",
         ADM0_EN = "Bangladesh") %>%
  select(variable, 
         admin_level, ADM0_EN, ADM1_EN, ADM2_EN, 
         dir, dir_se, 
         r, se, ll, ul, cv,
         un, wn, un_adm1_avg, wn_adm1_avg,
         model, cov)

# not necessary to do i think?
# # calculate confidence intervals for direct estimates
# postpred_ourmod$lb <- ifelse(!is.na(postpred_ourmod$qt_lb), postpred_ourmod$qt_lb*100, postpred_ourmod$value - 1.96*postpred_ourmod$se)
# postpred_ourmod$ub <- ifelse(!is.na(postpred_ourmod$qt_ub), postpred_ourmod$qt_ub*100, postpred_ourmod$value + 1.96*postpred_ourmod$se)
# # sometimes the direct confidence intervals will exceed 0 or 100
# postpred_ourmod$lb[postpred_ourmod$lb < 0] <- 0
# postpred_ourmod$ub[postpred_ourmod$ub > 100] <- 100


# aggregated predictions for adm0 and adm1 --------------------------------

# combine aggregated predictions
dat_agg <- rbind(agg_adm0, agg_adm1)

# # limit to model with lowest rmse
# dat_agg <- dat_agg %>%
#   inner_join(minerror_adm1_mod, by = c("variable", "model"))
# limit to chosen model
modkey <- dat %>%
  select(variable, model) %>%
  distinct()
dat_agg <- dat_agg %>%
  inner_join(modkey, by = c("variable", "model"))

# limit to necessary columns and rename
dat_agg <- dat_agg %>%
  mutate(ADM2_EN = NA,
         cv = NA, un_adm1_avg = NA, 
         wn_adm1_avg = NA) %>%
  select(variable, 
         admin_level, ADM0_EN, ADM1_EN, ADM2_EN, 
         dir, se_dir, 
         agg, se_agg, lb_agg, ub_agg, cv,
         obs_un, obs_wn, un_adm1_avg, wn_adm1_avg,
         model, cov) %>%
  rename(dir_se = se_dir,
         r = agg,
         se = se_agg,
         ll = lb_agg,
         ul = ub_agg,
         un = obs_un,
         wn = obs_wn)

# if a variable is numeric, round
dat_agg <- dat_agg %>%
  mutate(across(where(is.numeric), ~ round(., 5)))

# combine aggregated with modeled adm2 --------------------------------------------

dat_res <- rbind(dat, dat_agg)
dat_res <- dat_res[order(dat_res$variable, dat_res$admin_level, dat_res$ADM1_EN, dat_res$ADM2_EN),]

# convert proportions to percentages
dat_res$dir <- dat_res$dir * 100
dat_res$dir_se <- dat_res$dir_se * 100
dat_res$r <- dat_res$r * 100
dat_res$se <- dat_res$se * 100
dat_res$ll <- dat_res$ll * 100
dat_res$ul <- dat_res$ul * 100

# add indicator description
dat_res <- dat_res %>%
  left_join(df_ind %>% select(variable, description), by = "variable") %>%
  relocate(description, .after = variable)

# Tidy --------------------------------------------------------------------

# check for any NA values
colSums(is.na(dat_res))
# no missing
# variable, description, admin_level, ADM0_EN, r, se, ll, ul, un, wn, n_cluster, model, cov
# ADM1_EN: number of missing is number of indicators (currently 31)
# ADM2_EN: number of missing is number of indicators * (1 nat est + 8 adm1 est). 31*9 = 279
# dir: missing happens when un = 0 (currently 16)
# dir_se: missing happens when un = 0 (currently 16)
# un_adm1_avg, wn_adm1_avg: number of missing is number of indicators * (1 nat est + 8 adm1 est). 31*9 = 279

# check that no missing values in main columns of interest
# variable, ADM2_EN, r, se, ll, ul, un, wn, model, cov

# check for any infinite values
sapply(dat_res, function(x) sum(is.infinite(x))) # 0

# tidy
dat_res <- dat_res[order(dat_res$variable, dat_res$admin_level, dat_res$ADM1_EN, dat_res$ADM2_EN),]

# Create codebook ---------------------------------------------------------

df_cb <- data.frame(variable = names(dat_res),
                    definition = NA,
                    notes = NA)

df_cb$definition[df_cb$variable == "variable"] <- "Indicator code"
df_cb$definition[df_cb$variable == "description"] <- "Variable description"
df_cb$definition[df_cb$variable == "admin_level"] <- "Administrative level of estimate (adm0, adm1, adm2)"
df_cb$definition[df_cb$variable == "ADM0_EN"] <- "Administrative level 0 value"
df_cb$definition[df_cb$variable == "ADM1_EN"] <- "Administrative level 1 value"
df_cb$definition[df_cb$variable == "ADM2_EN"] <- "Administrative level 2 value"
df_cb$definition[df_cb$variable == "dir"] <- "Direct estimate of prevalence"
df_cb$definition[df_cb$variable == "dir_se"] <- "Direct estimate of standard error"
df_cb$definition[df_cb$variable == "r"] <- "Modeled prevalence"
df_cb$definition[df_cb$variable == "se"] <- "Modeled standard error"
df_cb$definition[df_cb$variable == "ll"] <- "Lower limit of modeled prevalence"
df_cb$definition[df_cb$variable == "ul"] <- "Upper limit of modeled prevalence"
df_cb$definition[df_cb$variable == "cv"] <- "Coefficient of variation of modeled estimate"
df_cb$definition[df_cb$variable == "un"] <- "Unweighted number of observations for variable in specified admin level"
df_cb$definition[df_cb$variable == "wn"] <- "Weighted number of observations for variable in specified admin level"
df_cb$definition[df_cb$variable == "un_adm1_avg"] <- "Average unweighted number of observations for variable in adm1 region"
df_cb$definition[df_cb$variable == "wn_adm1_avg"] <- "Average weighted number of observations for variable in adm1 region"
df_cb$definition[df_cb$variable == "n_cluster"] <- "Number of clusters in specified admin level"
df_cb$definition[df_cb$variable == "model"] <- "Name of model used to generate modeled estimates (i.e., columns r, se, ll, ul)"
df_cb$definition[df_cb$variable == "cov"] <- "Covariates included in model used to generate modeled estimates (i.e., columns r, se, ll, ul)"

df_cb$notes[df_cb$variable == "dir"] <- "Calculated taking into account survey design (strata, clusters, survey weights)."
df_cb$notes[df_cb$variable == "dir_se"] <- "Calculated taking into account survey design (strata, clusters, survey weights)."
df_cb$notes[df_cb$variable == "se"] <- "Derived from posterior distribution."
df_cb$notes[df_cb$variable == "ll"] <- "Calculated as 2.5th quantile of posterior distribution."
df_cb$notes[df_cb$variable == "ul"] <- "Calculated from 97.5th quantile of posterior distribution."
df_cb$notes[df_cb$variable == "un_adm1_avg"] <- "This variable is only not-missing when admin_level=adm2. Can be used to fill in un that are equal to 0."
df_cb$notes[df_cb$variable == "wn_adm1_avg"] <- "This variable is only not-missing when admin_level=adm2. Can be used to fill in wn that are equal to 0."


# Reshape adm2 wide -------------------------------------------------------

# fill in un and wn that are zero with the adm1 average
dat_res_adm2 <-  dat_res %>%
  filter(admin_level == "adm2") %>%
  mutate(un = ifelse(un == 0, un_adm1_avg, un),
         wn = ifelse(wn == 0, wn_adm1_avg, wn))

# reshape wide
datWide <- dat_res_adm2 %>%
  select(variable, ADM2_EN, r, se, ll, ul, un, wn) %>%
  pivot_wider(
    id_cols = ADM2_EN,
    names_from = variable,
    values_from = c(r, se, ll, ul, un, wn),
    names_glue = "{variable}_{.value}",
    names_vary  = "slowest"   # groups by indicator: ind_r, ind_se, ind_ll, etc. 
  ) 


# Codebook for wide file with variable def --------------------------------------------------

df_cb2 <- df_ind %>% select(variable, description, description_detailed)
df_cb2_aux <- data.frame(variable = c("_r", "_se", "_ll", "_ul"),
                         description = c("modeled prevalence", "standard error derived from posterior distribution", 
                                         "2.5% quantile of posterior", "97.5% quantile of posterior"),
                         description_detailed = rep(NA))
df_cb2 <- rbind(df_cb2, df_cb2_aux)

# Save --------------------------------------------------------------------

write.csv(dat_res, paste0("./gen/results/output/BD-sae-mod", mymodsave,"_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE)
write.csv(df_cb, paste0("./gen/results/output/Codebook_BD-sae_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE)
write.csv(datWide, paste0("./gen/results/output/BD-sae-mod", mymodsave, "-adm2-wide", "_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE)
write.csv(df_cb2, paste0("./gen/results/output/Codebook_BD-sae-adm2-wide_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE)
