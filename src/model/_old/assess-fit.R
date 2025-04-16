################################################################################
#' @description Predict districts
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(brms)
library(ggpubr)
library(bayesplot)
library(dplyr)
#' Inputs
source("./src/util.R")
# all model outputs
dat_filename <- list.files("./gen/model/output/")
dat_filename <- dat_filename[grepl("ModelOutput", dat_filename, ignore.case = TRUE)]
# spatial correlation
spatial_cor_matrix <- readRDS("./gen/prepare-shp/output/adjacency_matrix.rds")
################################################################################

# Diagnostic plots --------------------------------------------------------

# for plots, can limit to file names that don't already have plots
new_files <- dat_filename
#new_files <- dat_filename[!(dat_filename %in% list.files("./gen/model/output/"))]

for(i in 1:length(new_files)){
  load(paste0("./gen/model/output/",new_files)[i])
 
  # 0. Estimated coefficients
  posterior_samples <- as.array(fit)
  # FE
  pars_to_plot <- grep("^b_", fit$fit@sim$fnames_oi, value = TRUE)
  p_coef <- mcmc_areas(posterior_samples, pars =  pars_to_plot) + theme_bw()
  p_multiple_chains <- mcmc_trace(posterior_samples, pars = pars_to_plot) + theme_bw()
  ggsave(paste0("./gen/model/audit/", gsub(".RData", "", new_files[i]), "_chains.png"), p_multiple_chains, width = 9, height = 6, dpi = 300)
  ggsave(paste0("./gen/model/audit/", gsub(".RData", "", new_files[i]), "_coef.png"), p_coef, width = 9, height = 6, dpi = 300)
  # RE
  pars_to_plot <- grep("^r_", fit$fit@sim$fnames_oi, value = TRUE)
  if(length(pars_to_plot) >  0 ){
    p_coef <- mcmc_areas(posterior_samples, pars =  pars_to_plot) + theme_bw()
    p_multiple_chains <- mcmc_trace(posterior_samples, pars = pars_to_plot) + theme_bw()
    ggsave(paste0("./gen/model/audit/", gsub(".RData", "", new_files[i]), "_chains-re.png"), 
           p_multiple_chains, width = 12, height = 9, dpi = 300)
    ggsave(paste0("./gen/model/audit/", gsub(".RData", "", new_files[i]), "_coef-re.png"), 
           p_coef, width = 9, height = 9, dpi = 300)
  }

  # A. Density Overlay Plot (Default)
  p1 <- pp_check(fit, type = "dens_overlay")  + ggtitle("Density Overlay")
  # Good priors: Posterior predictive density (blue) should roughly align with the observed data (black)
  # Overly strong priors: If the blue density is much narrower than the observed data, your prior may be too restrictive.
  # Overly weak priors: If the blue density is much wider than observed, your prior might not be helping with regularization.
  
  # B. Histogram Overlay
  p2 <- pp_check(fit, type = "hist") + ggtitle("Histogram Overlay") +
    theme(axis.text = element_text(size = 8), axis.title = element_text(size = 10))
  # Look for whether the simulated data follows the observed histogram.
  # If it deviates too much, your prior might be forcing the model into an unrealistic space.

  # C. Error Histogram (error_hist)
  # This checks the difference between observed and predicted proportions for each simulation.
  # If the errors are consistently large, your model might be miscalibrated.
  p3 <- pp_check(fit, type = "error_hist")   + ggtitle("Error Histogram")

  # D. Scatterplot of Predicted vs. Observed Means
  p4 <- pp_check(fit, type = "stat", stat = "mean")  + ggtitle("Pred v. Obs Means")
  # If the means of the predicted distributions are far from the observed mean, your prior could be affecting the central tendency.

  # E. Proportion of Successes (stat_freqpoly)
  # This checks whether the proportion of 1s (successes) in the observed data matches the predicted values.
  # If the observed mean is outside the distribution of simulated means, the model may be misfitting.
  #  Works well for checking class imbalance.
  p5 <- pp_check(fit, type = "stat_freqpoly", stat = "mean")  + ggtitle("Prop Successes")

  # F. Binned residuals
  p6 <- pp_check(fit, type = "error_binned") + ggtitle("Binned residuals")
  # If residuals show a strong trend, the model may be systematically mispredicting.
  
  # Arrange plots and save
  png(paste0("./gen/model/audit/", gsub(".RData", "", new_files[i]), "_posterior.png"), 
      width = 5000, height = 2000, res = 300)
  dummy_plot <- ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)
  print(dummy_plot)
  dev.off() 
  
}

# When to Change the Prior
# If the model predictions are much more concentrated than the observed data → Your prior is too strong (e.g., Normal(0, 0.5) might be too narrow).
# If the predictions are too spread out or unrealistic → Your prior is too weak (e.g., Normal(0, 10) might allow extreme values).
# If the predicted values are systematically biased (always higher or lower than observed) → Prior on the intercept might be inappropriate.


# Model fit ---------------------------------------------------------------

all_fit <- data.frame()

for(i in 1:length(dat_filename)){
  
  load(paste0("./gen/model/output/",dat_filename)[i])
  
  df_fit <- summary(fit)
  df_fixed <- df_fit$fixed
  df_random <- df_fit$random$district
  df_sds <- df_fit$splines
  df_fit <- rbind(df_fixed, df_random, df_sds)
  df_fit$coef <- row.names(df_fit)
  row.names(df_fit) <- NULL
  df_fit$file <- gsub(".RData", "", dat_filename[i])
  df_fit$vers <- vers
  df_fit$test <- test
  df_fit$outcome <- outcome_name
  df_fit <- df_fit %>%
    dplyr::select(file, vers, test, outcome, coef, everything())
  
  all_fit <- rbind(all_fit, df_fit)

}

write.csv(all_fit, paste0("./gen/model/audit/model-fit.csv"), row.names = FALSE)


# Model score ---------------------------------------------------------------

all_scores <- data.frame()
all_scores_wide <- data.frame()

for(i in 1:length(dat_filename)){
  
  load(paste0("./gen/model/output/",dat_filename)[i])
  
  df_loo <- loo(fit)
  df_loo <- as.data.frame(df_loo$estimates)
  
  df_waic <- waic(fit)
  df_waic <- as.data.frame(df_waic$estimates)

  df_score <- rbind(df_loo, df_waic)
  df_score$metric <- row.names(df_score)
  row.names(df_score) <- NULL
  df_score$file <- gsub(".RData", "", dat_filename[i])
  df_score$outcome <- outcome_name
  
  df_score <- df_score %>%
    dplyr::select(file, outcome, everything())
  
  df_score_wide <- df_score %>%
    pivot_wider(
      names_from = metric,
      values_from = c(Estimate, SE)
    )
  
  all_scores <- rbind(all_scores, df_score)
  all_scores_wide <- bind_rows(all_scores_wide, df_score_wide)
  # lower values better
}

write.csv(all_scores, paste0("./gen/model/audit/diagnostics.csv"), row.names = FALSE)
write.csv(all_scores_wide, paste0("./gen/model/audit/diagnostics-wide.csv"), row.names = FALSE)


# loo compare -------------------------------------------------------------

dat_filename1 <- dat_filename[grepl("ModelOutputMother-Iron_004-Test1_20250331.RData",dat_filename)]
load(paste0("./gen/model/output/", dat_filename1))
fit1 <- fit
dat_filename1 <- dat_filename[grepl("ModelOutputMother-Iron_008-Test1_20250331.RData",dat_filename)]
load(paste0("./gen/model/output/", dat_filename1))
fit2 <- fit
dat_filename1 <- dat_filename[grepl("ModelOutputMother-Iron_011-Test1_20250331.RData",dat_filename)]
load(paste0("./gen/model/output/",dat_filename1))
fit3 <- fit
dat_filename1 <- dat_filename[grepl("ModelOutputMother-Iron_012-Test1_20250403.RData",dat_filename)]
load(paste0("./gen/model/output/",dat_filename1))
fit4 <- fit


loo1 <- loo(fit1)
loo2 <- loo(fit2)
loo3 <- loo(fit3)
loo4 <- loo(fit4)

# model in first row has largest Expected Log Predictive Density (ELPD)
# a larger ELPD indicates a model with better out-of-sample predictive fit
loo_compare(loo1, loo2, loo3, loo4)

#A general guideline is that if the absolute value of elpd_diff is less than 4, the models have similar predictive performance. 
# If elpd_diff is greater than 4, comparing this difference to its standard error helps determine the robustness of the difference.
# If elpd_diff is more than twice the se_diff, the difference is considered statistically significant 

#       elpd_diff se_diff
# fit4   0.0       0.0  
# fit3 -43.1      10.3  
# fit1 -51.2      11.3  
# fit2 -51.4      11.3

# k-fold ------------------------------------------------------------------


load(paste0("./gen/model/output/",dat_filename)[17])

library(future)
plan(multisession)
# For small area estimation models, Leave-One-Group-Out Cross-Validation (LOGO-CV) can be particularly informative. In LOGO-CV, the model is iteratively trained on all but one group and tested on the omitted group. This method assesses how well the model generalizes to entirely new groups.

plan(multisession, workers = 4)

# weighted, district-level random effects
# "./gen/model/output/ModelOutputMother-Iron_004-Test1_20250331.RData"
# weighted, district-level random effects
# fitting without spatial adjacency matrix
logo_cv_results <- kfold(fit, K = length(unique(fit$data$district)), group = "district")
# leave one out group validation doesn't work with spatial correlation matrix
saveRDS(logo_cv_results, "./gen/model/temp/logo-iron-004-test1.rds")

# weighted, district-level RE, residence
