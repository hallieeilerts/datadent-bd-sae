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

for(i in 1:length(dat_filename)){
  
  load(paste0("./gen/model/output/",dat_filename)[i])
 
  # 0. Estimated coefficients
  posterior_samples <- as.array(fit)
  pars_to_plot <- grep("^b_", fit$fit@sim$fnames_oi, value = TRUE)
  p_coef <- mcmc_areas(posterior_samples, pars =  pars_to_plot) + theme_bw()
  p_multiple_chains <- mcmc_trace(posterior_samples, pars = pars_to_plot) + theme_bw()
  ggsave(paste0("./gen/model/audit/", gsub(".RData", "", dat_filename[i]), "_chains.png"), p_multiple_chains, width = 9, height = 6, dpi = 300)
  ggsave(paste0("./gen/model/audit/", gsub(".RData", "", dat_filename[i]), "_coef.png"), p_coef, width = 9, height = 6, dpi = 300)
  
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
  png(paste0("./gen/model/audit/", gsub(".RData", "", dat_filename[i]), "_posterior.png"), 
      width = 5000, height = 2000, res = 300)
  ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)
  dev.off() 
  
}

# When to Change the Prior
# If the model predictions are much more concentrated than the observed data → Your prior is too strong (e.g., Normal(0, 0.5) might be too narrow).
# If the predictions are too spread out or unrealistic → Your prior is too weak (e.g., Normal(0, 10) might allow extreme values).
# If the predicted values are systematically biased (always higher or lower than observed) → Prior on the intercept might be inappropriate.


# Extract prior info ------------------------------------------------------

all_priors <- data.frame()
all_priors_wide <- data.frame()

for(i in 1:length(dat_filename)){
  
  load(paste0("./gen/model/output/",dat_filename)[i])
  
  priorobj <- fit$prior
  df_prior <- data.frame(prior = priorobj$prior, 
                         coef = priorobj$coef,
                         class = priorobj$class )
  df_prior[df_prior == ""] <- NA
  df_prior <- df_prior %>% fill(prior, .direction = "down")
  df_prior <- subset(df_prior, !is.na(coef))
  df_prior$file <- gsub(".RData", "", dat_filename[i])
  df_prior$outcome <- outcome_name
  df_prior <- df_prior %>%
    dplyr::select(file, outcome, everything())
  
  df_prior_wide <- df_prior %>%
    select(file, outcome, prior, coef) %>%
    pivot_wider(
      names_from = coef,
      values_from = prior
    )
  
  all_priors <- rbind(all_priors, df_prior)
  all_priors_wide <- bind_rows(all_priors_wide, df_prior_wide)
}

write.csv(all_priors, paste0("./gen/model/audit/model-info.csv"), row.names = FALSE)
write.csv(all_priors_wide, paste0("./gen/model/audit/model-info-wide.csv"), row.names = FALSE)

# Fit score ---------------------------------------------------------------

all_scores <- data.frame()

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
  
  all_scores <- rbind(all_scores, df_score)
  
}


write.csv(all_scores, paste0("./gen/model/audit/diagnostics.csv"), row.names = FALSE)


