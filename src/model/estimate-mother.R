################################################################################
#' @description Estimate mother-level outcomes
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(brms)
library(spdep)  # For spatial matrices
#' Inputs
source("./src/util.R")
dhs_data <- read.csv("./gen/prepare-dhs/output/dat-mother.csv")
spatial_cor_matrix <- readRDS("./gen/prepare-shp/output/adjacency_matrix.rds")
################################################################################

# model info
model <- c("Mother-Iron", "Mother-ANC4", "Mother-ANC1tri")
outcome <- c("nt_wm_micro_iron", "rh_anc_4vs", "rh_anc_1tri")
vcov <- c("residence",
          "hhd_under5", "hhd_head_sex", "hhd_head_age",
          "mother_edu",
          "wealth_index")
vers <- "002"    # covariates
test <- "Test1"  # weights or hyperparameter changes

# model parameters
niter <- 2000   # Number of iterations
burn <- 1000    # Burn-in (keep everything)
thin <- 1       # Thinning factor (save 1 every 20) (increase to 20 later)
nchas <- 2      # Number of chains

# define priors
priors <- c(
  prior(normal(0, 1), class = "b"),  # Default normal prior for all fixed effects
  prior(normal(0, 1), class = "sd")  # Prior for random effects (districts)
)

for(i in 1:length(model)){
  
  # limit to columns of interest
  dat <- dhs_data[,c(outcome[i], vcov, "district","wt")]
  # Ensure district is a factor
  dat$district <- as.factor(dat$district)
  dat <- na.omit(dat) 
  
  # Create fixed-effects part of formula
  outcome_weights <- paste0(outcome[i], " | weights(wt)")
  fixed_formula <- reformulate(termlabels = vcov,
                               response = outcome_weights)
  # Convert to a character and append random effect for districts and spatial smoothing for districts
  full_formula_str <- paste0(paste(trimws(deparse(fixed_formula)), collapse = ""),
                             " + (1 | district) + s(district, bs = 're', sp = spatial_cor_matrix)")
  # Convert back to formula and use in bf()
  formula <- bf(as.formula(full_formula_str))
  
  # formula for empty model with no covariates
  #formula <- bf(paste0(outcome[i], " | weights(wt) ~ (1 | district) + s(district, bs = 're', sp = spatial_cor_matrix)"))
  # comment above formulas
  # also comment out beta priors in prior vector
  
  # Fit model
  Start <- Sys.time()
  fit <- brm(formula, 
             family = bernoulli(),
             data = dat, 
             prior = priors, 
             chains = nchas, 
             iter = niter, 
             warmup = burn,
             thin = thin)
  End <- Sys.time()
  Time <- End - Start
  
  model_name <- model[i]  
  outcome_name <- outcome[i] 
  # Save
  fileName <- paste0('ModelOutput', model_name ,"_", vers, '-', test,"_",format(Sys.Date(), "%Y%m%d"))
  save(fit, formula, dat, priors, model_name, outcome_name, vcov, vers, test, Time, fileName,
       file = paste0('./gen/model/output/', fileName, '.RData'))
}

