################################################################################
#' @description Estimate Child-VAS
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(brms)
library(spdep)  # For spatial matrices
#' Inputs
source("./src/util.R")
dhs_data <- read.csv("./gen/prepare-dhs/output/dat-child.csv")
spatial_cor_matrix <- readRDS("./gen/prepare-shp/output/adjacency_matrix.rds")
################################################################################

# model info
model <- "Child-VAS"
outcome <- "nt_ch_micro_vas"

# set a random seed for reproducibility
set.seed(123)  

# model parameters
niter <- 2000   # Number of iterations
burn <- 1000    # Burn-in
thin <- 1       # Thinning factor (keep everything)
nchas <- 2      # Number of chains

# Model with no covariates ------------------------------------------------

# set model version numbers
vers <- "001"
test <- "Test2"
# define priors
priors <- c(
  prior(normal(0, 1), class = "sd"), # Prior for sd of random intercepts per district
  prior(cauchy(0, 2), class = "sds") # Prior for sd of spatial random effects
)


# limit to columns of interest
dat <- dhs_data[,c(outcome, "district","wt")]
# Ensure district is a factor
dat$district <- as.factor(dat$district)
dat <- na.omit(dat) 

# model formula
formula <- bf(paste0(outcome, " | weights(wt) ~ (1 | district) + s(district, bs = 're', sp = spatial_cor_matrix)"))

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

model_name <- model 
outcome_name <- outcome
# Save
fileName <- paste0('ModelOutput', model_name ,"_", vers, '-', test,"_",format(Sys.Date(), "%Y%m%d"))
save(fit, formula, dat, priors, model_name, outcome_name, vcov, vers, test, Time, fileName,
     file = paste0('./gen/model/output/', fileName, '.RData'))


# Model with covariates ---------------------------------------------------

# set model version numbers
vers <- "002"
test <- "Test2"
# define priors
priors <- c(
  prior(normal(0, 1), class = "b"),  # Prior for fixed effects
  prior(normal(0, 1), class = "sd"), # Prior for sd of random intercepts per district
  prior(cauchy(0, 2), class = "sds") # Prior for sd of spatial random effects
)
# set covariates
vcov <- c("residence", "hhd_under5", "hhd_head_sex", "hhd_head_age",
          "mother_edu", "wealth_index")

# limit to columns of interest
dat <- dhs_data[,c(outcome, vcov, "district","wt")]
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

