################################################################################
#' @description Estimate Mother-Iron
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
model <- "Mother-Iron"
outcome <- "nt_wm_micro_iron"

# set a random seed for reproducibility
set.seed(123)  

# model parameters
niter <- 10000   # Number of iterations
burn <- 2000    # Burn-in
thin <- 1       # Thinning factor (keep everything)
nchas <- 4      # Number of chains

# Model with no covariates ------------------------------------------------

# set model version numbers
vers <- "013"
test <- "Test1"
# define priors
priors <- c(
  prior(cauchy(0, 2), class = "sds") # Prior for sd of spatial random effects
)
priors <- NULL

# limit to columns of interest
dat <- dhs_data[,c(outcome, "district","wt")]
# Ensure district is a factor
dat$district <- as.factor(dat$district)
dat <- na.omit(dat) 

# model formula
#formula <- bf(paste0(outcome, " ~ (1 | district)")) # this would be district-level RE
formula <- bf(paste0(outcome, " | weights(wt) ~ s(district, bs = 're', sp = spatial_cor_matrix)"))

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
vers <- "012"
test <- "Test1"
# define priors
priors <- c(
  prior(normal(0, 1), class = "b"), 
  prior(normal(0, 1), class = "sd"), # Prior for sd of random intercepts per district
  prior(cauchy(0, 2), class = "sds") # Prior for sd of spatial random effects
)
# set covariates
#vcov <- c("mother_edu")
#vcov <- c("wealth_index")
#vcov <- c("residence")
vcov <- c("residence", "mother_edu")

# limit to columns of interest
dat <- dhs_data[,c(outcome, vcov, "district","wt")]
# Ensure district is a factor
dat$district <- as.factor(dat$district)
dat <- na.omit(dat) 

# Create fixed-effects part of formula
outcome_weights <- paste0(outcome, " | weights(wt)")
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

model_name <- model
outcome_name <- outcome
# Save
fileName <- paste0('ModelOutput', model_name ,"_", vers, '-', test,"_",format(Sys.Date(), "%Y%m%d"))
save(fit, formula, dat, priors, model_name, outcome_name, vcov, vers, test, Time, fileName,
     file = paste0('./gen/model/output/', fileName, '.RData'))

