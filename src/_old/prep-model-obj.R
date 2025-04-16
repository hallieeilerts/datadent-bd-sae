################################################################################
#' @description Calculate covariates and outcomes
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(haven)
library(tidyverse)
library(survey)
#' Inputs
source("./src/util.R")
## DHS child recode
dat <- read_dta("./data/BD_2022_DHS_03042025_2114_120781/BDKR81DT/BDKR81FL.DTA")
## District names for clusters
clusters <- read.csv("./gen/prepare-shp/output/cluster-districts.csv")
################################################################################

dat_prep <- fn_prepKR(dat)

# merge on shape file district names to clusters
nrow(dat_prep) # 8784
dat_prep <- merge(dat_prep, clusters, by.x = "v001", by.y = "DHSCLUSTER")
nrow(dat_prep) # 8784

# create covariates
# residence = v025
# literacy = v155

# create outcome variables
dat_var <- fn_gen_nt_ch_micro_vas(dat_prep)
dat_var <- fn_gen_nt_ch_micro_dwm(dat_var)

library(lme4)

# Mixed-effects model (e.g., for estimating child malnutrition rate)
sae_model <- glmer(nt_ch_micro_vas ~ v025 + v155 + (1 | district),
                   data = dat_var, family = binomial)

summary(sae_model)
# (1 | district): Random intercept to account for district-level variation.
# glmer() is used since health outcomes in DHS are often binary (e.g., malnutrition: Yes/No).

library(brms)
library(spdep)  # For spatial adjacency matrix

# Load shapefile of districts (replace with actual file)
library(sf)
districts <- st_read("districts_shapefile.shp")
# Create adjacency matrix
nb <- poly2nb(districts)  # Neighbors list
W <- nb2mat(nb, style = "B", zero.policy = TRUE)  # Convert to adjacency matrix

# Example: Bayesian mixed-effects model with priors
priors <- c(
  prior(normal(0, 1), class = "b"),     # Priors for fixed effects (e.g., age, education)
  prior(normal(0, 5), class = "Intercept"),  # Prior for the intercept
  prior(cauchy(0, 1), class = "sd")     # Prior for the random effect (district-level variation)
)

# Fit the Bayesian model
model <- brm(
  health_outcome ~ age + education + wealth_index + (1 | district),   # Model formula
  data = dhs_data,  # Your dataset
  prior = priors,    # Priors for the model
  family = bernoulli(),  # If health_outcome is binary (e.g., child malnutrition Yes/No)
  chains = 4,        # Number of chains for sampling
  iter = 2000        # Number of iterations per chain
)

# Print model summary
summary(model)

# Plot posterior distributions
plot(model)

# Posterior predictive checks
posterior_predict(model)



