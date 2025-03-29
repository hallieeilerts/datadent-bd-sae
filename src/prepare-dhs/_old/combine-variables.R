covar <- readRDS("./gen/prepare-model-input/temp/covar-hhd.rds")
outcome <- readRDS("./gen/prepare-model-input/temp/outcomes-child.rds")
W <- readRDS("./gen/prepare-shp/output/adjacency_matrix.rds")
AD2 <- readOGR(dsn="./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
AD2 <- st_as_sf(AD2)

dhs_data <- outcome %>%
  left_join(covar, by = c("hhid", "b16" = "hvidx")) %>%
  mutate(recnr = 1:n())

library(brms)
library(spdep)  # For spatial adjacency matrix
library(ggplot2)
library(rstan) # for traceplot

# Specify the Bayesian model using brms
model <- brm(
  formula = nt_ch_micro_vas | trials(recnr) ~ ph_wealth_quint + hhsize + (1 | district),  # Random intercept for clusters (v001 is the cluster ID)
  data = dhs_data,
  family = binomial(),  # For a binary outcome (e.g., child mortality)
  prior = c(
    prior(normal(0, 10), class = "b"),  # Priors for fixed effects
    prior(normal(0, 5), class = "Intercept"),  # Prior for intercept
    prior(exponential(1), class = "sd")  # Prior for random intercept variance (cluster-level)
  ),
  chains = 4,  # Number of MCMC chains
  iter = 2000,  # Number of iterations
  warmup = 1000,  # Number of warmup iterations
  thin = 1,  # Thinning of MCMC chains
  control = list(adapt_delta = 0.95)  # Control settings for MCMC
)


# Create a neighbors list (based on spatial data)
neighbors_list <- poly2nb(spatial_data)
# Convert neighbors list into a spatial structure matrix
neighbors_matrix <- nb2mat(neighbors_list, style = "B", zero.policy = TRUE)
# Specify the spatial structure in the brms model
model_spatial <- brm(
  formula = nt_ch_micro_vas ~ ph_wealth_quint + (1 | district) + spatial(reformulate("district", response = "y")),
  data = dhs_data,
  family = binomial(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(normal(0, 5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  thin = 1,
  control = list(adapt_delta = 0.95)
)




# After fitting the model, it is crucial to check diagnostics to ensure convergence and the quality of the estimates.
traceplot(model)
#R-hat values should be close to 1 for each parameter (ideally between 0.95 and 1.05).
#Effective sample size (neff) should be sufficiently large (usually > 1000).

# post-modeling analysis

# Extract posterior samples
posterior_samples <- as.data.frame(model)

# Calculate cluster-level estimates (e.g., mean mortality by cluster)
# Example: Using intercept for mortality estimate
cluster_estimates <- posterior_samples %>%
  pivot_longer(cols = starts_with("r_district"),  # specify the columns to pivot
               names_to = "district",                   # Name for the new variable
               values_to = "value")  %>%
  group_by(district) %>%
  summarize(value = mean(value))
cluster_estimates$district <- gsub("r_district\\[", "", cluster_estimates$district)
cluster_estimates$district <- gsub(",Intercept\\]", "", cluster_estimates$district)
names(cluster_estimates)[which(names(cluster_estimates) == "district")] <- "ADM2_EN"





# Plot cluster-level estimates
AD2_merged <- left_join(AD2, cluster_estimates, by = "ADM2_EN")
# create map
ggplot() +
  geom_sf(data = AD2_merged, aes(fill = value), color = "black") +
  scale_fill_viridis_c(option = "magma", na.value = "gray80") + 
  theme_bw() +
  labs()





