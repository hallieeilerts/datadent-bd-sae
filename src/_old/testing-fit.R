library(brms)
library(spdep)  # For spatial matrices
library(tidyverse)


covar <- readRDS("./gen/prepare-model-input/temp/covar-hhd.rds")
outcome <- readRDS("./gen/prepare-model-input/temp/outcomes-child.rds")
spatial_cor_matrix <- readRDS("./gen/prepare-shp/output/adjacency_matrix.rds")
AD2 <- readOGR(dsn="./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
AD2 <- st_as_sf(AD2)

dhs_data <- outcome %>%
  left_join(covar, by = c("hhid", "b16" = "hvidx")) %>%
  mutate(recnr = 1:n())

# Ensure district is a factor
dhs_data$district <- as.factor(dhs_data$district)


# Define the model formula
formula <- bf(nt_ch_micro_vas ~ ph_wealth_quint + ph_hhhead_sex + 
                (1|district) +  # Random effect for districts
                s(district, bs = "re", sp = spatial_cor_matrix))  # Spatial smoothing for districts

# Fit the model using brms
fit <- brm(formula, 
           family = bernoulli(),
           data = dhs_data, 
           weights = wt,
           prior = c(
              prior(normal(0, 1), class = "b"),  # Default normal prior for all fixed effects
              prior(normal(0, 1), class = "sd")  # Prior for random effects (districts)
              #prior(student_t(3, 0, 2), class = "b", coef = "ph_hhhead_sex")  # Different prior for covariate2
            ), 
           chains = 2, 
           cores = 4,
           iter = 2000, 
           warmup = 1000,
           thin = 1)

# Check model results
summary(fit)

# Visualize the posterior distributions of the fixed effects
plot(fit)

# Make new data for prediction which is at the district level. Use mean values of covariates.

# Make predictions for individual-level data
individual_predictions <- predict(fit, newdata = dhs_data, type = "response", sample_new_levels = NULL)

# Combine predictions with individual data
dhs_data$predicted_vitamin_A <- as.data.frame(individual_predictions)$Estimate

# Aggregate by district to get district-level predicted probability
district_predictions <- dhs_data %>%
  group_by(district) %>%
  summarise(mean_predicted_vitamin_A = mean(predicted_vitamin_A, na.rm = TRUE))

# Plot district-level estimates
AD2_merged <- left_join(AD2, district_predictions, by = c("ADM2_EN" = "district"))
ggplot() +
  geom_sf(data = AD2_merged, aes(fill = mean_predicted_vitamin_A), color = "black") +
  scale_fill_viridis_c(option = "magma", na.value = "gray80") + 
  theme_bw() +
  labs()

# Generate posterior predictive samples for individual-level predictions
posterior_preds <- posterior_predict(fit)
# Create a new column to hold the predicted probabilities for each individual
test <- subset(dhs_data, !(is.na(nt_ch_micro_vas) | is.na(ph_wealth_quint) | is.na(ph_hhhead_sex)))
test$predictions <- apply(posterior_preds, 2, mean)

# Summarize individual-level predictions at the district level (e.g., mean probability of supplementation)
district_preds <- test %>%
  group_by(district) %>%
  summarize(
    predicted_prob = mean(predictions),
    lower = quantile(predictions, 0.025),  # 2.5% quantile (lower bound)
    upper = quantile(predictions, 0.975)   # 97.5% quantile (upper bound)
  )

# Plot district-level estimates
AD2_merged <- left_join(AD2, district_preds, by = c("ADM2_EN" = "district"))
ggplot() +
  geom_sf(data = AD2_merged, aes(fill = predicted_prob), color = "black") +
  scale_fill_viridis_c(option = "magma", na.value = "gray80") + 
  theme_bw() +
  labs()
# Plot uncertainty
ggplot(district_preds, aes(x = district, y = predicted_prob)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Error bars
  geom_point(aes(color=predicted_prob), size = 3) +  # Plot estimate points
  labs(title = "", x = "", y = "") +
  scale_color_viridis_c(option = "magma", na.value = "gray80",guide = "none") + 
  theme_bw() +
  coord_flip()

# separate script that generates plot
# plot uncertainty bars on top of direct estimates
# prepare more covariates for inclusion
# think about priors
