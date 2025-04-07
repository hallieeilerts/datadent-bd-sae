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
library(tidybayes)
#' Inputs
source("./src/util.R")
# all model outputs
dat_filename <- list.files("./gen/model/output/")
dat_filename <- dat_filename[grepl("ModelOutput", dat_filename, ignore.case = TRUE)]
# spatial correlation
spatial_cor_matrix <- readRDS("./gen/prepare-shp/output/adjacency_matrix.rds")
################################################################################

all_models <- data.frame()
all_models_wide <- data.frame()

for(i in 1:length(dat_filename)){
  
  load(paste0("./gen/model/output/",dat_filename)[i])
  
  priorobj <- fit$prior
  df_mod <- data.frame(prior = priorobj$prior, 
                       coef = priorobj$coef,
                       class = priorobj$class )
  df_mod[df_mod == ""] <- NA
  df_mod$coef[df_mod$class == "Intercept"] <- "Intercept"
  df_mod <- df_mod %>% fill(prior, .direction = "down")
  df_mod <- subset(df_mod, !is.na(coef))
  df_mod$coef[df_mod$class == "sd"] <- "re-intercepts"
  df_mod$file <- gsub(".RData", "", dat_filename[i])
  df_mod$vers <- vers
  df_mod$test <- test
  df_mod$outcome <- outcome_name
  df_mod$formula <- as.character(fit$formula)[1]
  df_mod$weighted <- grepl("weights", df_mod$formula)
  df_mod$coef2 <- df_mod$coef
  df_mod$coef2[df_mod$class == "sds"] <- "ssRE"
  df_mod$coef2 <- ifelse(df_mod$class == "b", paste0("b_", df_mod$coef2), df_mod$coef2)
  df_mod$chains <- length(fit$fit@stan_args)
  df_mod$iter <- fit$fit@stan_args[[2]]$iter
  df_mod$burnin <- fit$fit@stan_args[[2]]$warmup
  df_mod$thin <- fit$fit@stan_args[[2]]$thin
  
  df_mod <- df_mod %>%
    dplyr::select(file, outcome, formula, weighted, chains, iter, burnin, thin, everything())
  
  df_mod_wide <- df_mod %>%
    dplyr::select(file, vers, test, outcome, weighted, chains, iter, burnin, thin, prior, coef2) %>%
    pivot_wider(
      names_from = coef2,
      values_from = prior
    )
  
  all_models <- rbind(all_models, df_mod)
  all_models_wide <- bind_rows(all_models_wide, df_mod_wide)
}

write.csv(all_models, paste0("./gen/model/audit/model-info.csv"), row.names = FALSE)
write.csv(all_models_wide, paste0("./gen/model/audit/model-info-wide.csv"), row.names = FALSE)



# Better plots? -----------------------------------------------------------




get_variables(fit)
fit %>%
  spread_draws(r_district[district,term]) %>%
  head(10)
fit %>%
  spread_draws(b_Intercept) %>%
  head(10)
fit %>%
  spread_draws(b_Intercept, sd) %>%
  head(10)

fit %>%
  spread_draws(b_Intercept, r_district[district,]) %>%
  mutate(condition_mean = b_Intercept + r_district) %>%
  ggplot(aes(y = district, x = condition_mean)) +
  stat_halfeye()


# trace plots
library(ggmcmc)
modeltranformed <- ggs(fit) # the ggs function transforms the BRMS output into a longformat tibble, that we can use to make different types of plots.
ggplot(filter(modeltranformed, Parameter %in% c("b_Intercept")),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line()+
  facet_grid(Parameter ~ .,
             scale     = 'free_y',
             switch    = 'y')+
  labs(title = "Trace plots",
       col   = "Chains") +
  theme_minimal()

# trace plots
stanplot(fit, type = "trace")

# convergence plot
modelposterior <- as.mcmc(fit) # with the as.mcmc() command we can use all the CODA package convergence statistics and plotting options
library(coda)
# You should look at the Upper CI/Upper limit, which are all should be close to 1. If they aren’t close to 1, you should use more iterations.
gelman.diag(modelposterior[, 1:4])

# autocorrelation plot
stanplot(model, pars = 1:4, type = "acf")
