
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)
library(here)
library(cmdstanr)

# choose indicator
# "nt_ch_micro_vas"  "nt_ch_micro_dwm"  "nt_ebf" "nt_wm_micro_iron" "rh_anc_4vs" "rh_anc_1tri" 
outcome <- "nt_ch_micro_vas"

# choose model
vers <- "050"
test <- "Test1"
model_name <- "ArealBYM2"
file_name <- paste(model_name, outcome, vers, test, sep = "-")

# read rds file that contains names of stan csv files
stan_csv_filenames <- readRDS(here("gen", "model", "fit", paste0("csv_files-fit-", file_name, ".rds")))

# read stan csv files as cmdstan object
stanfit <- as_cmdstan_fit(stan_csv_filenames)

stanfit$draws()

# examine fit summary

#View(stanfit$summary())
df_sum <- stanfit$summary()
df_sum$param <- sub("([\\[(].*)", "", df_sum$variable)
df_sum$district <- str_extract(df_sum$variable, "(?<=\\[)[^\\]]*")

unique(df_sum$param)
# plot parameters with one value per district
# "u1" "u2"  "tau" "u" "p" "v" "scaled_vhat"
dist_param <- "u1"
df_sum %>%
  filter(param == dist_param) %>%
  ggplot() +
  geom_point(aes(x=fct_reorder(district, ess_bulk), y = ess_bulk)) +
  coord_flip()
df_sum %>%
  filter(param == dist_param) %>%
  ggplot() +
  geom_point(aes(x=fct_reorder(district, rhat), y = rhat)) +
  coord_flip()
quantile(subset(df_sum, param == "u1")$ess_bulk)


# look at single param
# "sigma_u" "sigma_tau" "gamma0" "gamma1" "gamma2"
single_param <- "rho"
subset(df_sum, param == single_param)

# look at fixed effects
modinfo <- read.csv("./gen/model/audit/model-info.csv")
shortvers <- substr(vers, 2, 3)
df_modinfo <- subset(modinfo, vers == shortvers & test == test)
df_modinfo <- df_modinfo[df_modinfo$outcome == outcome,]
df_modinfo$cov
dist_param <- "beta"
df_sum %>%
  filter(param == dist_param)
