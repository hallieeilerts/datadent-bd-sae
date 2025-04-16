################################################################################
#' @description Run model for each indicator
#' @return Model fit
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(sf)
library(dplyr)
library(tidyr)
library(here)
library(cmdstanr)
library(wesanderson)
library(ggplot2)
#' Inputs
source("./src/util.R")
# Direct estimates and variance
est <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
# Bangladesh district boundaries
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
# Adjacency matrix
prep <- readRDS("./gen/prepare-shp/output/adjacency_matrix.rds")
# model info from previous models
old_modinfo <- read.csv("./gen/model/audit/model-info.csv")
################################################################################

# set model
vers <- "001"
test <- "Test2"
model_name <- "areal_level_BYM2.stan"

# set parameters
burnin <- 1000
niter <- 10000
nchains <- 2

# Give adm2_index
bangladesh_2 <- bangladesh_2[order(bangladesh_2$ADM2_EN),]
bangladesh_2$district_id <- 1:nrow(bangladesh_2)
# Set model path
stanfile <- here(paste0("src/model/", model_name))
# vector of outcomes
v_var <- unique(est$variable)
# empty dataframe for storing model info
modinfo <- data.frame()

for(i in 1:length(v_var)){
  
  outcome <- v_var[i]
  
  # subset indicator
  ind_dat <- subset(est, variable == outcome)
  ind_dat <- ind_dat[order(ind_dat$ADM2_EN),]
  
  # Join spatial data
  ind_dist <- bangladesh_2 %>% 
    left_join(ind_dat, by = "ADM2_EN") %>%
    arrange(ADM2_EN)
    
  
  ### removing all LGAs with only one EA 
  ind_dist_complete <- ind_dist |> filter(!is.na(dir_var),degf!=0,dir_var>1e-10) 
  
  # Compile model
  mod <- cmdstan_model(stanfile)
  data_list <- list(
    N = nrow(ind_dist),
    NS = nrow(ind_dist_complete),
    adm2_index = ind_dist_complete$district_id,
    p_hat = ind_dist_complete$dir,
    v_hat = ind_dist_complete$dir_var,
    d = ind_dist_complete$degf, 
    k = ind_dist_complete$n_obs,
    N_edges = length(prep$n1),
    node1 = prep$n1,
    node2 = prep$n2,
    scaling_factor = prep$scaling_factor
  )
  
  # MCMC
  fit <- mod$sample(
    data = data_list,
    seed = 123,
    iter_warmup = burnin,
    iter_sampling = niter,
    save_warmup = FALSE,
    chains = nchains,
    parallel_chains = nchains,
    refresh = 50 # print update every 50 iters
  )
  
  # save fit
  file_name <- paste0("fitBYM2-", outcome, '-', vers, '-', test,"-",format(Sys.Date(), "%Y%m%d"))
  fit$save_object(file = paste0("./gen/model/output/", file_name, ".RDS"))
  # save information about model
  mod <- data.frame(file = file_name,
                         vers = vers,
                         test = test,
                         outcome = outcome,
                         chains = nchains,
                         iter = niter,
                         burnin = burnin,
                         model = model_name)
  modinfo <- rbind(modinfo, mod)
  
  # extract posterior samples
  post_samples = fit$draws(format = "df",  inc_warmup = F)
  post_samples$chain = as.character(post_samples$.chain)
  colnames(post_samples) <- colnames(post_samples) %>%
    gsub("\\[", "_", .) %>%
    gsub("\\]", "", .) %>%
    gsub(",","_", .)
  
  df_p = fit$draws(format = "df", variables=c('p'),  inc_warmup = F) |> mutate(chain=as.character(.chain)) 
  df_v = fit$draws(format = "df", variables=c('v'),  inc_warmup = F) |> mutate(chain=as.character(.chain))
  colnames(df_p) <- colnames(df_p) %>%
    gsub("\\[", "", .) %>%
    gsub("\\]", "", .) %>%
    gsub(",","_", .)
  colnames(df_v) <- colnames(df_v) %>%
    gsub("\\[", "", .) %>%
    gsub("\\]", "", .) %>%
    gsub(",","_", .)
  
  # for (param in colnames(post_samples)[2:10]){
  #   dat = post_samples#[post_samples$chain==2,]
  #   gg = ggplot(data=dat, aes_string(x=".iteration", y = param, color="chain"))+
  #     geom_line() + theme_minimal() 
  #   print(gg)
  # }
  # 
  # for (param in colnames(df_p)[1:10]){
  #   dat = df_p
  #   gg = ggplot(data=dat, aes_string(x=".iteration", y = param, color="chain"))+
  #     geom_line() + theme_minimal() 
  #   print(gg)
  # }
  # 
  # for (param in colnames(df_v)[1:10]){
  #   dat = df_v
  #   gg = ggplot(data=dat, aes_string(x=".iteration", y = param, color="chain"))+
  #     geom_line() + theme_minimal() 
  #   print(gg)
  # }
  # 
  
  postpred <- ind_dat
  postpred$post_mean <- df_p[,1:nrow(postpred)] |> colMeans() |> unname()
  postpred$post_var <-  sapply(df_p[,1:nrow(postpred)], var) |> unname()
  postpred$naive <- postpred$naive * 100
  postpred$dir <- postpred$dir * 100
  postpred$post_mean <- postpred$post_mean * 100
  
  # save posterior predictions
  file_name <- paste0("postpredBYM2-", outcome, '-', vers, '-', test,"-",format(Sys.Date(), "%Y%m%d"))
  write.csv(postpred, paste0("./gen/model/output/", file_name, ".csv"), row.names = FALSE)
  
}

# if onld model info has the same file name as in new, drop
old_modinfo <- subset(old_modinfo, !(file %in% modinfo$file))
new_modinfo <- rbind(modinfo, old_modinfo)
new_modinfo <- new_modinfo[order(new_modinfo$file)]
write.csv(new_modinfo, paste0("./gen/model/audit/model-info-test.csv"), row.names = FALSE)




