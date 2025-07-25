################################################################################
#' @description Fit model with just intercept
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
library(readr)
#' Inputs
source("./src/util.R")
# Direct estimates and variance
est <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
# Bangladesh district boundaries
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
# Adjacency matrix
prep <- readRDS("./gen/prepare-shp/output/adjacency_matrix.rds")
# model info from previous models if it exists
audit_files <- dir("./gen/model/audit/")
if(sum(grepl("model-info", audit_files)) > 0){
  old_modinfo <- read.csv("./gen/model/audit/model-info.csv")
}else{
  old_modinfo <- data.frame()
}
################################################################################

# set model
vers <- "100" 
test <- "Test2"
model_name <- "ArealBYM2"
model_file <- "areal_level_BYM2_intercept.stan"

# generate plots?
make_plots <- FALSE

# set parameters
burnin <- 1000
niter <- 1000
nchains <- 2

# create adm2_index
bangladesh_2 <- bangladesh_2[order(bangladesh_2$ADM2_EN),]
bangladesh_2$district_id <- 1:nrow(bangladesh_2)
# set model path
stanfile <- here(paste0("src/model/", model_file))
# vector of outcomes
v_var <- unique(est$variable)
# empty dataframe for storing model info
modinfo <- data.frame()

for(i in 1:length(v_var)){
  
  outcome <- v_var[i]
  print(outcome)
  file_name <- paste(model_name, outcome, vers, test, sep = "-")
  
  # subset indicator
  ind_dat <- subset(est, variable == outcome)
  ind_dat <- ind_dat[order(ind_dat$ADM2_EN),]
  
  # join spatial data
  ind_dist <- bangladesh_2 %>% 
    left_join(ind_dat, by = c("ADM1_EN", "ADM2_EN")) %>%
    arrange(ADM2_EN)
    
  
  ### removing all districts with only one cluster
  ind_dist_complete <- ind_dist |> filter(!is.na(dir_var),degf!=0,dir_var>1e-10) 
  if(nrow(ind_dist) != nrow(ind_dist_complete)){
    stop("district removed")
  }
  
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
  
  # save fit as csv files
  fit$save_output_files(dir = here("gen","model", "fit"), basename = paste0("fit-", file_name), timestamp = FALSE)
  #  save names of csv files as rds
  csv_files <- fit$output_files()
  write_rds(csv_files, here("gen", "model", "fit", paste0("csv_files-", paste0("fit-", file_name), ".rds")))
  
  # save information about model
  df_info <- data.frame(file = file_name,
                         model_file = model_file,
                         model_name = model_name,
                         vers = vers,
                         test = test,
                         outcome = outcome,
                         cov = 1,
                         chains = nchains,
                         iter = niter,
                         burnin = burnin,
                         date = format(Sys.Date(), "%Y%m%d"))
  modinfo <- rbind(modinfo, df_info)
  
  # extract posterior samples
  post_samples = fit$draws(format = "df",  inc_warmup = F)
  post_samples$chain = as.character(post_samples$.chain)
  colnames(post_samples) <- colnames(post_samples) %>%
    gsub("\\[", "_", .) %>%
    gsub("\\]", "", .) %>%
    gsub(",","_", .)
  
  # extract prevalence
  df_p = fit$draws(format = "df", variables=c('p'),  inc_warmup = F) |> mutate(chain=as.character(.chain)) 
  colnames(df_p) <- colnames(df_p) %>%
    gsub("\\[", "", .) %>%
    gsub("\\]", "", .) %>%
    gsub(",","_", .)
  
  if(make_plots){
    
    df_v = fit$draws(format = "df", variables=c('v'),  inc_warmup = F) |> mutate(chain=as.character(.chain))
    df_sigmau = fit$draws(format = "df", variables=c('sigma_u'),  inc_warmup = F) |> mutate(chain=as.character(.chain))
    df_sigmatau = fit$draws(format = "df", variables=c('sigma_tau'),  inc_warmup = F) |> mutate(chain=as.character(.chain))
    df_rho = fit$draws(format = "df", variables=c('rho'),  inc_warmup = F) |> mutate(chain=as.character(.chain))
    df_gamma0 = fit$draws(format = "df", variables=c('gamma0'),  inc_warmup = F) |> mutate(chain=as.character(.chain))
    df_gamma1 = fit$draws(format = "df", variables=c('gamma1'),  inc_warmup = F) |> mutate(chain=as.character(.chain))
    df_gamma2 = fit$draws(format = "df", variables=c('gamma2'),  inc_warmup = F) |> mutate(chain=as.character(.chain))
    df_u1 = fit$draws(format = "df", variables=c('u1'),  inc_warmup = F) |> mutate(chain=as.character(.chain))
    df_u2 = fit$draws(format = "df", variables=c('u2'),  inc_warmup = F) |> mutate(chain=as.character(.chain))
    # extract FE, variables= ('beta')
    
    colnames(df_v) <- colnames(df_v) %>%
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .) %>%
      gsub(",","_", .)
    colnames(df_sigmau) <- colnames(df_sigmau) %>%
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .) %>%
      gsub(",","_", .)
    colnames(df_sigmatau) <- colnames(df_sigmatau) %>%
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .) %>%
      gsub(",","_", .)
    colnames(df_rho) <- colnames(df_rho) %>%
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .) %>%
      gsub(",","_", .)
    colnames(df_gamma0) <- colnames(df_gamma0) %>%
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .) %>%
      gsub(",","_", .)
    colnames(df_gamma1) <- colnames(df_gamma1) %>%
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .) %>%
      gsub(",","_", .)
    colnames(df_gamma2) <- colnames(df_gamma2) %>%
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .) %>%
      gsub(",","_", .)
    colnames(df_u1) <- colnames(df_u1) %>%
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .) %>%
      gsub(",","_", .)
    colnames(df_u2) <- colnames(df_u2) %>%
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .) %>%
      gsub(",","_", .)
    
    # check chains
    # u1, u2, p, v are one for each district
    # checks second thru tenth parameter
    for (param in colnames(post_samples)[2:10]){
      dat = post_samples #[post_samples$chain==2,]
      gg = ggplot(data=dat, aes_string(x=".iteration", y = param, color="chain"))+
        geom_line() + theme_minimal()
      print(gg)
    }
    # all districts
    df_u1 %>%
      pivot_longer(
        cols = -c(chain, .iteration, .draw, .chain)
      ) %>%
      ggplot() +
      geom_line(aes(x= .iteration, y = value, color = chain)) +
      facet_wrap(~name)
    df_u2 %>%
      pivot_longer(
        cols = -c(chain, .iteration, .draw, .chain)
      ) %>%
      ggplot() +
      geom_line(aes(x= .iteration, y = value, color = chain)) +
      facet_wrap(~name)
    df_v %>%
      pivot_longer(
        cols = -c(chain, .iteration, .draw, .chain)
      ) %>%
      ggplot() +
      geom_line(aes(x= .iteration, y = value, color = chain)) +
      facet_wrap(~name)
    df_p %>%
      pivot_longer(
        cols = -c(chain, .iteration, .draw, .chain)
      ) %>%
      ggplot() +
      geom_line(aes(x= .iteration, y = value, color = chain)) +
      facet_wrap(~name)
    for (param in colnames(df_p)[1:10]){
      dat = df_p
      gg = ggplot(data=dat, aes_string(x=".iteration", y = param, color="chain"))+
        geom_line() + theme_minimal()
      print(gg)
    }
    
    
    
    # not one per district
    for (param in colnames(df_sigmau)[1]){
      dat = df_sigmau
      gg = ggplot(data=dat, aes_string(x=".iteration", y = param, color="chain"))+
        geom_line() + theme_minimal()
      print(gg)
    }
    for (param in colnames(df_sigmatau)[1]){
      dat = df_sigmatau
      gg = ggplot(data=dat, aes_string(x=".iteration", y = param, color="chain"))+
        geom_line() + theme_minimal()
      print(gg)
    }
    for (param in colnames(df_rho)[1]){
      dat = df_rho
      gg = ggplot(data=dat, aes_string(x=".iteration", y = param, color="chain"))+
        geom_line() + theme_minimal()
      print(gg)
    }
    for (param in colnames(df_gamma0)[1]){
      dat = df_gamma0
      gg = ggplot(data=dat, aes_string(x=".iteration", y = param, color="chain"))+
        geom_line() + theme_minimal()
      print(gg)
    }
    for (param in colnames(df_gamma1)[1]){
      dat = df_gamma1
      gg = ggplot(data=dat, aes_string(x=".iteration", y = param, color="chain"))+
        geom_line() + theme_minimal()
      print(gg)
    }
    for (param in colnames(df_gamma2)[1]){
      dat = df_gamma2
      gg = ggplot(data=dat, aes_string(x=".iteration", y = param, color="chain"))+
        geom_line() + theme_minimal()
      print(gg)
    }
  }
  
  
  alpha = 0.05
  postpred <- ind_dat
  postpred$post_mean <- df_p[,1:nrow(postpred)] |> colMeans() |> unname()
  postpred$post_var <-  sapply(df_p[,1:nrow(postpred)], var) |> unname()
  df_quantile =  apply(df_p[,1:nrow(postpred)], 2 , quantile , probs = c(alpha/2,1-alpha/2) , na.rm = TRUE ) |> t()
  postpred$qt_lb <- df_quantile[, 1]
  postpred$qt_ub <- df_quantile[, 2]
  postpred$naive <- postpred$naive * 100
  postpred$dir <- postpred$dir * 100
  postpred$post_mean <- postpred$post_mean * 100
  
  # save posterior predictions
  write.csv(postpred, paste0("./gen/model/pred/", paste0("pred-", file_name), ".csv"), row.names = FALSE)
  
}

# if old model info has the same file name as in new, drop
if(nrow(old_modinfo) > 0){
  old_modinfo <- subset(old_modinfo, !(file %in% modinfo$file))
}
new_modinfo <- rbind(modinfo, old_modinfo)
new_modinfo <- new_modinfo[order(new_modinfo$file),]
write.csv(new_modinfo, paste0("./gen/model/audit/model-info.csv"), row.names = FALSE)




