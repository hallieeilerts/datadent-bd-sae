################################################################################
#' @description Fit model with fixed effects
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
# Direct estimates and variance of outcomes
est <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
# District-level covariates
covar <- read.csv("./gen/prepare-dhs/output/covar-district.csv")
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
# indicator info for which covariates to use
ind <- read_excel("./data/ind-info.xlsx", sheet = "indicators")
indcovar <- read_excel("./data/ind-info.xlsx", sheet = "covariates")
################################################################################

# set version of model
# the first version fit will be this plus one (j)
vers_start <- 100
test <- 1
model_name <- "ArealBYM2"
model_file <- "areal_level_BYM2_FE.stan"

# subset to included indicators
df_ind <- subset(ind, status == "include")

# generate plots?
make_plots <- FALSE

# set parameters
burnin <- 1000
niter <- 1000
nchains <- 4

# create adm2_index
bangladesh_2 <- bangladesh_2[order(bangladesh_2$ADM2_EN),]
bangladesh_2$district_id <- 1:nrow(bangladesh_2)
# set model
stanfile <- here(paste0("src/model/", model_file))
# vector of outcomes
v_var <- unique(df_ind$variable)

# empty dataframe for storing model info
modinfo <- data.frame()

# merge outcome variables with district level covariates
dat <- merge(est, covar, by = c("ADM2_EN", "variable"))

# for each indicator
for(i in 1:length(v_var)){

  myoutcome <- v_var[i]
  print(myoutcome)
  
  # subset indicator
  ind_dat <- subset(dat, variable == myoutcome)
  ind_dat <- ind_dat[order(ind_dat$ADM2_EN),]
  
  # REMOVING BUFFER
  # bound direct estimate between zero and 1
  #eps <- 1e-10  # small buffer away from 0 and 1
  #ind_dat$dir <- pmin(1 - eps, pmax(eps, ind_dat$dir))
  
  # join spatial data
  ind_dist <- bangladesh_2 %>% 
    left_join(ind_dat, by = c("ADM2_EN", "ADM1_EN")) %>%
    arrange(ADM2_EN)
  
  ### removing all districts with only one cluster
  ind_dist_complete <- ind_dist |> filter(!is.na(dir_var),degf!=0,dir_var>1e-10) 
  
  # # set variance floor
  # # c * p*(1-p)/k  (c = 1 is conservative; try 1.5 or 2 if needed)
  # ind_dist_complete <- ind_dist_complete %>%
  #   mutate(v_floor = 1 * dir*(1-dir) / obs_un)
    
  # covariate group for outcome
  v_covar_grp <- subset(ind, variable == myoutcome)$covar_grp
  # covariates for this group
  df_covar_grp <- subset(indcovar, covar_grp == v_covar_grp)
  
  # for each set of covariates
  for(j in 1:(ncol(df_covar_grp)-1)){
    
    #j <- 1
    
    # file name
    vers <- vers_start + j
    file_name <- paste(model_name, myoutcome, vers, test, sep = "-")
    print(file_name)
    
    v_cov_mod <- df_covar_grp[,j+1]
    v_cov_mod <- v_cov_mod[!is.na(v_cov_mod)]
    print(v_cov_mod)
    
    # model matrix for covariates
    fmla <- as.formula(paste("~", paste(v_cov_mod, collapse = " + ")))
    X <- model.matrix(fmla, data = ind_dist_complete) # automatically includes intercept
    D <- length(v_cov_mod) + 1 # number of covar plus 1 for intercept

    # create full model covariate matrix that has zeros for districts with missing data
    # pass this one to the model code which has been updated
    X_full <- matrix(0, nrow = nrow(ind_dist), ncol = D)  
    X_full[ind_dist_complete$district_id, ] <- X  
    
    # Compile model
    mod <- cmdstan_model(stanfile)
    data_list <- list(
      # adding FE
      D = D,
      X = X_full,
      N = nrow(ind_dist),
      NS = nrow(ind_dist_complete),
      adm2_index = ind_dist_complete$district_id,
      p_hat = ind_dist_complete$dir,
      v_hat = ind_dist_complete$dir_var,
      #v_floor = ind_dist_complete$v_floor,
      d = ind_dist_complete$degf, 
      k = ind_dist_complete$obs_un,
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
    fit$save_output_files(dir = here("gen", "model", "fit"), basename = paste0("fit-", file_name), timestamp = FALSE)
    #  save names of csv files as rds
    csv_files <- fit$output_files()
    write_rds(csv_files, here("gen", "model", "fit", paste0("csv_files-", paste0("fit-", file_name), ".rds")))
    
    # save information about model
    df_info <- data.frame(file = file_name,
                          model_file = model_file,
                          model_name = model_name,
                          vers = vers,
                          test = test,
                          outcome = myoutcome,
                          cov = paste(v_cov_mod, collapse = ","),
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
    
    #postpred %>% select(ADM2_EN, naive, naive_var, n_obs, dir, dir_var, dirplot, dirplot_var, post_mean, qt_lb, qt_ub) %>% mutate_if(is.numeric, round, digits = 4) %>% filter(ADM2_EN == "Feni")
    
    # save posterior predictions
    write.csv(postpred, paste0("./gen/model/pred/", paste0("pred-", file_name), ".csv"), row.names = FALSE)
    
    # if old model info has the same file name as in new, drop
    if(nrow(old_modinfo) > 0){
      old_modinfo <- subset(old_modinfo, !(file %in% modinfo$file))
    }
    new_modinfo <- rbind(modinfo, old_modinfo)
    new_modinfo <- new_modinfo[order(new_modinfo$file),]
    write.csv(new_modinfo, paste0("./gen/model/audit/model-info.csv"), row.names = FALSE)
    
  }
}





