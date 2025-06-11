################################################################################
#' @description Aggregate adm2 predictions to adm1 level for all predicted outcomes/models
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(dplyr)
library(tidyr)
library(here)
library(cmdstanr)
#' Inputs
source("./src/util.R")
# direct adm2 estimates
direct_adm2 <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
################################################################################

# outcomes
v_outcomes <- c("nt_ch_micro_vas", "nt_ch_micro_dwm", "nt_ebf", "nt_wm_micro_iron", "rh_anc_4vs", "rh_anc_1tri")
l_mod <- list()
l_res <- list()
for(i in 1:length(v_outcomes)){
  
  # select outcome
  outcome <- v_outcomes[i]
  # subset direct_adm2 weights this outcome (will differ whether mother-level or child-level)
  ind_dat <- subset(direct_adm2, variable == outcome)[,c("ADM2_EN", "ADM1_EN", "sum_wgt")]
  
  # second loop for all models that have been run for this outcome
  v_modfits <- list.files("./gen/model/fit")
  v_modfits <- v_modfits[grepl(".rds", v_modfits)]
  v_modfits <- v_modfits[grepl(outcome, v_modfits)]
  
  for(j in 1:length(v_modfits)){
    
    file_name <- v_modfits[j]
    # read rds file that contains names of stan csv files
    stan_csv_filenames <- readRDS(here("gen", "model", "fit", paste0(file_name)))
    # read stan csv files as cmdstan object
    stanfit <- as_cmdstan_fit(stan_csv_filenames)
    
    # extract model estimates
    df_p = stanfit$draws(format = "df", variables=c('p'),  inc_warmup = F) |> mutate(chain=as.character(.chain)) 
    colnames(df_p) <- colnames(df_p) %>%
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .) %>%
      gsub(",","_", .)
    
    # merge onto info for direct estimates that has adm1 and adm2 labels, and adm2 weights (sum_wgt)
    df_p_t <- as.data.frame(t(df_p[,1:nrow(ind_dat)]))
    names(df_p_t) <- paste0("X", 1:ncol(df_p_t))
    df_post <- cbind(ind_dat, df_p_t)
    
    # Group by adm1 and calculate weighted mean for adm2 estimates
    df_post_div <- df_post |> as.data.frame() |>
      group_by(ADM1_EN) |>
      summarise(across(all_of(colnames(df_post)[grepl('X',colnames(df_post))]), ~ weighted.mean(., w = sum_wgt)))
    
    # compute mean and credible interval across all posterior samples
    alpha = 0.05
    df_post_div$post_mean <- df_post_div[,2:(nrow(df_p)+1)] |> rowMeans()
    df_quantile =  apply(df_post_div[,2:nrow(df_p)], 1 , quantile , probs = c(alpha/2,1-alpha/2) , na.rm = TRUE ) |> t()
    df_post_div$qt_lb <- df_quantile[, 1]
    df_post_div$qt_ub <- df_quantile[, 2]
    df_post_div$length95 <- df_post_div$qt_ub - df_post_div$qt_lb
    mean(df_post_div$length95)
    df_post_div <- df_post_div |> select(-colnames(df_post)[grepl('X',colnames(df_post))])
    
    filecol <- sub("csv_files-fit-", "", file_name)
    filecol <- sub(".rds", "", filecol)
    df_post_div$file <- filecol
    
    l_mod[[j]] <- df_post_div
  }
  df_outcomemods <- do.call(rbind, l_mod)
  l_res[[i]] <- df_outcomemods
}
df_res <- do.call(rbind, l_res)

# Save --------------------------------------------------------------------

write.csv(df_res, file = "./gen/validation/temp/agg-adm2-pred.csv", row.names = FALSE)
