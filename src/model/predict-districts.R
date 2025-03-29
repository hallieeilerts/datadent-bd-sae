################################################################################
#' @description Predict districts
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(brms)
#' Inputs
source("./src/util.R")
# spatial correlation
spatial_cor_matrix <- readRDS("./gen/prepare-shp/output/adjacency_matrix.rds")
################################################################################

# load vitamin A model output
dat_filename <- list.files("./gen/model/output/")
dat_filename <- dat_filename[grepl("ModelOutputChild-VAS", dat_filename, ignore.case = TRUE)]
#dat_filename <- tail(sort(dat_filename),1) # Most recent
vas <- list()
for(i in 1:length(dat_filename)){
  load(paste0("./gen/model/output/",dat_filename[i]))
  # Visualize the posterior distributions of the fixed effects
  #plot(fit)
  # predict district prevalence
  vas[[i]] <- fn_pred_wtd(fit, dat, outcome_name)
  vas[[i]]$model <- paste0(vers,"-", test)
}
vas <- do.call(rbind, vas)

# load deworming model output
dat_filename <- list.files("./gen/model/output/")
dat_filename <- dat_filename[grepl("ModelOutputChild-DWM", dat_filename, ignore.case = TRUE)]
#dat_filename <- tail(sort(dat_filename),1) # Most recent
dwm <- list()
for(i in 1:length(dat_filename)){
  load(paste0("./gen/model/output/",dat_filename[i]))
  # predict district prevalence
  dwm[[i]] <- fn_pred_wtd(fit, dat, outcome_name)
  dwm[[i]]$model <- paste0(vers,"-",test)
}
dwm <- do.call(rbind, dwm)

# load exclusive breastfeeding
dat_filename <- list.files("./gen/model/output/")
dat_filename <- dat_filename[grepl("ModelOutputChild-EBF", dat_filename, ignore.case = TRUE)]
exbf <- list()
for(i in 1:length(dat_filename)){
  load(paste0("./gen/model/output/",dat_filename[i]))
  # predict district prevalence
  exbf[[i]] <- fn_pred_wtd(fit, dat, outcome_name)
  exbf[[i]]$model <- paste0(vers,"-",test)
}
exbf <- do.call(rbind, exbf)


# load pregnant women iron supplements model output
dat_filename <- list.files("./gen/model/output/")
dat_filename <- dat_filename[grepl("ModelOutputMother-Iron", dat_filename, ignore.case = TRUE)]
#dat_filename <- tail(sort(dat_filename),1) # Most recent
iron <- list()
for(i in 1:length(dat_filename)){
  load(paste0("./gen/model/output/",dat_filename[i]))
  # predict district prevalence
  iron[[i]] <- fn_pred_wtd(fit, dat, outcome_name)
  iron[[i]]$model <- paste0(vers,"-",test)
}
iron <- do.call(rbind, iron)

# load anc4
dat_filename <- list.files("./gen/model/output/")
dat_filename <- dat_filename[grepl("ModelOutputMother-ANC4", dat_filename, ignore.case = TRUE)]
anc4 <- list()
for(i in 1:length(dat_filename)){
  load(paste0("./gen/model/output/",dat_filename[i]))
  # predict district prevalence
  anc4[[i]] <- fn_pred_wtd(fit, dat, outcome_name)
  anc4[[i]]$model <- paste0(vers,"-",test)
}
anc4 <- do.call(rbind, anc4)

# load anc 1st trimester
dat_filename <- list.files("./gen/model/output/")
dat_filename <- dat_filename[grepl("ModelOutputMother-ANC1tri", dat_filename, ignore.case = TRUE)]
anc1t <- list()
for(i in 1:length(dat_filename)){
  load(paste0("./gen/model/output/",dat_filename[i]))
  # predict district prevalence
  anc1t[[i]] <- fn_pred_wtd(fit, dat, outcome_name)
  anc1t[[i]]$model <- paste0(vers,"-",test)
}
anc1t <- do.call(rbind, anc1t)


district_preds <- rbind(vas, dwm, exbf, iron, anc4, anc1t)

# Save --------------------------------------------------------------------

write.csv(district_preds, paste0("./gen/model/output/predictions.csv"), row.names = FALSE)

