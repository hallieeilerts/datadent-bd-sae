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
vas <- list()
for(i in 1:length(dat_filename)){
  load(paste0("./gen/model/output/",dat_filename[i]))
  # predict district prevalence
  vas[[i]] <- fn_pred_wtd(fit, dat)
  vas[[i]]$file <- gsub(".RData", "", dat_filename[i])
}
vas <- do.call(rbind, vas)

# load deworming model output
dat_filename <- list.files("./gen/model/output/")
dat_filename <- dat_filename[grepl("ModelOutputChild-DWM", dat_filename, ignore.case = TRUE)]
dwm <- list()
for(i in 1:length(dat_filename)){
  load(paste0("./gen/model/output/",dat_filename[i]))
  # predict district prevalence
  dwm[[i]] <- fn_pred_wtd(fit, dat)
  dwm[[i]]$file <- gsub(".RData", "", dat_filename[i])
}
dwm <- do.call(rbind, dwm)

# load exclusive breastfeeding
dat_filename <- list.files("./gen/model/output/")
dat_filename <- dat_filename[grepl("ModelOutputChild-EBF", dat_filename, ignore.case = TRUE)]
exbf <- list()
for(i in 1:length(dat_filename)){
  load(paste0("./gen/model/output/",dat_filename[i]))
  # predict district prevalence
  exbf[[i]] <- fn_pred_wtd(fit, dat)
  exbf[[i]]$file <- gsub(".RData", "", dat_filename[i])
}
exbf <- do.call(rbind, exbf)

# load iron supplements for pregnant women
dat_filename <- list.files("./gen/model/output/")
dat_filename <- dat_filename[grepl("ModelOutputMother-Iron", dat_filename, ignore.case = TRUE)]
iron <- list()
for(i in 1:length(dat_filename)){
  load(paste0("./gen/model/output/",dat_filename[i]))
  # predict district prevalence
  iron[[i]] <- fn_pred_wtd(fit, dat)
  iron[[i]]$file <- gsub(".RData", "", dat_filename[i])
}
iron <- do.call(rbind, iron)

# load anc4
dat_filename <- list.files("./gen/model/output/")
dat_filename <- dat_filename[grepl("ModelOutputMother-ANC4", dat_filename, ignore.case = TRUE)]
anc4 <- list()
for(i in 1:length(dat_filename)){
  load(paste0("./gen/model/output/",dat_filename[i]))
  # predict district prevalence
  anc4[[i]] <- fn_pred_wtd(fit, dat)
  anc4[[i]]$file <- gsub(".RData", "", dat_filename[i])
}
anc4 <- do.call(rbind, anc4)

# load anc 1st trimester
dat_filename <- list.files("./gen/model/output/")
dat_filename <- dat_filename[grepl("ModelOutputMother-ANC1tri", dat_filename, ignore.case = TRUE)]
anc1t <- list()
for(i in 1:length(dat_filename)){
  load(paste0("./gen/model/output/",dat_filename[i]))
  # predict district prevalence
  anc1t[[i]] <- fn_pred_wtd(fit, dat)
  anc1t[[i]]$file <- gsub(".RData", "", dat_filename[i])
}
anc1t <- do.call(rbind, anc1t)

district_preds <- rbind(vas, dwm, exbf, iron, anc4, anc1t)

# Save --------------------------------------------------------------------

write.csv(district_preds, paste0("./gen/model/output/predictions.csv"), row.names = FALSE)

# Only update one
district_preds <- read.csv("./gen/model/output/predictions.csv")
district_preds <- district_preds[!grepl("iron", district_preds$file, ignore.case = TRUE),]
district_preds <- rbind(district_preds, iron)
write.csv(district_preds, paste0("./gen/model/output/predictions.csv"), row.names = FALSE)

