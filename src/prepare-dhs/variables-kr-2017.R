################################################################################
#' @description Calculate mother-level variables for model input
#' @return DHS dataset with shape file admin2 district name and other generated variables added
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(haven)
library(tidyverse)
library(naniar)
#' Inputs
source("./src/util.R")
## DHS individual recode
dat <- read_dta("./data/BD_2017-18_DHS_08202025_153_120781/BDKR7RDT/BDKR7RFL.DTA")
## District names for clusters
clusters <- read.csv("./gen/prepare-shp/output/cluster-locations-2017.csv")
################################################################################

# # Try to find variable based on labels
# labels <- map_chr(dat, ~ attr(.x, "label"))
# df_labels <- tibble(
#   variable = names(labels),
#   label = labels)
# #for growth monitoring, in ir dataset this is s321c
# View(df_labels[grepl("growth", df_labels$label, ignore.case = TRUE),])
# View(df_labels[grepl("weight", df_labels$label, ignore.case = TRUE),])
# #s321c # services received: child growth monitoring
# View(dat[,grepl("s32", names(dat))])

# calculate sampling weight
dat$wt <- dat$v005/1000000

# merge on district names
dat <- merge(dat, clusters, by.x = "v001", by.y = "DHSCLUSTER")

# calculate outcome variables
dat_var <- fn_gen_nt_ch_gwmt_any(dat)
dat_var <- fn_gen_ch_allvac_either(dat_var)

dat_var <- dat_var %>%
  select(ADM2_EN, ADM1_EN, bidx, v001, v002, v003, v008, v023, v024, v025, b3, b16, 
         nt_ch_gwmt_any, ch_allvac_either,
         wt) %>%
  mutate(region_name = as.character(as_factor(v024)),
         residence = as.character(as_factor(v025)),
         hhid = paste(v001, v002, sep = "_"),
         child_age = (v008 - b3)/12) %>%
  rename(child_ln = b16,
         mother_ln = v003) %>%
  select(-c(v008, v024, b3))

# Save --------------------------------------------------------------------

saveRDS(dat_var, file = "./gen/prepare-dhs/temp/variables-kr-2017.rds")
