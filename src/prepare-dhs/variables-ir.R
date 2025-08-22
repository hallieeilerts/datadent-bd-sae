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
dat <- read_dta("./data/BD_2022_DHS_03042025_2114_120781/BDIR81DT/BDIR81FL.DTA")
## District names for clusters
clusters <- read.csv("./gen/prepare-shp/output/cluster-locations.csv")
################################################################################

# Try to find variable with column name
#View(dat[,grepl("m42", names(dat))])

# Try to find variable based on labels
labels <- map_chr(dat, ~ attr(.x, "label"))
df_labels <- tibble(
  variable = names(labels),
  label = labels)
#View(df_labels[grepl("vitamin a", df_labels$label),])
# View(df_labels[grepl("mineral", df_labels$label),])
# View(df_labels[grepl("vitamin", df_labels$label),])
# View(df_labels[grepl("yesterday", df_labels$label),])
# View(df_labels[grepl("cash", df_labels$label),])
# df_labels[grepl("assistance", df_labels$label),]
# df_labels[grepl("social", df_labels$label),]
# df_labels[grepl("protection", df_labels$label),]
#View(df_labels[grepl("breastfeeding", df_labels$label),])

# calculate sampling weight
dat$wt <- dat$v005/1000000

# merge on district names
dat <- merge(dat, clusters, by.x = "v001", by.y = "DHSCLUSTER")

# calculate outcome variables
dat_var <- fn_gen_nt_wm_micro_iron(dat)
dat_var <- fn_gen_nt_wm_micro_iron_any(dat_var)
dat_var <- fn_gen_rh_anc_4vs(dat_var)
dat_var <- fn_gen_rh_anc_1tri(dat_var)
dat_var <- fn_gen_rh_anc_1vs(dat_var)
dat_var <- fn_gen_rh_anc_bldpres(dat_var)
dat_var <- fn_gen_rh_anc_urine(dat_var)
dat_var <- fn_gen_rh_anc_bldsamp(dat_var)
dat_var <- fn_gen_rh_anc_wgt(dat_var)
dat_var <- fn_gen_rh_anc_iron(dat_var)
dat_var <- fn_gen_rh_pnc_nb_2days(dat_var)
dat_var <- fn_gen_rh_pnc_wm_2days(dat_var)
dat_var <- fn_gen_rh_pnc_wm_bfcounsel(dat_var)
#dat_var <- fn_gen_nt_wm_micro_iod(dat_var)

dat_var <- dat_var %>%
  dplyr::select(ADM2_EN, ADM1_EN, v001, v002, v003, v012, v023, v024, v025, v106, 
         nt_wm_micro_iron, nt_wm_micro_iron_any, 
         rh_anc_4vs, rh_anc_1vs, rh_anc_1tri, rh_anc_bldpres,
         rh_anc_urine, rh_anc_bldsamp, rh_anc_wgt, rh_anc_iron, 
         rh_pnc_wm_2days, rh_pnc_nb_2days, rh_pnc_wm_bfcounsel,
         wt) %>%
  mutate(mother_edu = as.character(as_factor(v106)),
         region_name = as.character(as_factor(v024)),
         residence = as.character(as_factor(v025))) %>%
  rename(mother_ln = v003,
         mother_age = v012) %>%
  dplyr::select(-c(v106, v024, v025))

# Save --------------------------------------------------------------------

saveRDS(dat_var, file = "./gen/prepare-dhs/temp/variables-ir.rds")

