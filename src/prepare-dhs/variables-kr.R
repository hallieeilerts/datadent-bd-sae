################################################################################
#' @description Calculate child-level variables for model input
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
## DHS child recode
dat <- read_dta("./data/BD_2022_DHS_03042025_2114_120781/BDKR81DT/BDKR81FL.DTA")
## District names for clusters
clusters <- read.csv("./gen/prepare-shp/output/cluster-locations.csv")
################################################################################

# Try to find variable based on labels
labels <- map_chr(dat, ~ attr(.x, "label"))
df_labels <- tibble(
  variable = names(labels),
  label = labels)
# View(df_labels[grepl("mineral", df_labels$label),])
# View(df_labels[grepl("vitam", df_labels$label),])
# View(df_labels[grepl("supple", df_labels$label),])
# View(df_labels[grepl("yesterday", df_labels$label),])
# View(df_labels[grepl("24 hours", df_labels$label),])
df_labels[grepl("mineral", df_labels$label),]
#View(df_labels[grepl("vitamin", df_labels$label),])
df_labels[grepl("yesterday", df_labels$label, ignore.case = TRUE),]
df_labels[grepl("mineral", df_labels$label, ignore.case = TRUE),]
df_labels[grepl("vitamin", df_labels$label, ignore.case = TRUE),]
#View(dat[,grepl("^h", names(dat))])

# Child drank or ate vitamin or mineral supplements yesterday
# Children living in a HH with SP receipt within past year
# Household received cash or food assistance
# Household with child or WRA received SP in last year

# calculate sampling weight
dat$wt <- dat$v005/1000000

# merge on district names
dat <- merge(dat, clusters, by.x = "v001", by.y = "DHSCLUSTER")

# calculate outcome variables
dat_var <- fn_gen_nt_ch_micro_vas(dat)
dat_var <- fn_gen_nt_ch_micro_dwm(dat_var)
dat_var <- fn_gen_nt_ebf(dat_var)
dat_var <- fn_gen_nt_mdd(dat_var)
dat_var <- fn_gen_nt_ch_micro_mp(dat_var)
dat_var <- fn_gen_ch_diar_zinc(dat_var)
dat_var <- fn_gen_ch_diar_ors(dat_var)
dat_var <- fn_gen_nt_ch_micro_iron(dat_var)
#dat_var <- fn_gen_nt_mdd(dat_var)         # not needed
#dat_var <- fn_gen_ch_meas_either(dat_var) # missing
#dat_var <- fn_gen_ch_rotav3_either(dat_var) # missing
#dat_var <- fn_gen_ch_pent3_either(dat_var) # missing
#dat_var <- fn_gen_nt_ch_micro_iod(dat_var) # missing

dat_var <- dat_var %>%
  select(ADM2_EN, ADM1_EN, bidx, v001, v002, v003, v008, v023, v024, v025, b3, b16, 
         nt_ch_micro_vas, nt_ch_micro_dwm, nt_ebf, 
         nt_ch_micro_mp, ch_diar_zinc, ch_diar_ors, 
         nt_ch_micro_iron, wt) %>%
  mutate(region_name = as.character(as_factor(v024)),
         residence = as.character(as_factor(v025)),
         hhid = paste(v001, v002, sep = "_"),
         child_age = (v008 - b3)/12) %>%
  rename(child_ln = b16,
         mother_ln = v003) %>%
  select(-c(v008, v024, b3))


dhs_svy <- dat_var %>% as_survey_design(ids = "v001", # psu
#                                     strata = "v023", # strata for sampling
#                                     weights = "wt",
#                                     nest = TRUE)
# dir <- dhs_svy %>%
#   group_by(ADM2_EN) %>%
#   summarise(nt_ch_micro_dwm = survey_mean(nt_ch_micro_dwm, na.rm = TRUE, vartype = "var"))
# # 6-59
# foo1 <- dir
# # 6-23
# foo2 <- dir
# p <- foo1 %>%
#   mutate(age = "6-59m") %>%
#   bind_rows(foo2 %>% mutate(age = "6-23m")) %>%
#   ggplot() +
#   geom_bar(aes(x=ADM2_EN, y = nt_ch_micro_dwm, fill = age), position = "dodge", stat = "identity") +
#   coord_flip() +
#   theme(text = element_text(size = 8))
# ggsave("./gen/prepare-dhs/audit/ch_deworming.png", p, dpi = 500, width = 6, height = 4)

# Save --------------------------------------------------------------------

saveRDS(dat_var, file = "./gen/prepare-dhs/temp/variables-kr.rds")

