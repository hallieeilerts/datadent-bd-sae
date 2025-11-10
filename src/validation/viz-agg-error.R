################################################################################
#' @description Plot error and overlap for aggregated predictions
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(stringr)
library(readxl)
#' Inputs
source("./src/util.R")
ind <- read_excel("./data/ind-info.xlsx", sheet = "indicators")
# error in aggregated estimates
error_adm1 <- read.csv("./gen/validation/output/agg-error-adm1.csv")
error_adm0 <- read.csv("./gen/validation/output/agg-error-adm0.csv")
# minimum error model for adm1
minerror_adm1_mod <- read.csv("./gen/validation/output/agg-minerror-adm1.csv")
# model info
modinfo <- read.csv("./gen/model/audit/model-info.csv")
# Bangladesh division boundaries
bangladesh_1 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm1_bbs_20201113")
################################################################################

# subset to included indicators
df_ind <- subset(ind, status == "include")

# vector of indicators
v_var <- unique(df_ind$variable)

# adm1 rsme --------------------------------------------------------------------

# Plot RMSE for each indicator by model
p1 <- error_adm1 %>% 
  group_by(variable, model) %>%
  mutate(sqerror = (error)^2) %>%
  summarise(RMSE = mean(sqerror)) %>%
  group_by(variable) %>%
  mutate(minRMSE = min(RMSE)) %>%
  mutate(minbar = ifelse(RMSE == minRMSE, "min", "other")) %>%
  ggplot() +
  geom_bar(aes(x=model, y = RMSE, fill = minbar), stat = "identity") +
  labs(x = "model", title = "Error") +
  theme_bw() +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("red","grey38")) + 
  coord_flip() +
  facet_wrap(~variable, scales = "free_x") +
  guides(fill = "none")
ggsave(paste0("./gen/validation/audit/adm1-rsme-allmodels.png"), p1, width = 8, height = 10, units = "in", dpi = 300)

# Plot RMSE for all indicators combined
p2 <- error_adm1 %>% 
  group_by(model) %>%
  mutate(sqerror = (error)^2) %>%
  summarise(RMSE = mean(sqerror)) %>%
  mutate(minRMSE = min(RMSE)) %>%
  mutate(minbar = ifelse(RMSE == minRMSE, "min", "other")) %>%
  ggplot() +
  geom_bar(aes(x=model, y = RMSE, fill = minbar), stat = "identity") +
  labs(x = "model", title = "Error") +
  theme_bw() +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("red","grey38")) + 
  coord_flip() +
  guides(fill = "none")
ggsave("./gen/validation/audit/adm1-rsme-all.png", p2, width = 4, height = 5, dpi = 300)


# Plot RMSE for indicator type
p3 <- error_adm1 %>% 
  left_join(ind %>% select(variable, covar_grp)) %>%
  group_by(covar_grp, model) %>%
  mutate(sqerror = (error)^2) %>%
  summarise(RMSE = mean(sqerror)) %>%
  group_by(covar_grp) %>%
  mutate(minRMSE = min(RMSE)) %>%
  mutate(minbar = ifelse(RMSE == minRMSE, "min", "other")) %>%
  ggplot() +
  geom_bar(aes(x=model, y = RMSE, fill = minbar), stat = "identity") +
  labs(x = "model", title = "Error") +
  theme_bw() +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("red","grey38")) + 
  coord_flip() +
  facet_wrap(~covar_grp, scales = "free_x") +
  guides(fill = "none")
ggsave("./gen/validation/audit/adm1-rsme-byCovarGrp.png", p3, width = 6, height = 5, dpi = 300)


# adm1 rsme normalized ---------------------------------------------------------

# Normalized RMSE by indicator and model
p1 <- error_adm1 %>% 
  group_by(variable, model) %>%
  summarise(NRMSE = sqrt(mean(error^2))/(max(dir) - min(dir))) %>%
  group_by(variable) %>%
  mutate(minNRMSE = min(NRMSE)) %>%
  mutate(minbar = ifelse(NRMSE == minNRMSE, "min", "other")) %>%
  ggplot() +
  geom_bar(aes(x=model, y = NRMSE, fill = minbar), stat = "identity") +
  labs(x = "Model", title = "Error in aggregated predictions - adm1", y = "Normalized RMSE)") +
  theme_bw() +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#00BFC4", "grey38")) + 
  coord_flip() +
  facet_wrap(~variable) +
  guides(fill = "none")
ggsave("./gen/validation/audit/adm1-nrmse-byInd1.png", p1, width = 8, height = 10, dpi = 300)
p1 <- error_adm1 %>% 
  filter(variable != "ph_wtr_improve") %>%
  group_by(variable, model) %>%
  summarise(NRMSE = sqrt(mean(error^2))/(max(dir) - min(dir))) %>%
  group_by(variable) %>%
  mutate(minNRMSE = min(NRMSE)) %>%
  mutate(minbar = ifelse(NRMSE == minNRMSE, "min", "other")) %>%
  ggplot() +
  geom_bar(aes(x=model, y = NRMSE, fill = minbar), stat = "identity") +
  labs(x = "Model", title = "Error in aggregated predictions - adm1", y = "Normalized RMSE)") +
  theme_bw() +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#00BFC4", "grey38")) + 
  coord_flip() +
  facet_wrap(~variable) +
  guides(fill = "none")
ggsave("./gen/validation/audit/adm1-nrmse-byInd2.png", p1, width = 8, height = 10, dpi = 300)


# Plot normalized RMSE for sample of indicators
p2 <- error_adm1 %>% 
  filter(variable %in% c("ch_diar_ors", 
                         "rh_anc_toxinj",
                         "ph_sani_improve",
                         #"nt_wm_micro_iron_any", "ph_wtr_improve", "rh_anc_4vs"
                         "nt_ch_micro_vas")) %>%
  mutate(variable = case_when(
    variable == "ch_diar_ors" ~ "Children under 24m with diarrhea who received ORS",
    #variable == "nt_wm_micro_iron_any" ~ "Women took iron supplements during pregnancy",
    variable == "nt_ch_micro_vas" ~ "Children 6-23m given vitamin A supplements",
    #variable == "ph_wtr_improve" ~ "Household access to improved water source",
    variable == "ph_sani_improve" ~ "Household access to improved sanitation",
    variable == "rh_anc_toxinj" ~ "Women received 2+ tetanus shots during pregnancy",
    #variable == "rh_anc_4vs" ~ "Women attended 4+ ANC visits during pregnancy",
    TRUE ~ variable
  )) %>%
  mutate(model = case_when(
    model == "100-1" ~ "Baseline",
    model == "101-1" ~ "1 covariate",
    model == "102-1" ~ "2 covariates",
    model == "103-1" ~ "3 covariates",
    TRUE ~ model
  )) %>%
  mutate(model = factor(model, levels = c("Baseline", "1 covariate", "2 covariates", "3 covariates"))) %>%
  group_by(variable, model) %>%
  summarise(NRMSE = sqrt(mean(error^2))/(max(dir) - min(dir))) %>%
  group_by(variable) %>%
  mutate(minNRMSE = min(NRMSE)) %>%
  mutate(minbar = ifelse(NRMSE == minNRMSE, "min", "other")) %>%
  ggplot() +
  geom_bar(aes(x=model, y = NRMSE, fill = minbar), stat = "identity") +
  labs(x = "Model", title = "Error between aggregated adm2 predictions and direct adm1 estimates", y = "Normalized RMSE") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  scale_fill_manual(values = c("#00BFC4", "grey38")) + 
  coord_flip() +
  facet_wrap(~variable, nrow = 1, labeller = labeller(variable = label_wrap_gen(20))) +
  guides(fill = "none")
ggsave("./gen/validation/audit/adm1-nrmse-byInd-samp.png", p2, width = 7, height = 3.5, dpi = 500)

# Plot normalized RMSE for all indicators combined
p3 <- error_adm1 %>% 
  group_by(variable, model) %>%
  summarise(NRMSE = sqrt(mean(error^2))/(max(dir) - min(dir))) %>%
  group_by(model) %>%
  summarise(NRMSE = mean(NRMSE)) %>%
  mutate(minNRMSE = min(NRMSE)) %>%
  mutate(minbar = ifelse(NRMSE == minNRMSE, "min", "other")) %>%
  ggplot() +
  geom_bar(aes(x=model, y = NRMSE, fill = minbar), stat = "identity") +
  labs(x = "Model", title = "Error in aggregated predictions - adm1", y = "Normalized RMSE") +
  theme_bw() +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#00BFC4", "grey38")) + 
  coord_flip() +
  guides(fill = "none")
ggsave("./gen/validation/audit/adm1-nrmse-all.png", p3, width = 4, height = 5, dpi = 300)

# Plot normalized RMSE for indicator type
p4 <- error_adm1 %>% 
  group_by(variable, model) %>%
  summarise(NRMSE = sqrt(mean(error^2))/(max(dir) - min(dir))) %>%
  left_join(ind %>% select(variable, covar_grp)) %>%
  group_by(covar_grp, model) %>%
  summarise(NRMSE = mean(NRMSE)) %>%
  mutate(minNRMSE = min(NRMSE)) %>%
  mutate(minbar = ifelse(NRMSE == minNRMSE, "min", "other")) %>%
  ggplot() +
  geom_bar(aes(x=model, y = NRMSE, fill = minbar), stat = "identity") +
  labs(x = "Model", title = "Error in aggregated predictions - adm1", y = "Normalized RMSE") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  scale_fill_manual(values = c("#00BFC4", "grey38")) + 
  coord_flip() +
  facet_wrap(~covar_grp) +
  guides(fill = "none")
ggsave("./gen/validation/audit/adm1-nrmse-byCovarGrp.png", p4, width = 6, height = 5, dpi = 300)

# adm1 overlap ------------------------------------------------------------

# # limit to model with minimum rsme
# chosenmod_adm1 <- error_adm1 %>%
#   inner_join(minerror_adm1_mod, by = c("variable", "model"))
# # limit to model chosen using RMSE by covariate grouping in consultation with Maiga
# chosenmod_adm1 <- error_adm1 %>%
#   inner_join(ind %>%
#                select(variable, model_covar_grp) %>%
#                rename(model = model_covar_grp), by = c("variable", "model"))
# limit to model 103
chosenmod_adm1 <- error_adm1 %>%
  filter(model == "103-1")


# Plot overlap for model
plotdat <- bangladesh_1 %>% 
  left_join(chosenmod_adm1, by = "ADM1_EN")
for(i in 1:length(v_var)){
  myoutcome <- v_var[i]
  p2 <- ggplot() +
    geom_sf(data = subset(plotdat, variable == myoutcome), aes(fill = int_overlap)) +
    geom_sf(data = bangladesh_1, color = "black", fill = NA) +
    labs(title = myoutcome, subtitle = "Interval overlap") +
    scale_fill_manual(values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")) +
    theme_void() +
    theme(
      text = element_text(size = 10),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) 
  ggsave(paste0("./gen/validation/audit/adm1-overlap-", myoutcome, ".png"), p2, width = 12, height = 6)
}

# Plot uncertainty intervals
# direct and modeled
# chosen model
dat2 <- chosenmod_adm1 %>%
  select(variable, ADM1_EN, int_overlap, dir, lb_dir, ub_dir, agg, lb_agg, ub_agg) %>%
  pivot_longer(
    cols = c(dir, agg, lb_dir, lb_agg, ub_dir, ub_agg),
    names_to = "name",
    values_to = "val"
  ) %>%
  mutate(
    type = case_when(
      grepl("agg", name) ~ "agg",
      TRUE ~ "dir"
    ),
    measure = case_when(
      grepl("^lb", name) ~ "lb",
      grepl("^ub", name) ~ "ub",
      TRUE ~ "value"
    )
  ) %>%
  select(-name) %>%
  pivot_wider(
    names_from = measure,
    values_from = val
  ) %>%
  mutate(name = ifelse(type == "agg", "Aggregated adm2", "Direct")) %>%
  mutate(int_overlap = ifelse(int_overlap == TRUE, "Overlap", "No overlap"))

length(unique(dat2$variable)) # 31 indicators
nrow(subset(dat2, type == "dir")) # 248
nrow(subset(dat2, int_overlap == "Overlap" & name == "Aggregated adm2")) # 245 comparisons between adm1 and model
length(unique(subset(dat2, int_overlap == "No overlap")$variable)) # 2 variables with no overlap
subset(dat2, int_overlap == "No overlap")

p3 <- dat2 %>%
  ggplot(aes(x = ADM1_EN, y = value, color = name, group = name)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(aes(shape = int_overlap), position = position_dodge(width = 0.8)) +
  labs(x = "", y = "", title = "Comparison of direct adm1 and modeled-aggregated adm2") +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size = 10), legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_shape_manual(values = c(8, 16)) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~variable) 
ggsave(paste0("./gen/validation/audit/adm1-uncert-int.png"), p3, width = 10, height = 5.5, units = "in", dpi = 300)


p4 <- dat2 %>%
  filter(variable %in% c("ch_diar_ors", 
                         #"nt_wm_micro_iron_any",
                         #"ph_wtr_improve", "rh_anc_4vs"
                         "ph_sani_improve", "rh_anc_toxinj",
                         "nt_ch_micro_vas")) %>%
  mutate(variable = case_when(
    variable == "ch_diar_ors" ~ "Children under 24m with diarrhea who received ORS",
    #variable == "nt_wm_micro_iron_any" ~ "Women took iron supplements during pregnancy",
    variable == "nt_ch_micro_vas" ~ "Children 6-23m given vitamin A supplements",
    #variable == "ph_wtr_improve" ~ "Household access to improved water source",
    variable == "ph_sani_improve" ~ "Household access to improved sanitation",
    variable == "rh_anc_toxinj" ~ "Women received 2+ tetanus shots during pregnancy",
    #variable == "rh_anc_4vs" ~ "Women attended 4+ ANC visits during pregnancy",
    TRUE ~ variable
  )) %>%
  ggplot(aes(x = ADM1_EN, y = value*100, color = name, group = name)) +
  geom_errorbar(aes(ymin = lb*100, ymax = ub*100), position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(position = position_dodge(width = 0.8)) +
  #geom_point(aes(shape = int_overlap), position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c("#00BFC4", "grey38"),
                     guide = guide_legend(reverse = TRUE)) + 
  labs(x = "Adm1 division", y = "Prevalence", title = "Comparison of aggregated adm2 predictions and direct adm1 estimates") +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size = 10), legend.title=element_blank(), 
        #axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom") +
  scale_shape_manual(values = c(8, 16)) +
  facet_wrap(~variable, nrow = 1, labeller = labeller(variable = label_wrap_gen(20))) 
ggsave(paste0("./gen/validation/audit/adm1-uncert-int-samp.png"), p4, width = 7, height = 4, dpi = 500)


# adm0 rsme --------------------------------------------------------------------

# Plot RMSE
p1 <- error_adm0 %>% 
  group_by(variable, model) %>%
  mutate(n = n(),
         sqerror = (error)^2) %>%
  summarise(RMSE = mean(sqerror)) %>%
  group_by(variable) %>%
  mutate(minRMSE = min(RMSE)) %>%
  mutate(minbar = ifelse(RMSE == minRMSE, "min", "other")) %>%
  ggplot() +
  geom_bar(aes(x=model, y = RMSE, fill = minbar), stat = "identity") +
  labs(x = "model", title = "Error") +
  theme_bw() +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("red","grey38")) + 
  coord_flip() +
  facet_wrap(~variable, scales = "free_x") +
  guides(fill = "none")
ggsave(paste0("./gen/validation/audit/adm0-error-allmodels.png"), p1, width = 8, height = 10, units = "in", dpi = 300)

# adm0 overlap ------------------------------------------------------------

# # limit to model with minimum rsme
# chosenmod_adm0 <- error_adm0 %>%
#   inner_join(minerror_adm1_mod, by = c("variable", "model"))
# # limit to model chosen using RMSE by covariate grouping in consultation with Maiga
# chosenmod_adm0 <- error_adm0 %>%
#   inner_join(ind %>%
#                select(variable, model_covar_grp) %>%
#                rename(model = model_covar_grp), by = c("variable", "model"))
# limit to model 103
chosenmod_adm0 <- error_adm0 %>%
  filter(model == "103-1")

# overlap
table(chosenmod_adm0$int_overlap)

# Plot uncertainty intervals
dat3 <- chosenmod_adm0 %>%
  select(variable, ADM0_EN, int_overlap, dir, lb_dir, ub_dir, agg, lb_agg, ub_agg) %>%
  pivot_longer(
    cols = c(dir, agg, lb_dir, lb_agg, ub_dir, ub_agg),
    names_to = "name",
    values_to = "val"
  ) %>%
  mutate(
    type = case_when(
      grepl("agg", name) ~ "agg",
      TRUE ~ "dir"
    ),
    measure = case_when(
      grepl("^lb", name) ~ "lb",
      grepl("^ub", name) ~ "ub",
      TRUE ~ "value"
    )
  ) %>%
  select(-name) %>%
  pivot_wider(
    names_from = measure,
    values_from = val
  ) %>%
  mutate(name = ifelse(type == "agg", "Aggregated adm2", "Direct")) %>%
  mutate(int_overlap = ifelse(int_overlap == TRUE, "Overlap", "No overlap"))

p3 <- dat3 %>%
  ggplot(aes(x = variable, y = value, color = name, group = name)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(position = position_dodge(width = 0.8)) +
  labs(x = "", y = "") +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size = 10), legend.title=element_blank()) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE))
ggsave(paste0("./gen/validation/audit/adm0-uncert-int.png"), p3, width = 12, height = 10, units = "in", dpi = 300)


