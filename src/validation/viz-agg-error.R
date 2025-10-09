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

# temporary: remove ones that i haven't run models for yet
# v_var <- v_var[!(v_var %in% c("rh_anc_toxinj", "rh_anc_neotet"))]

# adm1 --------------------------------------------------------------------

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
ggsave(paste0("./gen/validation/audit/adm1-error-allmodels.png"), p1, width = 8, height = 10, units = "in", dpi = 300)

# Plot RMSE for all indicators combined
error_adm1 %>% 
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

# Plot RMSE for indicator type
error_adm1 %>% 
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

# Plot normalized RMSE
error_adm1 %>% 
  group_by(variable) %>%
  mutate(error_normalized = (error - min(error))/(max(error) - min(error)),
         sqerror = (error_normalized)^2) %>%
  group_by(variable, model) %>%
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
  facet_wrap(~variable, scales = "free_x") +
  guides(fill = "none")

# Plot normalized RMSE for all indicators combined
error_adm1 %>% 
  group_by(variable) %>%
  mutate(error_normalized = (error - min(error))/(max(error) - min(error)),
         sqerror = (error_normalized)^2) %>%
  group_by(model) %>%
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

# Plot normalized RMSE for indicator type
error_adm1 %>% 
  group_by(variable) %>%
  mutate(error_normalized = (error - min(error))/(max(error) - min(error)),
         sqerror = (error_normalized)^2) %>%
  left_join(ind %>% select(variable, covar_grp)) %>%
  group_by(covar_grp, model) %>%
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
  facet_wrap(~covar_grp, scales = "free_x") +
  guides(fill = "none")


# # limit to model with minimum error
# minerror_adm1 <- error_adm1 %>% 
#   inner_join(minerror_adm1_mod, by = c("variable", "model"))
# limit to chosen model
minerror_adm1 <- error_adm1 %>% 
  inner_join(modinfo %>%
               select(variable, model_covar_grp) %>%
               rename(model = model_covar_grp), by = c("variable", "model"))

# Plot overlap for model with minimum error
plotdat <- bangladesh_1 %>% 
  left_join(minerror_adm1, by = "ADM1_EN")
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
# for model with minimum error
dat2 <- minerror_adm1 %>%
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

nrow(subset(dat2, int_overlap == "Overlap" & name == "Aggregated adm2")) # 245
length(unique(subset(dat2, int_overlap == "No overlap")$variable)) # 2
length(unique(dat2$variable)) # 31

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

# adm0 --------------------------------------------------------------------

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

# # limit to model with minimum error
# minerror_adm0 <- error_adm0 %>% 
#   inner_join(minerror_adm1_mod, by = c("variable", "model"))
# limit to chosen model
minerror_adm0 <- error_adm0 %>% 
  inner_join(modinfo %>%
               select(variable, model_covar_grp) %>%
               rename(model = model_covar_grp), by = c("variable", "model"))

# overlap
table(minerror_adm0$int_overlap)

# Plot uncertainty intervals
dat3 <- minerror_adm0 %>%
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


