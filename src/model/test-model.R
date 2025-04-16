################################################################################
#' @description Testing model for iron supplementation
#' @return plots
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
#' Inputs
source("./src/util.R")
# direct estimates and variance
est <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
# Bangladesh district boundaries
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
# adjacency matrix
prep <- readRDS("./gen/prepare-shp/output/adjacency_matrix.rds")
################################################################################

# subset indicator
ind_dist <- subset(est, variable == "nt_wm_micro_iron")

# Choose model to run, 'BYM2' for area-level variance smoothing model, and 'binom_BYM2' for area/cluster level binomial model 
model_name = "BYM2" # "binom_BYM2"

# Give adm2_index
bangladesh_2$district_id <- 1:nrow(bangladesh_2)

# Join spatial data
ind_dist <- bangladesh_2 %>% 
  left_join(ind_dist, by = "ADM2_EN")

### removing all LGAs with only one EA 
ind_dist_complete <- ind_dist |> filter(!is.na(dir_var),degf!=0,dir_var>1e-10) 
stanfile <- here('src/model/',paste0('areal_level_',model_name,'.stan')) # For Binomial model, "areal_level_binom_BYM2.stan"
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
  iter_warmup = 500,
  iter_sampling = 500,
  save_warmup = FALSE,
  chains = 2,
  parallel_chains = 2,
  refresh = 50 # print update every 50 iters
)

post_samples = fit$draws(format = "df",  inc_warmup = F)
post_samples$chain = as.character(post_samples$.chain)
colnames(post_samples) <- colnames(post_samples) %>%
  gsub("\\[", "_", .) %>%
  gsub("\\]", "", .) %>%
  gsub(",","_", .)


for (param in colnames(post_samples)[2:10]){
  dat = post_samples#[post_samples$chain==2,]
  gg = ggplot(data=dat, aes_string(x=".iteration", y = param, color="chain"))+
    geom_line() + theme_minimal() 
  print(gg)
}

df_p = fit$draws(format = "df", variables=c('p'),  inc_warmup = F) |> mutate(chain=as.character(.chain)) 
df_v = fit$draws(format = "df", variables=c('v'),  inc_warmup = F) |> mutate(chain=as.character(.chain))

colnames(df_p) <- colnames(df_p) %>%
  gsub("\\[", "", .) %>%
  gsub("\\]", "", .) %>%
  gsub(",","_", .)
colnames(df_v) <- colnames(df_v) %>%
  gsub("\\[", "", .) %>%
  gsub("\\]", "", .) %>%
  gsub(",","_", .)

for (param in colnames(df_p)[1:10]){
  dat = df_p
  gg = ggplot(data=dat, aes_string(x=".iteration", y = param, color="chain"))+
    geom_line() + theme_minimal() 
  print(gg)
}
for (param in colnames(df_v)[1:10]){
  dat = df_v
  gg = ggplot(data=dat, aes_string(x=".iteration", y = param, color="chain"))+
    geom_line() + theme_minimal() 
  print(gg)
}


ind_dist$post_mean <- df_p[,1:nrow(ind_dist)] |> colMeans() |> unname()
ind_dist$post_var <-  sapply(df_p[,1:nrow(ind_dist)],var) |> unname()

ind_dist$naive <- ind_dist$naive * 100
ind_dist$dir <- ind_dist$dir * 100
ind_dist$post_mean <- ind_dist$post_mean * 100

# what if i get uncertainty intervals from posterior distribution and not using predicted variance?
ind_dist$post_lb <- df_p[, 1:nrow(ind_dist)] |> apply(2, quantile, probs = 0.025) |>  unname()
ind_dist$post_ub <- df_p[, 1:nrow(ind_dist)] |> apply(2, quantile, probs = 0.975) |>  unname()
ind_dist$post_lb <- ind_dist$post_lb * 100
ind_dist$post_ub <- ind_dist$post_ub * 100

# plot prevalence ---------------------------------------------------------


plot_map <- function(data, col, title, metric, level) {
  
  # Create a map: 
  map <- tm_shape(data) + 
    tm_fill(col = col,
            title = metric, 
            style = "cont",
            breaks = seq(0, 90, by = 10),
            textNA = "Missing Data",
            legend.is.portrait = F,
            palette = wesanderson::wes_palette("Zissou1Continuous")) + 
    tm_layout(main.title = title, frame = F, main.title.size = 0.8, 
              main.title.position = "center", legend.outside.position = "bottom",
              legend.outside.size = 0.35) +
    tm_borders(lwd = 0) + 
    tm_legend(show = T) +
    tm_shape(bangladesh_2) +
    tm_borders(col = "black", lwd = 0.8)
  
  return(map)
}

naive_map <- plot_map(data = ind_dist,
                      col = "naive",
                      title = "iron (naive estimates)",
                      metric = "(%)",
                      level = "district")
naive_map
tmap_save(naive_map, "gen/maps/iron_naive.png", width = 8, height = 8,
          units = "in", dpi = 600)

direct_map <- plot_map(data = ind_dist , 
                       col = "dir", 
                       title = "iron (design-based estimates)", 
                       metric = "(%)", 
                       level = "district")
#direct_map
tmap_save(direct_map, "gen/maps/iron_direct.png", width = 8, height = 8,
          units = "in", dpi = 600)

FB_map <- plot_map(data = ind_dist ,
                   col = "post_mean", 
                   title = "iron (Full-Bayes BYM2 smoothed estimates)", 
                   metric = "(%)", 
                   level = "district")

#FB_map
tmap_save(FB_map, here("gen","maps",paste0("iron_FB_",model_name,".png")), width = 8, height = 8,
          units = "in", dpi = 600)


# plot variance -----------------------------------------------------------

plot_map <- function(data, col, title, metric, level) {
  
  # Create a map: 
  map <- tm_shape(data) + 
    tm_fill(col = col,
            title = metric, 
            style = "cont",
            breaks = seq(0, 0.035, by = 0.01),
            textNA = "Missing Data",
            legend.is.portrait = F,
            palette = viridis::viridis(10)) + 
    tm_layout(main.title = title, frame = F, main.title.size = 0.8, 
              main.title.position = "center", legend.outside.position = "bottom",
              legend.outside.size = 0.35) +
    tm_borders(lwd = 0) + 
    tm_legend(show = T) +
    tm_shape(bangladesh_2) +
    tm_borders(col = "black", lwd = 0.8)
  
  return(map)
}

# Variance of direct estimates: 
var_direct <- plot_map(data = ind_dist, 
                            col = "dir_var", 
                            title = "Variance of direct HT estimates", 
                            metric = "Variance", 
                            level = "district")

# var_direct
tmap_save(var_direct, "gen/maps/iron_direct_var.png", width = 8, height = 8,
          units = "in", dpi = 600)

# Variance of smoothed estimates: 
var_smoothed_FB <- plot_map(data = ind_dist, 
                                 col = "post_var", 
                                 title = "Variance of smoothed estimates", 
                                 metric = "Variance", 
                                 level = "district")

# var_smoothed_FB
tmap_save(var_smoothed_FB, "gen/maps/iron_FB_BYM2_var.png", width = 8, height = 8,
          units = "in", dpi = 600)

# dot plots ---------------------------------------------------------------

est <- data.frame(district = ind_dist$ADM2_EN,
                  dir = ind_dist$dir,
                  pred = ind_dist$post_mean)
est %>%
  pivot_longer(
    cols = -district,
  ) %>%
  ggplot() +
  geom_point(aes(x=district, y = value, col = name, shape = name)) +
  coord_flip()


var <- data.frame(district = ind_dist$ADM2_EN,
                  dir = ind_dist$dir_var,
                  pred = ind_dist$post_var)
var %>%
  pivot_longer(
    cols = -district,
  ) %>%
  ggplot() +
  geom_point(aes(x=district, y = value, col = name, shape = name)) +
  coord_flip()

ci <- data.frame(district = ind_dist$ADM2_EN,
                 dir = ind_dist$dir,
                 pred = ind_dist$post_mean,
                 dir_var = ind_dist$dir_var,
                 pred_var = ind_dist$post_var)
ci$dir_se <- sqrt(ci$dir_var) * 100
ci$pred_se <- sqrt(ci$pred_var) * 100

ci_long <- ci %>%
  pivot_longer(
    cols = c(dir,pred),
  ) %>%
  mutate(lb = ifelse(name == "dir", value - 1.96*dir_se, value - 1.96*pred_se),
         ub = ifelse(name == "dir", value + 1.96*dir_se, value + 1.96*pred_se))

# add uncertainty intervals from posterior distribution (not predicted variance)
add_pred <- data.frame(name = "pred-quant",
           district = ind_dist$ADM2_EN,
           value = ind_dist$post_mean,
           lb = ind_dist$post_lb,
           ub = ind_dist$post_ub)
ci_long <- bind_rows(ci_long, add_pred)

ci_long %>%
  ggplot(aes(x = district, y = value, color = name, group = name)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(position = position_dodge(width = 0.8)) +
  labs(x = "", y = "") +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size = 10), legend.title=element_blank())


