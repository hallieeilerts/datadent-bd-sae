
#' Clear environment
rm(list = ls())

# choose indicator
# "nt_ch_micro_vas"  "nt_ch_micro_dwm"  "nt_ebf" "nt_wm_micro_iron" "rh_anc_4vs" "rh_anc_1tri" 
outcome <- "nt_ch_micro_vas"

# choose model
vers <- "002"
test <- "Test1"
model_name <- "ArealBYM2"
file_name <- paste(model_name, outcome, vers, test, sep = "-")

# read rds file that contains names of stan csv files
stan_csv_filenames <- readRDS(here("gen", "model", "fit", paste0("csv_files-fit-", file_name, ".rds")))

# read stan csv files as cmdstan object
stanfit <- as_cmdstan_fit(stan_csv_filenames)

# direct estiamtes
est <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
est_adm1 <- read.csv("./gen/calculate-direct/audit/direct-estimates-adm1.csv")

# Bangladesh district boundaries
bangladesh_1 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm1_bbs_20201113")
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")


# subset indicator from direct estimates
ind_dat <- subset(est, variable == outcome)
ind_dat <- ind_dat[order(ind_dat$ADM2_EN),]
ind_dist <- bangladesh_2  %>% 
  left_join(ind_dat, by = c("ADM2_EN"))

ind_dat <- subset(est_adm1, variable == outcome)
ind_dat <- ind_dat[order(ind_dat$ADM1_EN),]
ind_state <- bangladesh_1 %>% 
  left_join(ind_dat, by = c("ADM1_EN"))



# Model estimates 
df_p = stanfit$draws(format = "df", variables=c('p'),  inc_warmup = F) |> mutate(chain=as.character(.chain)) 
colnames(df_p) <- colnames(df_p) %>%
  gsub("\\[", "", .) %>%
  gsub("\\]", "", .) %>%
  gsub(",","_", .)
# --- compute mean and credible interval district level -----
alpha = 0.05
postpred <- ind_dat
postpred$post_mean <- df_p[,1:nrow(postpred)] |> colMeans() |> unname()
postpred$post_var <-  sapply(df_p[,1:nrow(postpred)], var) |> unname()
df_quantile =  apply(df_p[,1:nrow(postpred)], 2 , quantile , probs = c(alpha/2,1-alpha/2) , na.rm = TRUE ) |> t()
postpred$qt_lb <- df_quantile[, 1]
postpred$qt_ub <- df_quantile[, 2]
postpred$dir <- postpred$dir * 100
postpred$post_mean <- postpred$post_mean * 100

# Coverage 
#postpred$dir_lower90_wilson <- wilson_lower(postpred$dir, postpred$n_eff, alpha = alpha)

#--------------------------------------------------------------------------------
# --- State level Validation ----------------------------------------------------
# compute mean and credible interval at state level
# lga_pop <- pop_estimates |> group_by(state) |> 
#   reframe(lga = lga,pop_lga = population,pop_state = sum(pop_lga))  |>
#   mutate(geo_weight = pop_lga/pop_state) 
#--------------------------------------------------------------------------------

df_post <- cbind(ind_dist |> select(ADM1_EN, ADM2_EN, sum_wgt),
                 t(df_p[,1:nrow(ind_dist)])) 

df_post_state <- df_post |> as.data.frame() |>
  select(-geometry) |>
  group_by(ADM1_EN) |>
  summarise(across(all_of(colnames(df_post)[grepl('X',colnames(df_post))]), ~ weighted.mean(., w = sum_wgt)))

df_post_state$post_mean <- df_post_state[,2:(nrow(df_p)+1)] |> rowMeans()
df_quantile =  apply(df_post_state[,2:nrow(df_p)], 1 , quantile , probs = c(alpha/2,1-alpha/2) , na.rm = TRUE ) |> t()
df_post_state$qt_lb <- df_quantile[,1]
df_post_state$qt_ub <- df_quantile[,2]
df_post_state$ci_length95 <- df_post_state$qt_lb - df_post_state$qt_ub

ind_state <- ind_state |> left_join((df_post_state|> select(-colnames(df_post)[grepl('X',colnames(df_post))])), by = 'ADM1_EN')


#vb12_inad_state <- vb12_inad_state |> left_join(nigeria_1|> select(state, geometry), by='state')
ind_state <- st_as_sf(ind_state)
ggplot(vb12_inad_state |> st_as_sf()
           , aes(fill =dir)) +
  geom_sf() +
  labs(title='Full Bayes estimate') +
  theme_minimal(base_size = 14) 
