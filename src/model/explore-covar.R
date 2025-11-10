################################################################################
#' @description Test covariates
#' @return Model fit
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(dplyr)
library(tidyr)
library(here)
library(randomForest)
library(ggplot2)
library(readr)
library(readxl)
library(ggplot2)
library(car)
#' Inputs
source("./src/util.R")
# Direct estimates and variance of outcomes
est <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
# District-level covariates
covar <- read.csv("./gen/prepare-dhs/output/covar-district.csv")
# List of indicators
ind <- read_excel("./data/ind-info.xlsx")
################################################################################

# merge outcome variables with covariates
dat <- merge(est, covar, by = c("ADM2_EN", "variable"))

# subset to included indicators
df_ind <- subset(ind, status == "include")

# Covariate importance ----------------------------------------------------

# Fit a single random forest model using all the covariates specified.
# Perturb data after fitting model.
# IncMSE: Mean increase in prediction error (RMSE or MSE) when that variable’s values are randomly permuted across the dataset. 

df_res_imp <- data.frame()
for(i in 1:nrow(df_ind)){
  
  indi <- df_ind[i,]$variable
  
  print(paste0(i, ": ", indi))
  
  # subset to indicator
  df <- subset(dat, variable == indi)
  df <- df[complete.cases(df[, c("dir", "mother_edu", "mother_age", "residence", 
                                 "wealth_index", "hhd_under5", "hhd_head_age", 
                                 "hhd_head_sex", "child_age")]), ]
  
  # Test using step()
  #step(glm(dir ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_age + mother_edu + wealth_index, data = df), direction = "both")
  
  # Test using random forest with all covariates
  rf_model <- randomForest(dir ~ mother_edu + mother_age + residence + wealth_index + hhd_under5 + hhd_head_age + hhd_head_sex + child_age, data = df, importance = TRUE)
  # A larger decrease in accuracy (or increase in MSE) indicates higher importance for that variable
  df_importance <- as.data.frame(importance(rf_model, type = 1))
  names(df_importance)[1] <- "IncMSE"
  df_importance$covar <- row.names(df_importance)
  row.names(df_importance) <- NULL
  df_importance$variable <- indi
  
  df_res_imp <- rbind(df_res_imp, df_importance)
  
}

# merge on covariate groups
df_plot <- merge(df_res_imp, df_ind[,c("variable", "covar_grp")])
df_plot <- df_plot[order(df_plot$covar_grp, df_plot$variable, df_plot$IncMSE),]
df_plot <-  df_plot %>%
  mutate(covar_grp = case_when(
    covar_grp == "anc" ~ "ANC",
    covar_grp == "ch_nut" ~ "Child micronutrients",
    covar_grp == "ch_trtmnt" ~ "Child vaccinations and treatment",
    covar_grp == "del_pnc" ~ "Delivery and PNC",
    covar_grp == "ph" ~ "Household",
    covar_grp == "wm_micro" ~ "Women micronutrients",
    TRUE ~ NA
  )) %>%
  mutate(covar_grp = factor(covar_grp, 
                            levels = c("ANC", "Delivery and PNC",
                                       "Child micronutrients", "Child vaccinations and treatment",
                                       "Women micronutrients",
                                       "Household")
                            ))

# labels for included covariates
df_text <- df_plot %>%
  group_by(covar_grp) %>%
  distinct(variable) %>%
  summarise(variable = paste(variable, collapse = "\n")) %>%
  mutate(x = -Inf, y = Inf, hjust = -0.1, vjust = 1.1)

# plot importance by group
p1 <- df_plot %>%
  ggplot() +
  geom_boxplot(aes(x=covar, y = IncMSE), outlier.shape = NA) +
  geom_hline(aes(yintercept = 0), color = "red") +
  geom_text(data = df_text,
            aes(x = x, y = y, label = variable, hjust = hjust, vjust = vjust),
            inherit.aes = FALSE, size = 3) +
  labs(title = "Covariate importance by group", y = "Importance") +
  facet_wrap(~covar_grp) +
  coord_cartesian(ylim = c(-5, 15)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0("./gen/model/explore-covar/importance_", format(Sys.Date(), "%Y%m%d"), ".png"), p1, width = 10, height = 7.5, units = "in", dpi = 300)

# Variance inflation ------------------------------------------------------

i <- 1
indi <- df_ind[i,]$variable
df <- subset(dat, variable == indi)
df <- df[complete.cases(df[, c("dir", "mother_edu", "mother_age", "residence", 
                               "wealth_index", "hhd_under5", "hhd_head_age", 
                               "hhd_head_sex", "child_age")]), ]
nrow(dat)
nrow(df) # 64, all districts have values
mod_vif <- car::vif(lm(dir ~ mother_edu + mother_age + residence + wealth_index + hhd_under5 + hhd_head_age + hhd_head_sex + child_age, data = df))
df_vif <- as.data.frame(mod_vif)
names(df_vif)[1] <- "VIF"
df_vif$covar <- row.names(df_vif)

p2 <- df_vif %>%
  ggplot() +
  geom_bar(aes(x=covar, y = VIF), stat = "identity") +
  labs(title = "Variance inflation factor", y = "VIF") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0("./gen/model/explore-covar/vif_", format(Sys.Date(), "%Y%m%d"), ".png"), p2, width = 5, height = 5, units = "in", dpi = 300)


# Correlation -------------------------------------------------------------

i <- 1
indi <- df_ind[i,]$variable
df <- subset(dat, variable == indi)
df <- df[, c("mother_edu", "mother_age", "residence", 
       "wealth_index", "hhd_under5", "hhd_head_age", 
       "hhd_head_sex", "child_age")]
df <- df[complete.cases(df), ]

cor_matrix <- cor(df, use = "pairwise.complete.obs")
reg <- function(x, y){
  points(x,y, col="lightblue")
  abline(lm(y~x), col="blue")
  abline(v=0, h=0, col="red", lty=3)
}  

panel.cor <- function(x, y, digits = 2, min_cex = 1, max_cex = 4) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  r <- cor(x, y, use = "complete.obs")
  txt <- formatC(r, digits, format = "f")
  
  # Scale cex based on |r|
  cex <- min_cex + (max_cex - min_cex) * abs(r)
  
  text(0.5, 0.5, txt, cex = cex)
}

# # Custom panel for diagonal with wrapped variable names
panel.diag.wrap <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))

  var_name <- deparse(substitute(x))  # Will be overwritten, workaround below
}
# wrapping labels in column names
wrapped_names <- sapply(colnames(df), function(name) {
  paste(strwrap(name, width = 10), collapse = "\n")
})
colnames(df) <- wrapped_names
# Plot with wrapping
png(paste0("./gen/model/explore-covar/corr_", format(Sys.Date(), "%Y%m%d"),".png"), width = 800, height = 800)
pairs(df,
      labels = rep("", ncol(df)),  # suppress top labels
      lower.panel = reg,
      upper.panel = panel.cor,
      diag.panel = function(x, ...) {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        i <- which(sapply(df, identical, x))  # get index
        text(0.5, 0.5, wrapped_names[i], cex = 1.2)
      }
)
dev.off()


