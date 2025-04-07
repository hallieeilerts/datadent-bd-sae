################################################################################
#' @description Check for collinearity for mother-level
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(car)
#' Inputs
source("./src/util.R")
dhs_data <- read.csv("./gen/prepare-dhs/output/dat-mother.csv")
################################################################################

# convert character covariates
dhs_data$residence[dhs_data$residence == "urban"] <- 1
dhs_data$residence[dhs_data$residence == "rural"] <- 2
dhs_data$residence <- as.numeric(dhs_data$residence)

outcome <- c("nt_wm_micro_iron", "rh_anc_4vs", "rh_anc_1tri")
vcov <- c("residence",
          "hhd_under5", "hhd_head_sex", "hhd_head_age",
          "mother_edu",
          "wealth_index")

# nt_wm_micro_iron --------------------------------------------------------

# convert outcomes to 0 or 1
dhs_data$nt_wm_micro_iron[dhs_data$nt_wm_micro_iron == "Yes"] <- 1
dhs_data$nt_wm_micro_iron[dhs_data$nt_wm_micro_iron == "No"]  <- 0
dhs_data$nt_wm_micro_iron <- as.numeric(dhs_data$nt_wm_micro_iron)

dat <- dhs_data[,c("nt_wm_micro_iron", "residence", "hhd_under5", "hhd_head_sex", "hhd_head_age",
                   "mother_edu","wealth_index")]
dat <- dat[complete.cases(dat), ]

# Variance inflation factor
# A VIF above 5 (sometimes 10) suggests strong collinearity
vif(lm(nt_wm_micro_iron ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dat))

# Pearson correlation to check for high correlations (above 0.7)
cor_mat <- cor(dat, use="complete.obs")
which(cor_mat > 0.7)
which(cor_mat < -0.7)
cor_mat[which(cor_mat > 0.7)]
# the only high ones are the diagonals
cor_mat[cor_mat == 1] <- NA
cor_mat[upper.tri(cor_mat)] <- NA

# Convert matrix to a data frame
mat_df <- reshape2::melt(cor_mat)  # Converts to long format

# Define a custom color scale with multiple breakpoints
neg_color <- viridis(10, option = "B")[2]  # Dark blue for negative values
zero_color <- "white"                      # White for zero
pos_color <- viridis(10, option = "B")[9]  # Yellow for positive values

# Plot using ggplot2
p0 <- ggplot(mat_df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = neg_color, mid = zero_color, high = pos_color, midpoint = 0) +
  theme_bw() +
  labs(fill = "Pearson cor.", x= "", y = "") +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 90))
ggsave(paste0("./gen/prepare-model/output/correlation-mother-iron.png"), p0, width = 6, height = 6)

# rh_anc4vs ---------------------------------------------------------------

# convert outcomes to 0 or 1
dhs_data$rh_anc_4vs[dhs_data$rh_anc_4vs == "Yes"] <- 1
dhs_data$rh_anc_4vs[dhs_data$rh_anc_4vs == "No"]  <- 0
dhs_data$rh_anc_4vs <- as.numeric(dhs_data$rh_anc_4vs)

dat <- dhs_data[,c("rh_anc_4vs", "residence", "hhd_under5", "hhd_head_sex", "hhd_head_age",
                   "mother_edu","wealth_index")]
dat <- dat[complete.cases(dat), ]

# Variance inflation factor
# A VIF above 5 (sometimes 10) suggests strong collinearity
vif(lm(rh_anc_4vs ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dat))

# Pearson correlation to check for high correlations (above 0.7)
cor_mat <- cor(dat, use="complete.obs")
which(cor_mat > 0.7)
which(cor_mat < -0.7)
cor_mat[which(cor_mat > 0.7)]
# the only high ones are the diagonals


# rh_anc4vs ---------------------------------------------------------------

# convert outcomes to 0 or 1
dhs_data$rh_anc_1tri[dhs_data$rh_anc_1tri == "Yes"] <- 1
dhs_data$rh_anc_1tri[dhs_data$rh_anc_1tri == "No"]  <- 0
dhs_data$rh_anc_1tri <- as.numeric(dhs_data$rh_anc_1tri)

dat <- dhs_data[,c("rh_anc_1tri", "residence", "hhd_under5", "hhd_head_sex", "hhd_head_age",
                   "mother_edu","wealth_index")]
dat <- dat[complete.cases(dat), ]

# Variance inflation factor
# A VIF above 5 (sometimes 10) suggests strong collinearity
vif(lm(rh_anc_1tri ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dat))

# Pearson correlation to check for high correlations (above 0.7)
cor_mat <- cor(dat, use="complete.obs")
which(cor_mat > 0.7)
which(cor_mat < -0.7)
cor_mat[which(cor_mat > 0.7)]
# the only high ones are the diagonals

