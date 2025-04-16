################################################################################
#' @description Plot indicators separately
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(ggplot2)
library(dplyr)
library(viridis)
#' Inputs
source("./src/util.R")
# model names for plots
modinfo <- read.csv("./gen/model/audit/model-info.csv")
modinfoWide <- read.csv("./gen/model/audit/model-info-wide.csv")
# fit info
diag <- read.csv("./gen/model/audit/diagnostics.csv")
diagWide <- read.csv("./gen/model/audit/diagnostics-wide.csv")
################################################################################



# DEPRECATED
#' this is not how calculating elpd works
#' the se diff is not simply the difference in SEs






# merge on model info
df_plot <- merge(diag, modinfoWide, by = c("file", "outcome"))
df_plot$plotlabel <- paste0(df_plot$vers, "-", df_plot$test)

df_plot <- df_plot %>% filter(outcome == "nt_wm_micro_iron")
df_plot <- df_plot %>% filter(metric == "looic")

estimates <- df_plot$Estimate
se <- df_plot$SE
ratio_matrix <- outer(estimates, estimates, "-")
ratio_matrix <- ratio_matrix/2
sedif_matrix <- outer(se, se, "-")
ratio <- ratio_matrix/sedif_matrix
ratio[upper.tri(ratio)] <- NA
ratio

filenames <- df_plot$file
filenames <- gsub( "ModelOutputMother-Iron_", "", filenames)
filenames <- sub("_(?:[^_]*)$", "", filenames)
colnames(ratio) <- filenames 
row.names(ratio) <- filenames 

# Convert matrix to a data frame
mat_df <- reshape2::melt(ratio)  # Converts to long format

# Define a custom color scale with multiple breakpoints
custom_colors <- viridis(5, option = "B")  # Get 5 colors from the Viridis palette
range(mat_df$value, na.rm = TRUE)

# ELPD_diff = (LOOIC_ModelA - LOOIC_ModelB) / 2
p0 <- ggplot(mat_df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  # scale_fill_gradientn(
  #   colors = c(custom_colors[1], custom_colors[2], "white", custom_colors[4], custom_colors[5]), 
  #   values = scales::rescale(c(-90, -4, 0, 4, 90)),  # Define breakpoints
  #   limits = c(-90, 90)  # Ensure full range of values is covered
  # ) +
  theme_bw() +
  labs(x = "", y = "", fill = "ELPD_diff", title = "mother-iron") +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 90))
ggsave(paste0("./gen/model/temp/elpd-diff_mother-iron.png"), p0, width = 6, height = 6)

# Read this as...
# Row is better (blue, negative) than column
# Row is worse (red, positive) than column
subset(mat_df, Var1 == "008-Test1" & Var2 == "006-Test1")
# purple is negative as close to one (-2.722)
# everything is worse than model 003. unweighted and weighted plus district-level random effects
