################################################################################
#' @description Fit model using summer
#' @return Model fit
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(sae)
library(SUMMER)
library(survey)
library(srvyr)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
if (!isTRUE(requireNamespace("INLA", quietly = TRUE))) {
  install.packages("INLA", repos=c(getOption("repos"), 
                                   INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
}
#' Inputs
source("./src/util.R")
# Data
#dhs <- read.csv("./gen/prepare-dhs/output/dat-mother.csv")
dhs <- read.csv("./gen/prepare-dhs/output/dat-child.csv")
# Direct estimates and variance
est <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
# Bangladesh district boundaries
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
# Adjacency matrix
prep <- readRDS("./gen/prepare-shp/output/adjacency_matrix.rds")
################################################################################

# https://richardli.github.io/SUMMER/articles/web_only/small-area-estimation.html

unique(est$variable)
outcome <- "nt_ch_micro_dwm"

# direct estimates

direct <- subset(est, variable == outcome)

# survey design

design <- dhs %>% as_survey_design(ids = "v001", # psu
                                   strata = "v023", # strata for sampling
                                   weights = "wt",
                                   nest = TRUE)

# adjacency matrix

mat <- getAmat(bangladesh_2, bangladesh_2$ADM2_EN)

# use sae to smooth the logit-transformed direct estimates.

direct$logit.diab2 <- SUMMER::logit(direct$dir)
direct$logit.var <- direct$dir_var / (direct$dir ^ 2 * (1 - direct$dir) ^ 2)
SFH.brfss <- sae::mseSFH(logit.diab2 ~ 1, logit.var, mat, data = direct)
results <- data.frame(domain = direct$ADM2_EN,
                      eblup.SFH = SUMMER::expit(SFH.brfss$est$eblup), 
                      mse = SFH.brfss$mse)




# we fit two versions of the spatial area levelmodel in SUMMER

summer.brfss <- smoothArea(nt_ch_micro_dwm ~ child_age, 
                           domain= ~ADM2_EN,
                           design = design,
                           transform = "logit",
                           adj.mat = mat, level = 0.95)
summer.brfss.alt <- smoothArea(nt_ch_micro_dwm~1, 
                               domain= ~ADM2_EN,
                               design = design,
                               transform = "logit",
                               adj.mat = mat, level = 0.95,
                               pc.u = 0.1, pc.alpha = 0.01)

# create map plots
toplot <-  summer.brfss$bym2.model.est
toplot$logit.var <- toplot$var / 
  (summer.brfss$bym2.model.est$median ^ 2 * 
     (1 - summer.brfss$bym2.model.est$median) ^ 2)
toplot$median.alt <-  summer.brfss.alt$bym2.model.est$median
toplot$logit.var.alt <-  summer.brfss.alt$bym2.model.est$var / 
  (summer.brfss.alt$bym2.model.est$median ^ 2 *
     (1 - summer.brfss.alt$bym2.model.est$median) ^ 2)
toplot$median.sae <- results$eblup.SFH
toplot$mse.sae <- results$mse
variables <- c("median", "median.alt",  "median.sae",
               "logit.var", "logit.var.alt", "mse.sae")
names <- c("Median (default prior)", "Median (new prior)",  "EBLUP (sae)",
           "Variance (default prior)", "Variance (new prior)", "MSE (sae)")
# mapPlot(data = toplot, geo = bangladesh_2,
#         variables=variables[1:3], 
#         labels = names[1:3], by.data = "domain",
#         by.geo = "ADM2_EN", size = 0.1) 
# mapPlot(data = toplot, geo = bangladesh_2,
#         variables=variables[4:6], labels = names[4:6],
#         by.data = "domain", by.geo = "ADM2_EN", size = 0.1) 


df_plot <- rbind(summer.brfss$direct.est,
                 summer.brfss$bym2.model.est)
df_plot$method[df_plot$method == "Direct"] <- "1 - direct"
df_plot$method[df_plot$method == "Area level model: BYM2"] <- "2 - area level model: BYM2" 
p <- df_plot %>%
  ggplot(aes(x = domain, y = mean, color = method, group = method)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(position = position_dodge(width = 0.8)) +
  labs(x = "", y = "") +
  theme_bw() +
  coord_flip() +
  labs(title = outcome) +
  theme(text = element_text(size = 10), legend.title=element_blank()) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) 
ggsave(paste0("./gen/visualizations/uncert-int/", outcome,"-summer.png"), p, width = 8, height = 15, limitsize = F)
