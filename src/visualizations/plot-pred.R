################################################################################
#' @description Compare maps
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(ggplot2)
library(rgdal)
library(spdep)
#' Inputs
source("./src/util.R")
# predicted prevalence
pred <- read.csv("./gen/model/output/predictions.csv")
# direct
direct <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
# district shape files
AD2 <- readOGR(dsn="./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
AD2 <- st_as_sf(AD2)
################################################################################

# create confidence intervals for direct estimates
direct <- subset(direct, admin_level == "adm2")
names(direct)[which(names(direct) == "district_name")] <- "ADM2_EN"
direct <- direct[,c("ADM2_EN", "value", "lower", "upper", "indicator")]
direct$model <- "Direct"

names(pred)[which(names(pred) == "district")] <- "ADM2_EN"

# combine
dat <- rbind(direct, pred)

# merge with boundaries
AD2_merged <- left_join(AD2, dat, by = "ADM2_EN")

# plot maps
p1 <- ggplot() +
  geom_sf(data = AD2_merged, aes(fill = value), color = "black") +
  scale_fill_viridis_c(option = "magma", na.value = "gray80") + 
  theme_bw() +
  labs() +
  facet_grid(model~indicator)
ggsave(paste0("./gen/visualizations/output/map.png"), p1)

# Position dodge width
dodge_width <- 0.8  

# plot uncertainty
p2 <- ggplot(dat, aes(x = ADM2_EN, y = value, color = model, group = model)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = dodge_width), 
                width = 0.2) +  # Error bars
  geom_point(aes(shape = model), size = 1, position = position_dodge(width = dodge_width),) +  # Plot estimate points
  labs(title = "", x = "", y = "") +
  theme_bw() +
  coord_flip() +
  facet_wrap(~indicator) +
  theme(text = element_text(size = 10), legend.title=element_blank())
ggsave(paste0("./gen/visualizations/output/uncert-est.png"), p2, width = 10, height = 8)



