################################################################################
#' @description Create map for composite coverage indicator for sept 2025 presentation
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(sf)
library(dplyr)
library(tidyr)
library(wesanderson)
library(ggplot2)
library(tmap)
library(readxl)
#' Inputs
source("./src/util.R")
# Bangladesh district boundaries
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
compcov <- read_excel("./data/composite-coverage/composite coverage analysis figures, Bangladesh divisions.xlsx", sheet = "anemia - modelled")
################################################################################

dat <- compcov %>%
  filter(strate == "District") %>%
  select(level, cci_r) %>%
  rename(ADM2_EN = level) %>%
  mutate(ADM2_EN = case_when(
    ADM2_EN == "Barishal" ~ "Barisal",
    ADM2_EN == "Bogura" ~ "Bogra",
    ADM2_EN == "Brahmanbaria" ~  "Brahamanbaria",
    ADM2_EN == "Chapai Nawabganj" ~ "Nawabganj",
    ADM2_EN == "Chattogram" ~ "Chittagong" ,
    ADM2_EN == "Chuadanga" ~ "Chuadanga",
    ADM2_EN == "Cumilla" ~ "Comilla",
    ADM2_EN == "Jashore"  ~ "Jessore",
    ADM2_EN == "Kishoregonj"  ~ "Kishoreganj",
    ADM2_EN == "Narayangonj" ~ "Narayanganj",
    ADM2_EN == "Netrokona" ~ "Netrakona",
    TRUE ~ ADM2_EN
  ))

sort(unique(dat$ADM2_EN))[sort(unique(dat$ADM2_EN)) != sort(unique(bangladesh_2$ADM2_EN))]

# highlight these districts where One Nutrition Coverage Survey took place
v_highlight <- c("Sylhet", "Rangpur", "Khulna", "Dhaka")

# Join spatial data
plotdat <- bangladesh_2 %>% 
  left_join(dat, by = "ADM2_EN")

# subset spatial centroids for highlighted districts
highlight_pts <- plotdat %>%
  filter(ADM2_EN %in% v_highlight) %>%
  st_centroid()
# Nudge Dhaka east by 0.4 degrees longitude
highlight_pts$geometry[highlight_pts$ADM2_EN == "Dhaka"] <-
  highlight_pts$geometry[highlight_pts$ADM2_EN == "Dhaka"] + c(0.1, 0)
# Nudge Khulna north
highlight_pts$geometry[highlight_pts$ADM2_EN == "Khulna"] <-
  highlight_pts$geometry[highlight_pts$ADM2_EN == "Khulna"] + c(0, 0.2)

p <- ggplot() +
  geom_sf(data = plotdat, aes(fill = cci_r), color = "black") +
  geom_sf(data = highlight_pts, shape = 18, size = 3) +
  scale_fill_gradientn(
    colors = rev(wes_palette("Zissou1", 100, type = "continuous")),
    limits = c(min(plotdat$cci_r, na.rm = TRUE), max(plotdat$cci_r, na.rm = TRUE)),  # adjust as needed
    na.value = "grey80", name = ""
  ) +
  labs(title = "Anemia composite coverage") +
  theme_void() +
  theme(text = element_text(size = 10))

# Plot for datadent presentation Sept 2025 on wide slides
ggsave(paste0("./gen/visualizations/compcov-anemia.png"), p, width = 5, height = 5)
