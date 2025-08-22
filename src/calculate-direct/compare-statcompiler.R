################################################################################
#' @description Compare calculated national-level indicators to those from StatCompiler to make sure code is correct
#' @return Plot with comparisons between manually calculated indicators and Statcompiler values
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
#' Inputs
source("./src/util.R")
# Manually calculated indicators
direct <- read.csv("./gen/calculate-direct/audit/direct-estimates-adm0.csv")
# StatCompiler
dat_filename <- list.files("./gen/calculate-direct/audit")
dat_filename <- dat_filename[grepl("statcompiler", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename), 1)
statcompiler <- read.csv(paste0("./gen/calculate-direct/audit/", dat_filename, sep = ""))
################################################################################

# Calculated indicators and stat compiler
data <- merge(direct, statcompiler, by = c("dhs_indicator_code"), all.x = TRUE)
names(data)[which(names(data) == "value")] <- "value_SC"
names(data)[which(names(data) == "dir")] <- "value"
data$value_SC <- data$value_SC/100

datLong <- data %>%
  dplyr::select(variable, value, value_SC) %>%
  pivot_longer(c(value, value_SC)) %>%
  mutate(series = ifelse(endsWith(name, '_SC'), "StatCompiler", "Calculated"))

# check rh_pnc_wm_2days, rh_pnc_wm_bfcounsel


p1 <- datLong %>%
  ggplot() +
  geom_bar(aes(x = series, y = value, fill = series), stat = "identity") +
  facet_wrap(~variable) +
  theme(text = element_text(size = 10))
ggsave(str_glue("./gen/calculate-direct/audit/statcompiler-compare6.pdf"), p1, height = 10, width = 8, units = "in") 



