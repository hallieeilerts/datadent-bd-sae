################################################################################
#' @description Compare calculated indicators to StatCompiler
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/util.R")
# Manually calculated indicators
direct <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
# StatCompiler
dat_filename <- list.files("./gen/calculate-direct/audit")
dat_filename <- dat_filename[grepl("statcompiler", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename), 1)
statcompiler <- read.csv(paste0("./gen/calculate-direct/audit/", dat_filename, sep = ""))
################################################################################

# Calculated indicators and stat compiler
data <- merge(direct, statcompiler, suffixes = c("", "_SC"), by = c("dhs_indicator_code"), all.x = TRUE)
data <- subset(data, admin_level == "adm0")
data$value_SC <- data$value_SC/100

datLong <- data %>%
  select(indicator, value, value_SC) %>%
  pivot_longer(c(value, value_SC)) %>%
  mutate(series = ifelse(endsWith(name, '_SC'), "StatCompiler", "Calculated"))

p1 <- ggplot(datLong) +
  geom_bar(aes(x = series, y = value, fill = series), stat = "identity") +
  facet_wrap(~indicator) 
ggsave(str_glue("./gen/calculate-direct/audit/statcompiler-compare.pdf"), p1, height = 10, width = 8, units = "in") 



