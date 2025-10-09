################################################################################
#' @description Creates faceted map of prevalence and variance for an indicator
#' loads from model files (can easily do for any model)
#' @return One faceted map for prevalence, one for variance
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
#' Inputs
source("./src/util.R")
# Bangladesh division boundaries
bangladesh_1 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm1_bbs_20201113")
# Bangladesh district boundaries
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
est <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
ind <- read_excel("./data/ind-info.xlsx", sheet = "indicators")
info <- read.csv("./gen/model/audit/model-info.csv")
# minimum error model for adm1
minerror_adm1_mod <- read.csv("./gen/validation/output/agg-minerror-adm1.csv")
################################################################################

# subset to included indicators
df_ind <- subset(ind, status == "include")

# vector of indicators
v_var <- unique(df_ind$variable)
v_varlong <- df_ind$description

# indicators coming from kr
# for which i plot direct estimates without phantom household
v_var_ch <- subset(df_ind, dhs_dataset == "kr")$variable


# Sept 2025 presentation --------------------------------------------------

# plotting direct estimates against one model

# variables for sept 2025 presentation
#v_var_sept2025 <- c("nt_wm_micro_iron")
v_var_sept2025 <- v_var
v_varlong <- df_ind$description

for(i in 1:length(v_var_sept2025)){
  
  myoutcome <- v_var_sept2025[i]
  myoutlab <- subset(df_ind, variable == myoutcome)$description
  # selected model for indicator (one with minimum aggregated adm1 error)
  mymodel <- subset(minerror_adm1_mod, variable == myoutcome)$model
  print(myoutcome)
  
  ## VARIANCE SMOOTHING MODEL
  
  # filenames for posterior predictions (from all models) for this indicator
  filenames <- list.files("./gen/model/pred/")
  filenames <- filenames[grepl("pred", filenames, ignore.case = TRUE)]
  filenames <- filenames[grepl(myoutcome, filenames)]
  if(myoutcome == "nt_wm_micro_iron"){
    filenames <- filenames[!grepl("nt_wm_micro_iron_any", filenames)]
  }
  
  # load posterior prediction for first model in df
  filename <- filenames[1]
  # for brevity, extract vers and test
  model_abbrev <- paste(strsplit(filename, "-")[[1]][4:5], collapse = "-")
  model_abbrev <- gsub(".csv", "", model_abbrev)
  postpred <- read.csv(paste0("./gen/model/pred/",filename))
  names(postpred)[which(names(postpred) == "post_mean")] <- paste0(model_abbrev, "_mean")
  names(postpred)[which(names(postpred) == "post_var")] <- paste0(model_abbrev, "_var")
  names(postpred)[which(names(postpred) == "qt_lb")] <- paste0(model_abbrev, "_qt_lb")
  names(postpred)[which(names(postpred) == "qt_ub")] <- paste0(model_abbrev, "_qt_ub")
  
  # merge on posterior predictions for subsequent models
  if(length(filenames) > 1){
    # if more than one version of this model, read in and merge on
    for(j in 2:length(filenames)){
      
      filename <- filenames[j]
      # Extract the part between the second-to-last and third-to-last hyphen
      #model <- str_match(filename, "-([0-9]+-[^-.]+)-[0-9]{8}\\.csv$")[,2]
      model_abbrev <- paste(strsplit(filename, "-")[[1]][4:5], collapse = "-")
      model_abbrev <- gsub(".csv", "", model_abbrev)
      postpred_mult <- read.csv(paste0("./gen/model/pred/",filename))
      postpred_mult <- postpred_mult[,c("ADM2_EN", "post_mean", "post_var", "qt_lb", "qt_ub")]
      names(postpred_mult)[which(names(postpred_mult) == "post_mean")] <- paste0(model_abbrev, "_mean")
      names(postpred_mult)[which(names(postpred_mult) == "post_var")] <- paste0(model_abbrev, "_var")
      names(postpred_mult)[which(names(postpred_mult) == "qt_lb")] <- paste0(model_abbrev, "_qt_lb")
      names(postpred_mult)[which(names(postpred_mult) == "qt_ub")] <- paste0(model_abbrev, "_qt_ub")
      postpred <- merge(postpred, postpred_mult, by = "ADM2_EN")
    }
  }
  
  # reshape direct estimates and predictions long
  # use dirplot for direct estimates for indicators coming from child recode 
  # (dirplot is the direct estimate without the phantom household. this is the one we want to plot)
  # use dir for other dhs data files (where the direct estimate didn't need to be calculated separately for plotting)
  if(myoutcome %in% v_var_ch){
    postpred_value <- postpred %>%
      pivot_longer(
        cols = c("dirplot" | matches("^[0-9]") & ends_with("mean")),
        names_to = "name",
        values_to = "value"
      ) %>%
      mutate(name = ifelse(name == "dirplot", "direct", name)) %>%
      mutate(name = gsub("_mean", "", name)) %>%
      dplyr::select(ADM2_EN, name, value)
    postpred_var <- postpred %>%
      pivot_longer(
        cols = c("dirplot_var" | matches("^[0-9]") & ends_with("var")),
        names_to = "name",
        values_to = "var"
      ) %>%
      mutate(name = ifelse(name == "dirplot_var", "direct", name)) %>%
      mutate(name = gsub("_var", "", name)) %>%
      dplyr::select(ADM2_EN, name, var)
  }else{
    postpred_value <- postpred %>%
      pivot_longer(
        cols = c("dir" | matches("^[0-9]") & ends_with("mean")),
        names_to = "name",
        values_to = "value"
      ) %>%
      mutate(name = ifelse(name == "dir", "direct", name)) %>%
      mutate(name = gsub("_mean", "", name)) %>%
      dplyr::select(ADM2_EN, name, value)
    postpred_var <- postpred %>%
      pivot_longer(
        cols = c("dir_var" | matches("^[0-9]") & ends_with("var")),
        names_to = "name",
        values_to = "var"
      ) %>%
      mutate(name = ifelse(name == "dir_var", "direct", name)) %>%
      mutate(name = gsub("_var", "", name)) %>%
      dplyr::select(ADM2_EN, name, var)
  }
  
  # extract lower and upper bounds for predictions
  postpred_qt_lb <- postpred %>%
    pivot_longer(
      cols = c(matches("^[0-9]") & ends_with("_qt_lb")),
      names_to = "name",
      values_to = "qt_lb"
    ) %>%
    mutate(name = gsub("_qt_lb", "", name)) %>%
    dplyr::select(ADM2_EN, name, qt_lb)
  postpred_qt_ub <- postpred %>%
    pivot_longer(
      cols = c(matches("^[0-9]") & ends_with("_qt_ub")),
      names_to = "name",
      values_to = "qt_ub"
    ) %>%
    mutate(name = gsub("_qt_ub", "", name)) %>%
    dplyr::select(ADM2_EN, name, qt_ub)
  
  # combine
  postpred_ourmod <- postpred_value %>%
    inner_join(postpred_var, by = c("ADM2_EN", "name")) %>%
    full_join(postpred_qt_lb, by = c("ADM2_EN", "name")) %>%
    full_join(postpred_qt_ub, by = c("ADM2_EN", "name")) %>%
    mutate(var = var * 100,
           se = sqrt(var*100),
           value = value * 100)
  # calculate confidence intervals for direct estimates
  postpred_ourmod$lb <- ifelse(!is.na(postpred_ourmod$qt_lb), postpred_ourmod$qt_lb*100, postpred_ourmod$value - 1.96*postpred_ourmod$se)
  postpred_ourmod$ub <- ifelse(!is.na(postpred_ourmod$qt_ub), postpred_ourmod$qt_ub*100, postpred_ourmod$value + 1.96*postpred_ourmod$se)
  # sometimes the direct confidence intervals will exceed 0 or 100
  postpred_ourmod$lb[postpred_ourmod$lb < 0] <- 0
  postpred_ourmod$ub[postpred_ourmod$ub > 100] <- 100
  postpred_ourmod <- postpred_ourmod %>%
    mutate(model = "OurModel") %>%
    select(ADM2_EN, name, model, value, lb, ub, se, var)
  
  ## Add on covariate info
  
  postpred_comb <- postpred_ourmod
  postpred_comb$variable <- myoutcome
  
  # Load vers, test, covariate info for outcome models
  myinfo <- subset(info, outcome == myoutcome)[,c("vers", "test", "cov")]
  myinfo$name <- paste(myinfo$vers, myinfo$test, sep = "-")
  
  # merge on covariates to dat_plot
  dat_plot <- merge(postpred_comb, myinfo, by = "name", all.x = TRUE)
  
  # only keep direct and chosen model
  dat_plot <- dat_plot %>%
    filter(name %in% c("direct", mymodel)) %>%
    filter(name != "100-2") %>%
    mutate(name = ifelse(name == "direct", "Direct", "Modeled")) %>%
    mutate(name = factor(name, levels = c("Direct", "Modeled")))
  
  # Join spatial data
  postpred_sf <- bangladesh_2 %>% 
    left_join(dat_plot, by = "ADM2_EN")
  
  p1 <- ggplot() +
    geom_sf(data = postpred_sf, aes(fill = value), color = NA) +
    geom_sf(data = bangladesh_2, color = "black", fill = NA) +
    scale_fill_viridis_c(
      option = "D",  # "D" is default; you can also try "C", "B", "A", "E"
      limits = c(0, max(postpred_sf$value, na.rm = TRUE)),
      direction = -1, # reverse scale
      na.value = "grey80",
      name = ""
    ) +
    labs(title = myoutlab, subtitle = "Prevalence") +
    facet_wrap(~name) +
    theme_void() +
    theme(
      text = element_text(size = 10),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  ggsave(paste0("./gen/visualizations/facet-maps/map-",myoutcome,"-prev.png"), p1, width = 10, height = 5.5)
  
  p2 <- ggplot() +
    geom_sf(data = postpred_sf, aes(fill = var), color = NA) +
    geom_sf(data = bangladesh_2, color = "black", fill = NA) +
    scale_fill_viridis_c(
      option = "D",  # "D" is default; you can also try "C", "B", "A", "E"
      limits = c(0, max(postpred_sf$var, na.rm = TRUE)),
      direction = -1, # reverse scale
      na.value = "grey80",
      name = ""
    ) +
    labs(title = myoutlab, subtitle = "Variance") +
    facet_wrap(~name) +
    theme_void() +
    theme(
      text = element_text(size = 10),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  ggsave(paste0("./gen/visualizations/facet-maps/map-",myoutcome,"-var.png"), p2, width = 10, height = 5.5)
  
}

