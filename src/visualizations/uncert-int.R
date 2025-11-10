################################################################################
#' @description Creates uncertainty interval plots comparing our models, and our models vs summer
#' @return Loops through all indicators to create plots
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(sf)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(stringr)
library(patchwork)
#' Inputs
source("./src/util.R")
ind <- read_excel("./data/ind-info.xlsx", sheet = "indicators")
info <- read.csv("./gen/model/audit/model-info.csv")
info_summer <- read.csv("./gen/model/audit/model-info-summer.csv")
# minimum error model for adm1
minerror_adm1_mod <- read.csv("./gen/validation/output/agg-minerror-adm1.csv")
################################################################################

# subset to included indicators
df_ind <- subset(ind, status == "include")

# vector of indicators
v_var <- unique(df_ind$variable)
v_varlong <- df_ind$description

# indicators coming from kr
# for which i plot direct estimates without synthetic household
v_var_ch <- subset(df_ind, dhs_dataset == "kr")$variable

#v_var <- "nt_ch_micro_vas"
#v_var <- "nt_ebf"
#v_var <- "ch_diar_ors"
#v_var <- "nt_ch_gwmt_any"
#v_var <- "ph_wtr_improve"

# Compare all models ------------------------------------------------------

for(i in 1:length(v_var)){
  
  myoutcome <- v_var[i]
  myoutlab <- v_varlong[i]
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
  # use dirplot for direct estimates for indicators coming from child recode (where the direct estimate should be the one for plotting)
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
  
 
  ## ADD ON MODINFO
  
  postpred_comb <- postpred_ourmod
  postpred_comb$variable <- myoutcome
  
  # Load model info for our model
  myinfo <- subset(info, outcome == myoutcome)[,c("vers", "test", "cov")]
  myinfo$name <- paste(myinfo$vers, myinfo$test, sep = "-")
  
  # merge on covariates to dat_plot
  dat_plot <- merge(postpred_comb, myinfo, by = "name", all.x = TRUE)

  # split into two panels
  df_panels <- dat_plot %>%
    dplyr::select(ADM2_EN) %>%
    distinct() %>% arrange(ADM2_EN) %>%
    mutate(n = 1:n(),
           total = n(),
           panel = ifelse(n/total<0.5, 1, 2))
  
  dat_plot_modlabs <- dat_plot %>%
    mutate(highlight = ifelse(value < lb | value > ub, "highlight", "ok")) %>%
    mutate(highlight = factor(highlight, levels = c("ok", "highlight"))) %>%
    filter(name != "100-2") %>%
    mutate(name = factor(name, levels = c("direct", "100-1", "101-1", "102-1", "103-1")))
  
  plot1 <- dat_plot_modlabs %>% 
    filter(ADM2_EN %in% subset(df_panels, panel == 1)$ADM2_EN)
  plot2 <- dat_plot_modlabs %>% 
    filter(ADM2_EN %in% subset(df_panels, panel == 2)$ADM2_EN)
  
  p1 <- plot1 %>%
    ggplot(aes(x = ADM2_EN, y = value, color = name)) +
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
    geom_point(aes(shape = highlight), position = position_dodge(width = 0.5)) +
    labs(x = "", y = "") +
    theme_bw() +
    coord_flip() +
    labs(title = paste0(myoutcome)) +
    theme(text = element_text(size = 10), legend.title=element_blank()) +
    guides(shape = "none")
  p2 <- plot2 %>%
    ggplot(aes(x = ADM2_EN, y = value, color = name)) +
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
    geom_point(aes(shape = highlight), position = position_dodge(width = 0.5)) +
    labs(x = "", y = "") +
    theme_bw() +
    coord_flip() +
    theme(text = element_text(size = 10), legend.title=element_blank()) +
    scale_shape_manual(values = c(16, 8)) +
    guides(shape = "none")
  # Combine p1 and p2
  combined_plot <- (p1 + p2) + 
    plot_layout(guides = "collect") &   # collect shared legend
    theme(legend.position = "bottom")   # move legend bottom
  # Add single title on top
  combined_plot  <- combined_plot + plot_annotation(title = paste0(myoutlab))
  ggsave(paste0("./gen/visualizations/uncert-int/20251029/", myoutcome,".png"), 
         combined_plot, width = 10, height = 6, units = "in", dpi = 300)

}

# Sept 2025 presentation --------------------------------------------------

# plotting direct estimates against one model

# variables for sept 2025 presentation
v_var_sept2025 <- c("nt_wm_micro_iron")
v_var_sept2025 <- v_var
v_varlong <- df_ind$description

for(i in 1:length(v_var_sept2025)){
  
  myoutcome <- v_var_sept2025[i]
  myoutlab <- subset(df_ind, variable == myoutcome)$description
  #mymodel <- subset(df_ind, variable == myoutcome)$model
  # selected model for indicator (one with minimum aggregated adm1 error)
  mymodel <- subset(minerror_adm1_mod , variable == myoutcome)$model
  
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
  # use dirplot for direct estimates for indicators coming from child recode (where the direct estimate should be the one for plotting)
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
  
  
  ## ADD ON MODINFO
  
  postpred_comb <- postpred_ourmod
  postpred_comb$variable <- myoutcome
  
  # Load model info for our model
  myinfo <- subset(info, outcome == myoutcome)[,c("vers", "test", "cov")]
  myinfo$name <- paste(myinfo$vers, myinfo$test, sep = "-")
  
  # merge on covariates to dat_plot
  dat_plot <- merge(postpred_comb, myinfo, by = "name", all.x = TRUE)
  
  # only keep direct and chosen model
  dat_plot <- dat_plot %>%
    filter(name %in% c("direct", mymodel))
  
  # split into two panels
  df_panels <- dat_plot %>%
    dplyr::select(ADM2_EN) %>%
    distinct() %>% arrange(ADM2_EN) %>%
    mutate(n = 1:n(),
           total = n(),
           panel = ifelse(n/total<0.5, 1, 2))
  
  dat_plot_modlabs <- dat_plot %>%
    mutate(highlight = ifelse(value < lb | value > ub, "highlight", "ok")) %>%
    mutate(highlight = factor(highlight, levels = c("ok", "highlight"))) %>%
    filter(name != "100-2") %>%
    mutate(name = ifelse(name == "direct", "Direct", "Modeled")) %>%
    mutate(name = factor(name, levels = c("Direct", "Modeled")))
  
  plot1 <- dat_plot_modlabs %>% 
    filter(ADM2_EN %in% subset(df_panels, panel == 1)$ADM2_EN)
  plot2 <- dat_plot_modlabs %>% 
    filter(ADM2_EN %in% subset(df_panels, panel == 2)$ADM2_EN)
  
  p1 <- plot1 %>%
    ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
    geom_point(aes(shape = highlight), position = position_dodge(width = 0.5)) +
    labs(x = "", y = "") +
    theme_bw() +
    coord_flip() +
    theme(text = element_text(size = 10), legend.title=element_blank()) +
    guides(shape = "none")
  p2 <- plot2 %>%
    ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
    geom_point(aes(shape = highlight), position = position_dodge(width = 0.5)) +
    labs(x = "", y = "") +
    theme_bw() +
    coord_flip() +
    theme(text = element_text(size = 10), legend.title=element_blank()) +
    scale_shape_manual(values = c(16, 8)) +
    guides(shape = "none")
  # Combine p1 and p2
  combined_plot <- (p1 + p2) + 
    plot_layout(guides = "collect") &   # collect shared legend
    theme(legend.position = "bottom",
          legend.margin = margin(t = -5, b = 0),   # tighten legend spacing
          legend.box.margin = margin(t = -5)       # reduce gap between plot and legend
    )
  # Add single title on top
  combined_plot  <- combined_plot + plot_annotation(title = paste0(myoutlab))
  
  # ggsave(paste0("./reports/sept2025-pres/figures/", myoutcome,".png"), 
  #        combined_plot, width = 10, height = 5.5, units = "in", dpi = 300)
  ggsave(paste0("./gen/visualizations/uncert-int/20250905/", myoutcome,".png"), 
         combined_plot, width = 10, height = 5.5, units = "in", dpi = 300)
  
}


# Guidance note -----------------------------------------------------------

# plotting direct estimates against one model

v_var_guidance <- c("ch_diar_ors", 
                    "nt_ch_micro_vas",
                    "ph_sani_improve", 
                    "rh_anc_toxinj")
v_var_guidance_desc <- c("Children under 24m with diarrhea who received ORS",
               "Children 6-23m given vitamin A supplements", 
               "Household access to improved sanitation",
               "Women received 2+ tetanus shots during pregnancy")


for(i in 1:length(v_var_guidance)){
  
  myoutcome <- v_var_guidance[i]
  myoutlab <- v_var_guidance_desc[i]
  #mymodel <- subset(df_ind, variable == myoutcome)$model
  # selected model for indicator (one with minimum aggregated adm1 error)
  #mymodel <- subset(minerror_adm1_mod , variable == myoutcome)$model
  # FOR GUIDANCE NOTE
  # limit to model chosen using RMSE by covariate grouping in consultation with Maiga
  mymodel <- ind %>%
    filter(variable == myoutcome) %>%
    select(variable, model_covar_grp)  %>%
    rename(model = model_covar_grp) %>%
    pull(model)

  
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
  # use dirplot for direct estimates for indicators coming from child recode (where the direct estimate should be the one for plotting)
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
  # calculate confidence intervals for naive/direct estimates
  postpred_ourmod$lb <- ifelse(!is.na(postpred_ourmod$qt_lb), postpred_ourmod$qt_lb*100, postpred_ourmod$value - 1.96*postpred_ourmod$se)
  postpred_ourmod$ub <- ifelse(!is.na(postpred_ourmod$qt_ub), postpred_ourmod$qt_ub*100, postpred_ourmod$value + 1.96*postpred_ourmod$se)
  # sometimes the direct confidence intervals will exceed 0 or 100
  postpred_ourmod$lb[postpred_ourmod$lb < 0] <- 0
  postpred_ourmod$ub[postpred_ourmod$ub > 100] <- 100
  postpred_ourmod <- postpred_ourmod %>%
    mutate(model = "OurModel") %>%
    select(ADM2_EN, name, model, value, lb, ub, se, var)
  

  ## ADD ON MODINFO
  
  postpred_comb <- postpred_ourmod
  postpred_comb$variable <- myoutcome
  
  # Load model info for our model
  myinfo <- subset(info, outcome == myoutcome)[,c("vers", "test", "cov")]
  myinfo$name <- paste(myinfo$vers, myinfo$test, sep = "-")
  
  # merge on covariate info to dat_plot
  dat_plot <- merge(postpred_comb, myinfo, by = "name", all.x = TRUE)
  
  # only keep direct and chosen model
  dat_plot <- dat_plot %>%
    filter(name %in% c("direct", mymodel))
  
  # split into two panels
  df_panels <- dat_plot %>%
    dplyr::select(ADM2_EN) %>%
    distinct() %>% arrange(ADM2_EN) %>%
    mutate(n = 1:n(),
           total = n(),
           panel = ifelse(n/total<0.5, 1, 2))
  
  dat_plot_modlabs <- dat_plot %>%
    mutate(highlight = ifelse(value < lb | value > ub, "highlight", "ok")) %>%
    mutate(highlight = factor(highlight, levels = c("ok", "highlight"))) %>%
    filter(name != "100-2") %>%
    mutate(name = ifelse(name == "direct", "Naive", "Modeled")) %>%
    mutate(name = factor(name, levels = c("Modeled", "Naive")))
  
  plot1 <- dat_plot_modlabs %>% 
    filter(ADM2_EN %in% subset(df_panels, panel == 1)$ADM2_EN) 
  plot2 <- dat_plot_modlabs %>% 
    filter(ADM2_EN %in% subset(df_panels, panel == 2)$ADM2_EN) 
  
  p1 <- plot1 %>%
    ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
    geom_point(aes(shape = highlight), position = position_dodge(width = 0.5)) +
    scale_color_manual(values = c("#00BFC4", "grey38"), guide = guide_legend(reverse = TRUE)) +
    labs(x = "Adm2 district", y = "Prevalence") +
    theme_bw() +
    coord_flip() +
    theme(text = element_text(size = 10), legend.title=element_blank()) +
    guides(shape = "none")
  p2 <- plot2 %>%
    ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
    geom_point(aes(shape = highlight), position = position_dodge(width = 0.5)) +
    scale_color_manual(values = c("#00BFC4", "grey38"), guide = guide_legend(reverse = TRUE)) +
    labs(x = "", y = "Prevalence") +
    theme_bw() +
    coord_flip() +
    theme(text = element_text(size = 10), legend.title=element_blank()) +
    scale_shape_manual(values = c(16, 8)) +
    guides(shape = "none")
  # Combine p1 and p2
  combined_plot <- (p1 + p2) + 
    plot_layout(guides = "collect") &   # collect shared legend
    theme(legend.position = "bottom",
          legend.margin = margin(t = -5, b = 0),   # tighten legend spacing
          legend.box.margin = margin(t = -5)       # reduce gap between plot and legend
    )
  # Add single title on top
  combined_plot  <- combined_plot + plot_annotation(title = paste0(myoutlab))
  
  ggsave(paste0("./gen/visualizations/uncert-int/guidance-note/", myoutcome,".png"), 
         combined_plot, width = 8, height = 4, units = "in", dpi = 500)
  
}



# Compare against summer --------------------------------------------------

for(i in 1:length(v_var)){
  
  myoutcome <- v_var[i]
  myoutlab <- v_varlong[i]
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
  
  # extract direct estimates for indicators coming from child recode or other file
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
  
  # extract lower and upper bounds
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
           se = sqrt(var),
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

  ## SUMMER MODEL
  
  # filenames for posterior predictions (from all models) for this indicator
  filenames <- list.files("./gen/model/pred-summer/")
  filenames <- filenames[grepl("pred", filenames, ignore.case = TRUE)]
  filenames <- filenames[grepl(myoutcome, filenames)]
  if(myoutcome == "nt_wm_micro_iron"){
    filenames <- filenames[!grepl("nt_wm_micro_iron_any", filenames)]
  }
  
  # load posterior predictions for all models
  postpred_summer <- data.frame()
  for(j in 1:length(filenames)){
    filename <- filenames[j]
    # Extract the part between the second-to-last and third-to-last hyphen
    model_abbrev <- paste(strsplit(filename, "-")[[1]][4:5], collapse = "-")
    model_abbrev <- gsub(".csv", "", model_abbrev)
    postpred_mult <- read.csv(paste0("./gen/model/pred-summer/",filename))
    postpred_mult <- postpred_mult[,c("domain", "mean", "var", "lower", "upper")]
    names(postpred_mult)[which(names(postpred_mult) == "domain")] <- "ADM2_EN"
    names(postpred_mult)[which(names(postpred_mult) == "mean")] <- "value"
    names(postpred_mult)[which(names(postpred_mult) == "lower")] <- "lb"
    names(postpred_mult)[which(names(postpred_mult) == "upper")] <- "ub"
    postpred_mult$name <- model_abbrev
    postpred_summer <- rbind(postpred_summer, postpred_mult)
  }
  # to the summer file. add test -1
  # and change all vers to 001
  
  # adjust columns
  postpred_summer$value <- postpred_summer$value*100
  postpred_summer$var <- postpred_summer$var*100
  postpred_summer$se <- sqrt(postpred_summer$var)
  postpred_summer$lb <- postpred_summer$lb*100
  postpred_summer$ub <- postpred_summer$ub*100
  postpred_summer$model <- "SUMMER"
  
  postpred_summer <- postpred_summer %>%
    select(ADM2_EN, name, model, value, lb, ub, se, var)
  
  ## COMBINE
  
  postpred_comb <- rbind(postpred_ourmod, postpred_summer)
  postpred_comb$variable <- myoutcome
  
  # Load model info for our model and summer
  myinfo <- subset(info, outcome == myoutcome)[,c("vers", "test", "cov")]
  myinfo_summer <- subset(info_summer, outcome == myoutcome)[,c("vers", "test", "cov")]
  # merge model info, only keeping models which have same vers and test numbers
  bothinfo <- merge(myinfo, myinfo_summer, by = c("vers", "test"), suffixes = c("_mymod", "_summer"))
  if(nrow(bothinfo) != nrow(myinfo)){
    warning("Some models dropped.")
  }
  # only keep those that have same covariates
  bothinfo <- subset(bothinfo, cov_mymod == cov_summer)
  bothinfo$cov <- bothinfo$cov_mymod
  bothinfo$cov_mymod <- bothinfo$cov_summer <- NULL
  bothinfo$name <- paste(bothinfo$vers, bothinfo$test, sep = "-")
  
  # merge on covariates to dat_plot
  dat_plot <- merge(postpred_comb, bothinfo, by = "name", all.x = TRUE)
  # keep direct estimates or those that had covariates merged on
  dat_plot <- subset(dat_plot, name == "direct" | !is.na(cov))
  dat_plot$model[dat_plot$name == "direct"] <- "Direct"
  
  # split into two panels
  df_panels <- dat_plot %>%
    dplyr::select(ADM2_EN) %>%
    distinct() %>% arrange(ADM2_EN) %>%
    mutate(n = 1:n(),
           total = n(),
           panel = ifelse(n/total<0.5, 1, 2))

  v_cov <- unique(dat_plot$cov)
  v_cov <- v_cov[!is.na(v_cov)]
  
  # plot for each set of covariates
  for(k in 1:length(v_cov)){
    
    mycov <- v_cov[k]
    
    dat_plot <- dat_plot %>%
      mutate(highlight = ifelse(value < lb | value > ub, "highlight", "ok")) %>%
      mutate(highlight = factor(highlight, levels = c("ok", "highlight")))
    
    plot1 <- dat_plot %>% 
      filter(ADM2_EN %in% subset(df_panels, panel == 1)$ADM2_EN) %>%
      filter(model == "Direct" | cov %in% mycov)
    plot2 <- dat_plot %>% 
      filter(ADM2_EN %in% subset(df_panels, panel == 2)$ADM2_EN) %>%
      filter(model == "Direct" | cov %in% mycov)
    
    # name of model being shown in plot
    myname <- unique(plot1$name)
    myname <- myname[myname != "direct"]
    
    p1 <- plot1 %>%
      ggplot(aes(x = ADM2_EN, y = value, color = model)) +
      geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
      geom_point(aes(shape = highlight), position = position_dodge(width = 0.5)) +
      labs(x = "", y = "") +
      theme_bw() +
      coord_flip() +
      labs(title = paste0(myoutcome, "~", mycov)) +
      theme(text = element_text(size = 10), legend.title=element_blank()) +
      scale_color_discrete(guide = guide_legend(reverse = TRUE))  +
      scale_shape_manual(values = c(16, 8)) +
      guides(shape = "none")
    p2 <- plot2 %>%
      ggplot(aes(x = ADM2_EN, y = value, color = model)) +
      geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
      geom_point(aes(shape = highlight), position = position_dodge(width = 0.5)) +
      labs(x = "", y = "") +
      theme_bw() +
      coord_flip() +
      theme(text = element_text(size = 10), legend.title=element_blank()) +
      scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
      scale_shape_manual(values = c(16, 8))  +
      guides(shape = "none")
    # Combine p1 and p2
    combined_plot <- (p1 + p2) + 
      plot_layout(guides = "collect") &   # collect shared legend
      theme(legend.position = "bottom")   # move legend bottom
    # Add single title on top
    combined_plot  <- combined_plot + plot_annotation(title = paste0(myoutlab, " (", myoutcome, ")"))
    ggsave(paste0("./gen/visualizations/compare-summer/", myoutcome,"-", myname, ".png"), 
           combined_plot, width = 10, height = 7.5, units = "in", dpi = 300)
    
  }
}
