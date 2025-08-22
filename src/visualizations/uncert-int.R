################################################################################
#' @description Creates uncertainty interval plots for all models for one indicator
#' @return One plot with uncertainty intervals for all models for one indicator
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(sf)
library(dplyr)
library(tidyr)
library(wesanderson)
library(ggplot2)
library(stringr)
library(patchwork)
library(readxl)
#' Inputs
source("./src/util.R")
info <- read.csv("./gen/model/audit/model-info.csv")
ind <- read_excel("./data/ind-info.xlsx", sheet = "indicators")
################################################################################

# subset to included indicators
df_ind <- subset(ind, status == "include")

# choose one panel or two panel plots
two_panel <- TRUE

# indicator list
v_all_outcomes <- df_ind$variable
v_all_outlab <- df_ind$description

# list of indicators in child level estimates/model
# these are the ones we have to worry about for sparse data
# and plotting the direct estimate that hasn't been altered with a synthetic household
v_ch_outcomes <- c("ch_diar_ors",
                   "ch_diar_zinc",
                   "nt_ch_micro_dwm", 
                   "nt_ch_micro_mp",
                   "nt_ch_micro_vas", 
                   "nt_ebf")

which(v_all_outcomes == "rh_anc_1vs")

for(i in 1:length(v_all_outcomes)){
  
  mynum <- i
  
  myoutcome <- v_all_outcomes[mynum]
  outlab <- v_all_outlab[mynum]
  
  print(myoutcome)
  
  # 001 no intercept
  # 002 has intercept
  # 003 has residence covariate
  # 004 has mother_edu covariate
  # 005 has wealth_index covariate
  # 006 has hhd_under5 covariate
  # 007 has residence, mother_edu
  # 008 has residence, wealth_index
  # 050 added full slate of covariates as availabe on 20250507
  ## "residence", "wealth_index", "hhd_under5", "hhd_head_age", "hhd_head_sex", "mother_age", "child_age"
  # 051 removed residence because of high sd
  ## "wealth_index", "hhd_under5", "hhd_head_age", "hhd_head_sex", "mother_age", "child_age"
  # only keep >=100 to compare models with penalized complexity prior
  # if vers>=100, keep test1 or test2 (the latter with less spatial smoothing due to pcp prior values)
  
  # posterior predictions for all models for this indicator
  filenames <- list.files("./gen/model/pred/")
  filenames <- filenames[grepl("pred", filenames, ignore.case = TRUE)]
  filenames <- filenames[grepl(myoutcome, filenames)]
  if(myoutcome == "nt_wm_micro_iron"){
    filenames <- filenames[!grepl("nt_wm_micro_iron_any", filenames)]
  }
  
  # load posterior prediction for first model in df
  filename <- filenames[1]
  # for brevity, extract vers and test
  model_abbrev <- substr(filename, nchar(filename) - 12, nchar(filename) - 4)
  postpred <- read.csv(paste0("./gen/model/pred/",filename))
  names(postpred)[which(names(postpred) == "post_mean")] <- paste0(model_abbrev, "_mean")
  names(postpred)[which(names(postpred) == "post_var")] <- paste0(model_abbrev, "_var")
  names(postpred)[which(names(postpred) == "qt_lb")] <- paste0(model_abbrev, "_qt_lb")
  names(postpred)[which(names(postpred) == "qt_ub")] <- paste0(model_abbrev, "_qt_ub")
  
  # load and merge on posterior predictions for all subsequent models
  if(length(filenames) > 1){
    # if more than one version of this model, read in and merge on
    for(i in 2:length(filenames)){
      
      filename <- filenames[i]
      # Extract the part between the second-to-last and third-to-last hyphen
      #model <- str_match(filename, "-([0-9]+-[^-.]+)-[0-9]{8}\\.csv$")[,2]
      model_abbrev <- substr(filename, nchar(filename) - 12, nchar(filename) - 4)
      postpred_mult <- read.csv(paste0("./gen/model/pred/",filename))
      if("dirplot" %in% names(postpred_mult) & !("dirplot" %in% names(postpred))){
        postpred_mult <- postpred_mult[,c("ADM2_EN", "dirplot", "dirplot_var", "post_mean", "post_var", "qt_lb", "qt_ub")]
      }else{
        postpred_mult <- postpred_mult[,c("ADM2_EN", "post_mean", "post_var", "qt_lb", "qt_ub")]
      }
      names(postpred_mult)[which(names(postpred_mult) == "post_mean")] <- paste0(model_abbrev, "_mean")
      names(postpred_mult)[which(names(postpred_mult) == "post_var")] <- paste0(model_abbrev, "_var")
      names(postpred_mult)[which(names(postpred_mult) == "qt_lb")] <- paste0(model_abbrev, "_qt_lb")
      names(postpred_mult)[which(names(postpred_mult) == "qt_ub")] <- paste0(model_abbrev, "_qt_ub")
      postpred <- merge(postpred, postpred_mult, by = "ADM2_EN")
    }
  }
  
  # reshape -----------------------------------------------------------------
  
  if(myoutcome %in% v_ch_outcomes){
    
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
    
  }
  
  if(!(myoutcome %in% v_ch_outcomes)){
    
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
  
  postpred_plot <- postpred_value %>%
    inner_join(postpred_var, by = c("ADM2_EN", "name")) %>%
    full_join(postpred_qt_lb, by = c("ADM2_EN", "name")) %>%
    full_join(postpred_qt_ub, by = c("ADM2_EN", "name")) %>%
    mutate(se = sqrt(var))
  
  # calculate standard uncertainty intervals for direct estimates
  # credible intervals for model predictions
  postpred_plot$lb <- ifelse(!is.na(postpred_plot$qt_lb), postpred_plot$qt_lb, postpred_plot$value - 1.96*postpred_plot$se)
  postpred_plot$ub <- ifelse(!is.na(postpred_plot$qt_ub), postpred_plot$qt_ub, postpred_plot$value + 1.96*postpred_plot$se)
  # scale
  postpred_plot$value <- postpred_plot$value*100
  postpred_plot$lb <- postpred_plot$lb*100
  postpred_plot$ub <- postpred_plot$ub*100
  # sometimes the direct confidence intervals will exceed 0 or 100
  postpred_plot$lb[postpred_plot$lb < 0] <- 0
  postpred_plot$ub[postpred_plot$ub > 100] <- 100
  
  v_plots <- unique(postpred_plot$name)
  v_plots <- rev(v_plots)
  postpred_plot$name <- factor(postpred_plot$name, levels = v_plots)
  postpred_plot$model <- "OurModel"
  postpred_plot$model[postpred_plot$name == "direct"] <- "Direct"
  postpred_plot <- postpred_plot[,c("ADM2_EN", "name", "model", "value", "lb", "ub")]
  
  # Merge on model info -----------------------------------------------------
  
  # Subset to outcome of interest
  df_info <- subset(info, outcome == myoutcome)
  
  # if cov is NA in my model info, assign as 1
  df_info$cov[is.na(df_info$cov)] <- 1
  # create name column that matches with model predictions, only keep that and covariate column
  df_info <- df_info %>%
    mutate(vers_padded = str_pad(as.character(vers), width = 3, side = "left", pad = "0")) %>%
    mutate(name_mymodel = paste0(vers_padded, "-", test)) %>%
    dplyr::select(name_mymodel, cov)
  
  # merge covariate info onto model predictions
  postpred_plot <- postpred_plot %>%
    left_join(df_info, by = c("name" = "name_mymodel"))
  
  
  # One panel ---------------------------------------------------------------
  
  if(!two_panel){
    
    p <- postpred_plot %>%
      #filter(name %in% "direct" | cov == 1) %>% # cam also filter by covariate
      filter(name %in% c("direct", "002-Test1", "003-Test1", "004-Test1", "005-Test1", "006-Test1",
                         "007-Test1", "008-Test1", "050-Test1")) %>%
      mutate(name = case_when(
        name == "direct" ~ "01 - direct",
        name == "002-Test1" ~ "02 - baseline",
        name == "003-Test1" ~ "03 - residence",
        name == "004-Test1" ~ "04 - mother_edu",
        name == "005-Test1" ~ "05 - wealth_index",
        name == "006-Test1" ~ "06 - hhd_under5",
        name == "007-Test1" ~ "07 - residence, mother_edu",
        name == "008-Test1" ~ "08 - residence, wealth_index",
        name == "050-Test1" ~ "50",
        TRUE ~ name)
      ) %>%
      ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
      geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
      geom_point(position = position_dodge(width = 0.8)) +
      labs(x = "", y = "") +
      theme_bw() +
      coord_flip() +
      labs(title = myoutcome) +
      theme(text = element_text(size = 14), legend.title=element_blank()) +
      #scale_color_manual(values = wes_palette("Zissou1", n = length(unique(postpred_plot$name)), type = "discrete"))
      scale_color_discrete(guide = guide_legend(reverse = TRUE))
    ggsave(paste0("./gen/visualizations/uncert-int/", outcome,"_", format(Sys.Date(), "%Y%m%d"), ".png"), p, width = 8, height = 20, limitsize = F)
    
  }
  
  # two panels --------------------------------------------------------------
  
  if(two_panel){
    
    # uncertainty intervals for Datadent presentation on may 13, 2025
    # need two panels
    plotdat <- postpred_plot %>%
      filter(name %in% c("direct", "100-Test1")) %>%
      mutate(name = case_when(
        name == "direct" ~ "01 - direct",
        name == "100-Test1" ~ "02 - model, no cov",
        TRUE ~ name)
      ) 
    df_dist <- plotdat %>%
      dplyr::select(ADM2_EN) %>%
      distinct() %>% arrange(ADM2_EN) %>%
      mutate(n = 1:n(),
             total = n(),
             panel = ifelse(n/total<0.5, 1, 2))
    plot1 <- plotdat %>% filter(ADM2_EN %in% subset(df_dist, panel == 1)$ADM2_EN)
    plot2 <- plotdat %>% filter(ADM2_EN %in% subset(df_dist, panel == 2)$ADM2_EN)
    p1 <- plot1 %>%
      ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
      geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
      geom_point(position = position_dodge(width = 0.8)) +
      labs(x = "", y = "") +
      theme_bw() +
      coord_flip() +
      theme(text = element_text(size = 10), legend.title=element_blank()) +
      #scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
      scale_y_continuous(limits = c(min(plotdat$lb), max(plotdat$ub)))
    p2 <- plot2 %>%
      ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
      geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
      geom_point(position = position_dodge(width = 0.8)) +
      labs(x = "", y = "") +
      theme_bw() +
      coord_flip() +
      theme(text = element_text(size = 10), legend.title=element_blank()) +
      #scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
      scale_y_continuous(limits = c(min(plotdat$lb), max(plotdat$ub)))
    
    # Combine p1 and p2
    combined_plot <- (p1 + p2) + 
      plot_layout(guides = "collect") &   # collect shared legend
      theme(legend.position = "bottom")   # move legend bottom
    # Add single title on top
    combined_plot  <- combined_plot + plot_annotation(title = paste0(outlab, " (", myoutcome, ")"))
    
    ggsave(paste0("./gen/visualizations/uncert-int/two-panels_intercept_PCP/", myoutcome,"_", format(Sys.Date(), "%Y%m%d"), ".png"), combined_plot, width = 10, height = 7.5, units = "in", dpi = 300)
  }

}

