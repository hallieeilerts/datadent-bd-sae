

# Helper functions --------------------------------------------------------

# Configuration of covariate constants
# Note: scale is output scale
configure_covariate <- function(name, label, scale, dhs_indicator_code){
  
  # Safety check: warn about unknown scales
  allowed_scales = c("continuous", "continuous_from0", "continuous_from1", "zero_hundred", "zero_one")
  if (!scale  %in% allowed_scales) {
    stop(str_glue("Unknown scale '{scale}'"))
  }
  
  result <- c()
  result[["name"]] <- name
  result[["label"]] <- label
  result[["scale"]] <- scale
  result[["dhs_indicator_code"]] <- dhs_indicator_code
  result
}

write_output_data <- function(df, covariate_metadata) {
  filename_data <- str_glue("./gen/indicators/temp/{covariate_metadata$name}_data.csv")
  filename_meta <- str_glue("./gen/indicators/temp/{covariate_metadata$name}_metadata.json")
  
  # Write data file (csv)
  write.csv(df, filename_data, row.names=FALSE)
  
  # Write metadata file (json)
  cat(
    jsonlite::toJSON(covariate_metadata, auto_unbox=TRUE, pretty=TRUE),
    file = filename_meta,
    sep = "\n",
    append = FALSE
  )
}



# Data prep ---------------------------------------------------------------

fn_prepKR <- function(x){
  
  x$wt <- x$v005/1000000
  if("v024" %in% names(x)){
    x$region_name <- haven::as_factor(x$v024)
  }else{
    x$region_name <- haven::as_factor(x$v101)
  }
  x$sex <- as.character(haven::as_factor(x$b4))
  x$sex[x$sex == "male"] <- "M"
  x$sex[x$sex == 1] <- "M"
  x$sex[x$sex == "female"] <- "F"
  x$sex[x$sex == 2] <- "M"
  x$age <- (x$v008 - x$b3)/12
  x$age_months <- x$v008 - x$b3
  
  return(x)
}


# Indicators --------------------------------------------------------------

# Children age 6-59 mos given Vit. A supplements
fn_gen_nt_ch_micro_vas	<- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap11_NT/NT_CH_MICRO.R
  
  x$h33m2 <- x$h33m
  x$h33m2[x$h33m2 == 98] <- NA
  x$h33d2 <- ifelse(x$h33d==98, 15, as.numeric(x$h33d))
  x$h33y2 <- x$h33y
  x$h33y2[x$h33y2 == 9998] <- NA
  x$Date <- as.Date(paste(x$h33y2, x$h33m2, x$h33d2,sep="-"),"%Y-%m-%d")
  x$mdyc <- as.integer((x$v008a - (difftime(x$Date, "1960-01-01", units = "days") + 21916)) /30.4375)
  
  x$nt_ch_micro_vas <- 0
  x$nt_ch_micro_vas[(x$age_months >=6 & x$age_months <= 59) & (x$h34 ==1 | x$mdyc <= 6)] <- 1
  x$nt_ch_micro_vas[!(x$age_months >=6 & x$age_months <= 59) | x$b5 == 0 ] <- NA
  x$nt_ch_micro_vas[!is.na(x$nt_ch_micro_vas) & x$nt_ch_micro_vas == 0] <- "No"
  x$nt_ch_micro_vas[!is.na(x$nt_ch_micro_vas) & x$nt_ch_micro_vas == 1] <- "Yes"
  
  x$var <- x$nt_ch_micro_vas
  x <- subset(x, !is.na(var))
  return(x)
  
}

# Children age 6-59 mos given deworming medication
fn_nt_ch_micro_dwm	<- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap11_NT/NT_CH_MICRO.R
  
}


# Function to calculate weighted proportions for categorical variables
fn_wtd_prop <- function(x, covariate_metadata, adminlevel = "adm0"){
  
  if(adminlevel == "adm0"){
    x <- x %>% 
      mutate(admin_level = adminlevel,
             region_name = NA,
             district_name = NA) 
  }
  if(adminlevel == "adm1"){
    x <- x %>% 
      mutate(admin_level = adminlevel,
             district_name = NA) 
  }
  if(adminlevel == "adm2"){
    x <- x %>% 
      mutate(admin_level = adminlevel,
             district_name = district) 
  }
  
  # Calculate weighted proportion for each category of variable
  suppressMessages(
    result <- x %>%
      group_by(admin_level, region_name, district_name, var) %>%
      dplyr::summarise(n = sum(wt)) %>%
      mutate(per = n /sum(n)) %>%
      filter(!is.na(per)) %>% # In some regions, wt is always zero (PK2017DHS). Then per will be NA.
      select(-c(n))
  )
  
  return(result)
}

fn_format <- function(x, covariate_metadata){
  
  names(x)[names(x) == "per"] <- "value"
  x$indicator <- covariate_metadata$name
  
  x$dhs_indicator_code <- NA
  if(length(covariate_metadata$sex_categories) > 1){
    x$dhs_indicator_code[x$sex == "M"] <- covariate_metadata$dhs_indicator_code[1]
    x$dhs_indicator_code[x$sex == "F"] <- covariate_metadata$dhs_indicator_code[2]
    x$dhs_indicator_code[x$sex == "MF"] <- covariate_metadata$dhs_indicator_code[3]
  }else{
    x$dhs_indicator_code <- covariate_metadata$dhs_indicator_code[1]
  }
  
  x <- x %>% relocate(any_of(c("dhs_indicator_code","admin_level", 
                               "region_name", "district_name",
                                "value")), .after = indicator)
  
  x <- x[order(x$admin_level, x$region_name, x$district_name),]
  
  return(x)
  
}

