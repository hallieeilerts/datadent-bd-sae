

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
  filename_data <- str_glue("./gen/indicators/output/{covariate_metadata$name}_data.csv")
  filename_meta <- str_glue("./gen/indicators/output/{covariate_metadata$name}_metadata.json")
  
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

# deletes and recreates the data sub-folder
int_data_subfolder <- function(subfolder_name) {
  # ⚠ - Delete the entire data sub-directory to make sure we don't have old data
  if (unlink(str_glue("./data/{subfolder_name}"), recursive = TRUE) == 1) {
    stop(str_glue("Failed to delete data. Make sure you don't have any files open from that directory."))
  }
  
  # Create data fodler
  dir.create(str_glue("./data/{subfolder_name}"))
}

# deletes file
delete_file <- function(path) {
  if (unlink(path, recursive = TRUE) == 1) {
    stop(str_glue("Failed to delete {filename_data}. Please close any applications that might be using the file."))
  }
}

# Returns current time in iso86001 format
current_time_iso86001 <- function() {        
  time <- as.POSIXlt(Sys.time(), "UTC")
  strftime(time, "%Y-%m-%dT%H:%M:%S%z")
}

# Fetches data from a DHS indicator
read_dhs <- function(dhs_indicator_code) {
  api_url <-
    str_glue(
      "https://api.dhsprogram.com/rest/dhs/data/{dhs_indicator_code}?perpage=1000000"
    )
  json_file <- jsonlite::fromJSON(api_url)
  
  # Verify that we got the entire dataset
  if (json_file$RecordsReturned != json_file$RecordCount) {
    stop("Did not get all records, implement pagination")
  }
  
  # Unlist the JSON file entries
  json_data <- lapply(json_file$Data, function(x) {
    unlist(x)
  })
  
  # Convert JSON input to a data frame
  result <-
    as_tibble(t(as.data.frame(
      do.call("rbind", json_data), stringsAsFactors = FALSE
    )))
  
  dir.create("./data/dhs", showWarnings = FALSE)
  filename_data <- str_glue("./data/dhs-statcompiler/{dhs_indicator_code}.csv")
  filename_meta <- str_glue("./data/dhs-statcompiler/{dhs_indicator_code}.json")
  
  # Delete previous files
  delete_file(filename_data)
  delete_file(filename_meta)
  
  # Indicator description
  indicator_description <- unique(result$Indicator)
  
  # Write input file
  write.csv(result, filename_data, row.names=FALSE)
  # Write json
  cat(
    '{',
    str_glue(' "label": "GHO ({dhs_indicator_code})",'),
    str_glue(' "source_name": "DHS",'),
    str_glue(' "source_indicator_code": "{dhs_indicator_code}",'),
    str_glue(' "source_indicator_description": "{indicator_description}",'),
    str_glue(' "source_url": "{api_url}",'),
    str_glue(' "time_downloaded": "{current_time_iso86001()}"'),
    '}',
    file = filename_meta,
    sep = "\n",
    append = FALSE
  )
  
  result
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
fn_wtd_prop <- function(x) {
  
  # Create a list to store results for all admin levels
  results_list <- list()
  
  # Loop through all admin levels
  for (adminlevel in c("adm0", "adm1", "adm2")) {
    
    # Add appropriate admin level columns
    x_modified <- x %>% mutate(admin_level = adminlevel)
    
    if (adminlevel == "adm0") {
      x_modified <- x_modified %>%
        mutate(region_name = NA,
               district_name = NA)
    }
    
    if (adminlevel == "adm1") {
      x_modified <- x_modified %>%
        mutate(district_name = NA)
    }
    
    if (adminlevel == "adm2") {
      x_modified <- x_modified %>%
        mutate(district_name = district)
    }
    
    # Compute weighted proportion
    suppressMessages({
      result <- x_modified %>%
        group_by(admin_level, region_name, district_name, var) %>%
        dplyr::summarise(n = sum(wt), .groups = "drop") %>%
        group_by(admin_level, region_name, district_name) %>%
        mutate(per = n / sum(n)) %>%
        filter(!is.na(per)) %>%  # Handle cases where wt is always zero
        filter(var == "Yes") %>%
        select(-var)
    })
    
    # Store results in list
    results_list[[adminlevel]] <- result
  }
  
  # Combine results for all levels into one dataframe
  final_result <- bind_rows(results_list)
  
  return(final_result)
}

fn_var_taylor <- function(x, admin_levels = c("adm0", "adm1", "adm2")){

  # List to store variance results for each admin level
  results_list <- list()

  for (adminlevel in admin_levels) {
    
    # Modify data according to admin level
    x_modified <- x %>% mutate(admin_level = adminlevel)
    
    if (adminlevel == "adm0") {
      x_modified <- x_modified %>%
        mutate(region_name = "none",
               district_name = "none")
    }
    
    if (adminlevel == "adm1") {
      x_modified <- x_modified %>%
        mutate(district_name = "none")
    }
    
    if (adminlevel == "adm2") {
      x_modified <- x_modified %>%
        mutate(district_name = district)
    }
    
    # Convert categorical variable to numeric
    x_modified <- x_modified %>%
      mutate(var_numeric = ifelse(var=="Yes", 1, 0))
    
    # Define survey design using Taylor Linearization
    survey_design <- svydesign(
      id = ~v021,       # Primary Sampling Unit (PSU)
      strata = ~v022,   # Stratification variable
      weights = ~wt,    # Survey weights
      data = x_modified,
      nest = TRUE       # Accounts for stratification
    )
    
    # Define survey design
    survey_design <- svydesign(
      id = ~v021,       # Primary Sampling Unit (PSU)
      strata = ~v022,   # Stratification variable
      weights = ~wt,    # Survey weights
      data = x_modified,
      nest = TRUE       # Accounts for stratification
    )
    
    # Taylor series linearization
    # Compute the weighted proportion and variance using svymean
    taylor_results <- svyby(
      formula = ~var_numeric,  # variable of interest
      by = ~admin_level + region_name + district_name,  # grouping
      design = survey_design,
      FUN = svymean,               # Compute mean (proportion)
      vartype = "se"               # Get standard error (SE)
    ) %>%
      mutate(variance = as.numeric(se)^2) %>%
      rename(value = var_numeric)
    
    # Store results
    results_list[[adminlevel]] <- taylor_results
  }

  # Combine results into a single dataframe
  final_variance_results <- bind_rows(results_list)

  return(final_variance_results)

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

fn_format_var <- function(x, covariate_metadata){
  
  x$strata <- row.names(x)
  row.names(x) <- NULL
  x$indicator <- covariate_metadata$name
  
  x$dhs_indicator_code <- NA
  if(length(covariate_metadata$sex_categories) > 1){
    x$dhs_indicator_code[x$sex == "M"] <- covariate_metadata$dhs_indicator_code[1]
    x$dhs_indicator_code[x$sex == "F"] <- covariate_metadata$dhs_indicator_code[2]
    x$dhs_indicator_code[x$sex == "MF"] <- covariate_metadata$dhs_indicator_code[3]
  }else{
    x$dhs_indicator_code <- covariate_metadata$dhs_indicator_code[1]
  }
  
  x <- x %>% relocate(any_of(c("dhs_indicator_code", "strata",
                               "admin_level", 
                               "region_name", "district_name",
                               "value", "se", "variance")), .after = indicator)
  

  x$district_name[x$district_name == "none"] <- NA
  
  # format region name to match shape files
  x$region_name <- paste(toupper(substr(x$region_name, 1, 1)), substr(x$region_name, 2, nchar(x$region_name)), sep="")
  x$region_name[x$region_name == "None"] <- NA
  x$region_name[x$region_name == "Barishal"] <- "Barisal"
  x$region_name[x$region_name == "Chattogram"] <- "Chittagong"
  
  x <- x[order(x$admin_level, x$region_name, x$district_name),]
  
  return(x)
  
}

