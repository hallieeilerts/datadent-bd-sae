

# DHS helper functions --------------------------------------------------------

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




# Spatial helper functions ------------------------------------------------

nb2mat_to_nodes <- function(adj_mat) {
  N_A <- nrow(adj_mat)
  N_edges <- sum(adj_mat != 0) / 2
  n1 <- vector(mode="numeric", length = N_edges)
  n2 <- vector(mode="numeric", length = N_edges)
  k <- 1
  for (i in 1:N_A) {
    for (j in i:N_A) {
      if (adj_mat[i, j] != 0) {
        n1[k] <- i
        n2[k] <- j
        k <- k + 1
      }
    }
  }
  return(list(n1 = n1, n2 = n2))
}

prepare_bym2 <- function(adj_mat) {
  nodes <- nb2mat_to_nodes(adj_mat)
  inla_adj <- sparseMatrix(i = nodes$n1, j = nodes$n2,
                           x = 1, symmetric = T)
  # ICAR precision matrix
  Q <- Diagonal(nrow(adj_mat), Matrix::rowSums(inla_adj)) - inla_adj
  Q_jit = Q + Diagonal(nrow(adj_mat)) * max(diag(Q)) * sqrt(.Machine$double.eps)
  
  Q_inv = inla.qinv(Q_jit, constr=list(A = matrix(1, 1, nrow(adj_mat)), e=0))
  
  #Compute the geometric mean of the variances, which are on the diagonal of Q.inv
  scl = exp(mean(log(diag(Q_inv))))
  return(list(n1 = nodes$n1, n2 = nodes$n2, scaling_factor = scl))
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


fn_prepPR <- function(x){
  
  x$wt <- x$hv005/1000000
  if("hv028" %in% names(x)){
    x$wt_male <- x$hv028/1000000 # hv005 is household weight, hv028 is household weight for male subsample
  }else{
    x$wt_male <- NA
  }
  x$region_name <- haven::as_factor(x$hv024)

  return(x)
}

fn_prepHR <- function(x){
  
  x$wt <- x$hv005/1000000
  x$region_name <- haven::as_factor(x$hv024)
  
  return(x)
}


# KR variables ------------------------------------------------------------


# Children age 6-59 mos given Vit. A supplements
fn_gen_nt_ch_micro_vas	<- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap11_NT/NT_CH_MICRO.R
  
  x$age <- (x$v008 - x$b3)/12
  x$age_months <- x$v008 - x$b3
  
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
  x$nt_ch_micro_vas[!is.na(x$nt_ch_micro_vas) & x$nt_ch_micro_vas == 0] <- 0
  x$nt_ch_micro_vas[!is.na(x$nt_ch_micro_vas) & x$nt_ch_micro_vas == 1] <- 1
  
  #x$var <- x$nt_ch_micro_vas
  #x <- subset(x, !is.na(var))
  
  return(x)
  
}

# Children age 6-59 mos given deworming medication
fn_gen_nt_ch_micro_dwm	<- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap11_NT/NT_CH_MICRO.R
  x$nt_ch_micro_dwm <- 0
  x$nt_ch_micro_dwm[(x$h43 == 1)] <- 1
  x$nt_ch_micro_dwm[(x$age_months <6 | x$age_months > 59 | x$b5 <= 0)] <- NA
  x$nt_ch_micro_dwm[!is.na(x$nt_ch_micro_dwm) & x$nt_ch_micro_dwm == 0] <- 0
  x$nt_ch_micro_dwm[!is.na(x$nt_ch_micro_dwm) & x$nt_ch_micro_dwm == 1] <- 1

  #x$var <- x$nt_ch_micro_dwm
  #x <- subset(x, !is.na(var))
  
  return(x)
  
}

# BD 2022 doesn't have
# Children aged 12-23m
# Rotavirus 3rd dose vaccination according to either source
fn_gen_ch_rotav3_either	<- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap10_CH/CH_VAC.R
  x$age_months <- x$v008 - x$b3
  x <- x %>%
    mutate(agegroup = 
             case_when(
               age_months >=12 & age_months <=23 ~ 1,
               age_months >=24 & age_months <=35 ~ 2)) %>%
    mutate(vacage = ifelse(agegroup==1 & b5==1, 1, 0)) %>% # select age group and live children 
    mutate(rotav1 = case_when(h57 %in% c(1,2,3) ~ 1, h57%in%c(0,8) ~ 0  )) %>%
    mutate(rotav2 = case_when(h58 %in% c(1,2,3) ~ 1, h58%in%c(0,8) ~ 0  )) %>%
    mutate(rotav3 = case_when(h59 %in% c(1,2,3) ~ 1, h59%in%c(0,8) ~ 0  )) %>%
    mutate(rotavsum= rotav1+rotav2+rotav3) %>%
    mutate(ch_rotav3_either = case_when(rotavsum >=3 ~ 1, TRUE ~ 0 )) %>%
    mutate(ch_rotav3_either = ifelse(vacage == 0, NA, ch_rotav3_either))
    
    return(x)
}

fn_gen_ch_meas_either	<- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap10_CH/CH_VAC.R
  x$age_months <- x$v008 - x$b3
  x <- x %>%
    mutate(agegroup = 
             case_when(
               age_months >=12 & age_months <=23 ~ 1,
               age_months >=24 & age_months <=35 ~ 2)) %>%
    mutate(vacage = ifelse(agegroup==1 & b5==1, 1, 0)) %>% # select age group and live children 
    mutate(ch_meas_either = 
           case_when(h9 %in% c(1,2,3) ~ 1, h9 %in% c(0,8) ~ 0)) %>%
    mutate(ch_meas_either = ifelse(vacage == 0, NA, ch_meas_either))
  
  return(x)
}

# Exclusively breastfed - last-born under 6 months
fn_gen_nt_ebf <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap11_NT/NT_IYCF.R
  x <- x %>%
    mutate(nt_bf_curr =
             case_when(
               m4==95  ~ 1 ,
               m4 %in% c(93,94,98,99) ~ 0)) %>%
    mutate(water  = case_when(v409==1  ~ 1 , v409!=1 ~ 0)) %>%
    mutate(liquids= case_when(v409a==1 | v410==1 | v410a==1 | v412c==1 | v413==1 | v413a==1 | v413b==1 | v413c==1 | v413d==1  ~ 1 , 
                              v409a!=1 | v410!=1 | v410a!=1 | v412c!=1 | v413!=1 | v413a!=1 | v413b!=1 | v413c!=1 | v413d!=1 ~ 0)) %>%
    mutate(milk   = case_when(v411==1 | v411a==1 ~ 1 , v411!=1 | v411a!=1 ~ 0)) %>%
    mutate(solids = case_when(v414a==1 | v414b==1 | v414c==1 | v414d==1 | v414e==1 | v414f==1 | v414g==1 | v414h==1 | v414i==1 | 
                                v414j==1 | v414k==1 | v414l==1 | v414m==1 | v414n==1 | v414o==1 | v414p==1 | v414q==1 | v414r==1 | 
                                v414s==1 | v414t==1 | v414u==1 | v414v==1 | v414w==1 | v412a==1 | v412b==1 | m39a==1 ~ 1 ,
                              v414a!=1 | v414b!=1 | v414c!=1 | v414d!=1 | v414e!=1 | v414f!=1 | v414g!=1 | v414h!=1 | v414i!=1 | 
                                v414j!=1 | v414k!=1 | v414l!=1 | v414m!=1 | v414n!=1 | v414o!=1 | v414p!=1 | v414q!=1 | v414r!=1 | 
                                v414s!=1 | v414t!=1 | v414u!=1 | v414v!=1 | v414w!=1 | v412a!=1 | v412b!=1 | m39a!=1~ 0) ) %>%
    mutate(nt_bf_status = case_when(nt_bf_curr==0 ~ 0, solids==1 ~ 5, milk==1 ~ 4, liquids==1 ~3, water==1 ~2, TRUE~1 )) %>%
    mutate(nt_ebf =
             case_when(
               age<6 & nt_bf_status==1  ~ 1 ,
               age<6 & nt_bf_status!=1 ~ 0)) 
  
  return(x)
}


# Mothers who received IYCF counseling in the last 6 months - NEW Indicator in DHS8
fn_gen_nt_counsel_iycf	<- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap11_NT/NT_CH_MICRO.R
  x$nt_counsel_iycf <- 0
  x$nt_counsel_iycf[(x$v486 == 1)] <- 1
  x$nt_counsel_iycf[(x$age_months <6 | x$age_months > 59 | x$b5 <= 0)] <- NA
  x$nt_counsel_iycf[!is.na(x$nt_counsel_iycf) & x$nt_counsel_iycf == 0] <- 0
  x$nt_counsel_iycf[!is.na(x$nt_counsel_iycf) & x$nt_counsel_iycf == 1] <- 1
  
  return(x)
  
}


# IR variables ------------------------------------------------------------

# Number of days women took iron supplements during last pregnancy
fn_gen_nt_wm_micro_iron <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap11_NT/NT_WM_NUT.R
  x$nt_wm_micro_iron <- NA
  x$nt_wm_micro_iron[x$v208 == 0] <- NA
  x$nt_wm_micro_iron[x$m45_1 == 0] <- 0
  x$nt_wm_micro_iron[x$m46_1 < 60] <- 1
  x$nt_wm_micro_iron[x$m46_1 >= 60 & x$m46_1 < 90] <- 2
  x$nt_wm_micro_iron[x$m46_1 >= 90 & x$m46_1 <= 300] <- 3
  x$nt_wm_micro_iron[x$m46_1 >= 998 | x$m45_1 >= 8] <- NA
  
  x$nt_wm_micro_iron[!is.na(x$nt_wm_micro_iron) & x$nt_wm_micro_iron < 3] <-  0  # <90 days
  x$nt_wm_micro_iron[!is.na(x$nt_wm_micro_iron) & x$nt_wm_micro_iron >= 3] <- 1 # 90+ days
   
  #set_value_labels(nt_wm_micro_iron = c("None"=0, "<60"=1, "60-89"=2, "90+"=3, "Don't know/missing"=4))
  
  return(x)
}

# ANC 4+ visits
fn_gen_rh_anc_4vs <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_ANC.R
  x <- x %>%
    mutate(period = 60) %>%
    mutate(age = b19_01) %>%
    mutate(rh_anc_numvs =
             case_when(
               m14_1 == 0 ~ 0 ,
               m14_1 == 1 ~ 1 ,
               m14_1  %in% c(2,3)   ~ 2 ,
               m14_1>=4 & m14_1<=90  ~ 3 ,
               m14_1>90  ~ 9 ,
               age>=period ~ 99 )) %>%
    replace_with_na(replace = list(rh_anc_numvs = c(99))) %>%
    mutate(rh_anc_4vs =
             case_when(
               rh_anc_numvs==3 ~ 1,
               rh_anc_numvs %in% c(0,1,2,9) ~ 0 ))
  
  return(x)
}

# //Number of months pregnant at time of first ANC visit
fn_gen_rh_anc_1tri	<- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_ANC.R
  x <-  x %>%
    mutate(rh_anc_moprg =
                  case_when(
                    m14_1 == 0 ~ 0 ,
                    m13_1  %in% c(0,1,2,3)   ~ 1 ,
                    m13_1  %in% c(4,5)  ~ 2 ,
                    m13_1  %in% c(6,7)~ 3,
                    m13_1>=8 & m13_1<=90 ~ 4, 
                    m13_1>90 & m13_1<100 ~ 9 ,
                    age>=period ~ 99 )) %>%
    replace_with_na(replace = list(rh_anc_moprg = c(99))) %>%
    mutate(rh_anc_1tri = 
             case_when(
               rh_anc_moprg == 1 ~ 1,
               rh_anc_moprg %in% c(0,2,3,4,9) ~ 0)
             )
  return(x)
}

fn_gen_fp_cusm_w_mod <- function(x){
  
}

fn_gen_fp_cusy_w_mod <- function(x){
  
}



# Indicator processing ----------------------------------------------------

# Function to calculate weighted proportions for categorical variables
fn_wtd_prop <- function(x, varname) {
  
  x$var <- varname
  x <- subset(x, !is.na(var))
  
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
fn_wtd_prop2 <- function(x, varname) {
  
  x$varname <- x[,varname]

  x <- subset(x, !is.na(varname))
  
  # Create a list to store results for all admin levels
  results_list <- list()
  
  x_modified <- x %>%
      mutate(district_name = district)
    
    # Compute weighted proportion
    suppressMessages({
      result <- x_modified %>%
        group_by(district_name, varname) %>%
        dplyr::summarise(n = sum(wt), .groups = "drop") %>%
        group_by(district_name) %>%
        mutate(per = n / sum(n)) %>%
        filter(!is.na(per)) %>%
        select(-n)
    })
  
  
  return(result)
}




#x <- dat
#varname <- "ch_rotav3_either"

fn_var_taylor <- function(x, varname, admin_levels = c("adm0", "adm1", "adm2")){

  x$var <- x[,varname]
  x <- subset(x, !is.na(var))
  
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

  final_variance_results$outcome <- varname
  
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
  #x$region_name[x$region_name == "Barishal"] <- "Barisal"
  #x$region_name[x$region_name == "Chattogram"] <- "Chittagong"
  
  x <- x[order(x$admin_level, x$region_name, x$district_name),]
  
  return(x)
  
}

# Predict posterior -------------------------------------------------------

fn_pred <- function(fit, dat, outcome){
  # Generate posterior predictive samples for individual-level predictions
  posterior_preds <- posterior_predict(fit)
  
  # Create a new column to hold the mean of predicted probabilities for each individual
  dat$predictions <- apply(posterior_preds, 2, mean)
  
  # Summarize individual-level predictions at the district level
  district_preds <- dat %>%
    group_by(district) %>%
    summarize(
      value = mean(predictions),
      lower = quantile(predictions, 0.025),  # 2.5% quantile (lower bound)
      upper = quantile(predictions, 0.975)   # 97.5% quantile (upper bound)
    )
  
  district_preds$indicator <- outcome_name
  
  return(district_preds)
}


weighted_quantile <- function(x, w, probs) {
  ord <- order(x)
  x_sorted <- x[ord]
  w_sorted <- w[ord]
  cum_w <- cumsum(w_sorted) / sum(w_sorted)
  approx(cum_w, x_sorted, xout = probs, rule = 2)$y
}

# accounts for survey weights in aggregation to district level
fn_pred_wtd <- function(fit, dat){
  # Generate posterior predictive samples for individual-level predictions
  posterior_preds <- posterior_predict(fit)
  
  # Create a new column to hold the mean of predicted probabilities for each individual
  dat$predictions <- apply(posterior_preds, 2, mean)
  
  # Summarize individual-level predictions at the district level
  if(grepl("weights", paste0(fit$formula)[1])){
    district_preds <- dat %>%
      group_by(district) %>%
      summarise(
        lower =  weighted_quantile(predictions, wt, 0.025),
        value =  weighted_quantile(predictions, wt, 0.50),
        upper =  weighted_quantile(predictions, wt, 0.975)
      )
  }else{
    district_preds <- dat %>%
      group_by(district) %>%
      summarise(
        lower =  quantile(predictions, 0.025),
        value =  quantile(predictions, 0.50),
        upper =  quantile(predictions, 0.975)
      )
  }
  
  return(district_preds)
}

fn_pred2 <- function(fit, dat){

  individual_draws <- dat %>%
    add_epred_draws(fit)
  
  if(grepl("weights", paste0(fit$formula)[1])){
    
    district_preds <- individual_draws %>%
      group_by(district, .draw) %>%
      summarise(mean_pred = weighted.mean(.epred, wt), .groups = "drop")
    
  }else{
    
    district_preds <- individual_draws %>%
      group_by(district, .draw) %>%
      summarise(mean_pred = mean(.epred), .groups = "drop")
    
  }
  
  district_summary <- district_preds %>%
    group_by(district) %>%
    summarise(
      lower = quantile(mean_pred, 0.025),
      value = quantile(mean_pred, 0.5),
      upper = quantile(mean_pred, 0.975)
    )
  
  
  return(district_summary)
  
}

fn_areal_pred <- function(fit, dat){
  
  # get posterior predictions
  posterior_preds <- posterior_predict(fit)
  
  # For example, you can compute the mean and credible intervals (e.g., 95% CI) of the predictions:
  pred_summary <- apply(posterior_preds, 2, function(x) {
    c(mean = mean(x), lower = quantile(x, 0.025), upper = quantile(x, 0.975))
  })
  row.names(pred_summary) <- c("mean", "lower", "upper")

  pred_summary_df <- data.frame(
    district = dat$district,
    lower = pred_summary["lower", ],
    value = pred_summary["mean", ],
    upper = pred_summary["upper", ]
  )
  
  
  return(pred_summary_df)  
}
