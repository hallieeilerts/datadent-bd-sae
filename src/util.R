

# DHS helper functions --------------------------------------------------------

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

# Validation helper functions ---------------------------------------------

check_overlap <- function(xmin, xmax, ymin, ymax) {
  # Ensure inputs are vectors of the same length
  if (length(xmin) != length(xmax) || length(ymin) != length(ymax)) {
    stop("Input vectors must have the same length.")
  }
  
  # Check for NA values
  overlap <- ifelse(
    is.na(xmin) | is.na(xmax) | is.na(ymin) | is.na(ymax),
    NA, # Return NA if any input in the pair is NA
    !(xmax < ymin | ymax < xmin) # Otherwise, compute overlap
  )
  
  return(overlap)
}


# Phantom household -------------------------------------------------------


## NEED TO MAKE SURE THESE CALCULATED APPROPRIATELY
#degf_ph
#n_obs_ph

# function to add phantom household when calculating direct estimates and process data frame for analysis
sae_df <- function(df, est_orig, geo_level, strata, 
                   add_phantom = FALSE, geo_level_upper = NULL){
 
  # requires library(survey)
  
  strata_formula <- as.formula(paste("~", paste(strata, collapse = " + ")))
  
  # df_aggregates should have naive, naive_var, design_based_mean, design_based_var, n_obs (number of clusters), degf (cluster minus 1)
  df_aggregates <- est_orig %>% filter(variable %in% myvar)
  
  # calculate cluster level variance
  df_cluster_level <- dhs %>% 
    group_by(pick(unique(c(geo_level_upper, geo_level, strata, "v001")))) %>%
    summarise(cluster_mean = mean(.data[[var_sym]], na.rm = TRUE),
              cluster_count = sum(.data[[var_sym ]], na.rm = TRUE),
              cluster_weight = sum(wt),
              #n_hh = n(),
              n_hh = sum(!is.na(.data[[var_sym]]))
    ) %>% 
    ungroup() %>%
    mutate(ea = as.character(v001),
           strata_name = paste(.data[[strata[1]]])
    )
  df_cluster_level_original <- df_cluster_level
  
  # identify adm2/strata combos with one unique cluster mean
  df_geo_phantom <- df_cluster_level %>%  
    group_by(pick(unique(c(geo_level_upper, geo_level, strata)))) %>%
    summarise(n_values = n_distinct(cluster_mean)) %>%
    filter(n_values == 1) %>%
    select(all_of(unique(c(geo_level_upper,geo_level,strata)))) %>%
    mutate(strata_name = paste(.data[[strata[1]]]))
  
  strata_list <- unique(df_geo_phantom[["strata_name"]])
  j = 0
  for(each_stratum in strata_list){
    
    #each_stratum <- "7"
    geo_phantom_list <- df_geo_phantom[[geo_level]][df_geo_phantom[["strata_name"]]==each_stratum]
    
    df_tmp_to_add <- df_cluster_level %>%
      filter(strata_name == each_stratum) %>%
      group_by(pick(unique(c(geo_level_upper, strata, "strata_name")))) %>%
      summarise(cluster_mean = weighted.mean(cluster_mean, cluster_weight, na.rm = TRUE),
                cluster_weight = sum(cluster_weight, na.rm = TRUE)/sum(n_hh),
                n_hh = 1)
    
    for (each_geo in geo_phantom_list){
      
      #each_geo <- "Bagerhat"
      j = j + 1
      df_cluster_level <- bind_rows(df_cluster_level,
                                    (df_tmp_to_add %>%
                                       mutate(!!geo_level := each_geo,
                                              ea = paste0("phantom",as.character(j)))
                                    )
      )
    }
  }
  
  # cluster level survey design
  df_cluster_level_svy <- svydesign(
    ids = ~ea,
    strata = strata_formula,  
    weights = ~cluster_weight,
    data = df_cluster_level,
    nest = TRUE
  )
  
  df_aggregates_tmp <- svyby(formula = ~cluster_mean, 
                             by = as.formula(paste("~",geo_level)),
                             design = df_cluster_level_svy,
                             FUN = svymean,
                             na.rm = TRUE,
                             vartype = "var") %>%
    rename(design_based_ph_mean = cluster_mean, 
           design_based_ph_var = var) %>%
    left_join(
      df_cluster_level %>% 
        group_by(pick(geo_level)) |>
        summarise(degf_ph = n() - 1,    # number of clusters (including phantom cluster if added) minus 1
                  n_obs_ph = sum(n_hh)) # number of households (does not correspond with n_obs, which is number of clusters)
    )
  df_aggregates <- df_aggregates %>% left_join(df_aggregates_tmp, by = geo_level)
  
  return((list(df_aggregates, df_cluster_level, df_cluster_level_original)))
   
}

# KR variables ------------------------------------------------------------

# //Children age 6-59 mos given iron supplements
fn_gen_nt_ch_micro_iron <- function(x){
  
  # h42: Taking iron pills, sprinkles with iron or iron syrup in the last 7 days
  x$age_months <- x$v008 - x$b3
  x <- x %>%
       mutate(nt_ch_micro_iron =
               case_when(
                 age_months < 6 | age_months > 23 | b5==0 ~ 99, 
                 h42!=1 ~ 0 ,
                 h42==1 ~ 1 )) %>%
       replace_with_na(replace = list(nt_ch_micro_iron = c(99))) 
  return(x)
  
}

# //Children age 6-59 mos given Vit. A supplements
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
  x$nt_ch_micro_vas[(x$age_months >=6 & x$age_months <= 23) & (x$h34 ==1 | x$mdyc <= 6)] <- 1
  x$nt_ch_micro_vas[!(x$age_months >=6 & x$age_months <= 23) | x$b5 == 0 ] <- NA
  x$nt_ch_micro_vas[!is.na(x$nt_ch_micro_vas) & x$nt_ch_micro_vas == 0] <- 0
  x$nt_ch_micro_vas[!is.na(x$nt_ch_micro_vas) & x$nt_ch_micro_vas == 1] <- 1
  
  #x$var <- x$nt_ch_micro_vas
  #x <- subset(x, !is.na(var))
  
  return(x)
  
}

# //Children age 6-23 mos given deworming medication
fn_gen_nt_ch_micro_dwm	<- function(x){
  
  x$age_months <- x$v008 - x$b3
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap11_NT/NT_CH_MICRO.R
  x$nt_ch_micro_dwm <- 0
  x$nt_ch_micro_dwm[(x$h43 == 1)] <- 1
  x$nt_ch_micro_dwm[(x$age_months <6 | x$age_months > 23 | x$b5 <= 0)] <- NA
  x$nt_ch_micro_dwm[!is.na(x$nt_ch_micro_dwm) & x$nt_ch_micro_dwm == 0] <- 0
  x$nt_ch_micro_dwm[!is.na(x$nt_ch_micro_dwm) & x$nt_ch_micro_dwm == 1] <- 1

  #x$var <- x$nt_ch_micro_dwm
  #x <- subset(x, !is.na(var))
  
  return(x)
  
}

# //Exclusively breastfed - last-born under 6 months
fn_gen_nt_ebf <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap11_NT/NT_IYCF.R
  x$age_months <- x$v008 - x$b3
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
               age_months<6 & nt_bf_status==1  ~ 1 ,
               age_months<6 & nt_bf_status!=1 ~ 0)) 
  
  return(x)
}

# //Child with minimum dietary diversity- last-born 6-23 months
# v414, v411, v410, v412, v413, v000, m4
fn_gen_nt_mdd	<- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap11_NT/NT_IYCF.R
  x$age_months <- x$v008 - x$b3
  x <- x %>%
    # nt_formula, nt_milk, nt_dairy, nt_grains, nt_root, nt_bbyfood, nt_vita, nt_frtveg, nt_eggs, nt_meatfish, nt_nuts
    # country specific foods. These can be added to the foods below based on the survey. See example for nt_root & nt_meatfish below
    mutate(food1  = case_when(v414a==1  ~ 1 , v414a!=1 ~ 0)) %>%
    mutate(food2  = case_when(v414b==1  ~ 1 , v414a!=1 ~ 0)) %>%
    mutate(food3  = case_when(v414c==1  ~ 1 , v414a!=1 ~ 0)) %>%
    mutate(food4  = case_when(v414d==1  ~ 1 , v414a!=1 ~ 0)) %>%
    mutate(nt_formula  = case_when(v411a==1  ~ 1 , v411a!=1~ 0)) %>% # Given formula
    mutate(nt_milk  = case_when(v411==1  ~ 1 , v411!=1~ 0)) %>% # Given other milk
    mutate(nt_liquids= case_when(v410==1 | v412c==1 | v413==1  ~ 1 , v410!=1 | v412c!=1 | v413!=1  ~ 0)) %>% # Given other liquids
    mutate(nt_bbyfood  = case_when(v412a==1  ~ 1 , v412a!=1~ 0)) %>% # Given fortified baby food
    mutate(nt_grains  = case_when(v412a==1 | v414e==1 ~ 1 , v412a!=1 | v414e!=1 ~ 0)) %>% # Given grains
    mutate(nt_vita = case_when(v414i==1 | v414j==1 | v414k==1 ~ 1 , v414i!=1 | v414j!=1 | v414k!=1 ~ 0)) %>% # Given Vit A rich foods
    mutate(nt_frtveg  = case_when(v414l==1  ~ 1 , v414l!=1~ 0)) %>% # Given other fruits or vegetables
    mutate(nt_root  = case_when(v414f==1 | food1==1 ~ 1, v414f!=1 ~ 0)) %>% # Given roots or tubers
    mutate(nt_nuts  = case_when(v414o==1  ~ 1 , v414o!=1~ 0)) %>% # Given nuts or legumes
    mutate(nt_meatfish  = case_when( (v414h==1 |v414m==1 |v414n==1| food2==1) ~ 1,  
                                     !(v414h==1 | v414m==1 | v414n==1) ~ 0)) %>% # Given meat, fish, shellfish, or poultry  
    mutate(nt_eggs  = case_when(v414g==1  ~ 1 , v414g!=1~ 0)) %>% # Given eggs
    mutate(nt_dairy  = case_when(v414p==1 | v414v==1 ~ 1 , v414p!=1 | v414v!=1 ~ 0)) %>% # Given dairy
    mutate(nt_solids = case_when( nt_bbyfood==1 | nt_grains==1 | nt_vita==1 | nt_frtveg==1 | nt_root==1 | nt_nuts==1 | nt_meatfish==1 | 
                                    nt_eggs==1 | nt_dairy==1 | v414s==1 ~ 1 ,
                                  nt_bbyfood!=1 | nt_grains!=1 | nt_vita!=1 | nt_frtveg!=1 | nt_root!=1 | nt_nuts!=1 | nt_meatfish!=1 | 
                                    nt_eggs!=1 | nt_dairy!=1 | v414s!=1 ~ 0) ) %>%
    # 1. breastmilk
    mutate(group1 = case_when(m4==95  ~ 1 ,  m4!=95 ~ 0)) %>% 
    #2. infant formula, milk other than breast milk, cheese or yogurt or other milk products
    mutate(group2 = case_when(nt_formula==1 | nt_milk==1 | nt_dairy==1  ~ 1 , nt_formula!=1 | nt_milk!=1 | nt_dairy!=1 ~ 0)) %>%
    #3. foods made from grains, roots, tubers, and bananas/plantains, including porridge and fortified baby food from grains
    mutate(group3  = case_when(nt_grains==1 | nt_root==1 | nt_bbyfood==1 ~ 1 , nt_grains!=1 | nt_root!=1 | nt_bbyfood!=1 ~ 0)) %>%
    #4. vitamin A-rich fruits and vegetables
    mutate(group4  = case_when(nt_vita==1  ~ 1 , nt_vita!=1 ~ 0)) %>%
    #5. other fruits and vegetables
    mutate(group5  = case_when(nt_frtveg==1 ~ 1 , nt_frtveg!=1~ 0)) %>% 
    #6. eggs
    mutate(group6  = case_when(nt_eggs==1 ~ 1 , nt_eggs!=1~ 0)) %>% 
    #7. meat, poultry, fish, and shellfish (and organ meats)
    mutate(group7  = case_when(nt_meatfish==1 ~ 1 , nt_meatfish!=1~ 0)) %>% 
    #8. legumes and nuts
    mutate(group8  = case_when(nt_nuts==1 ~ 1 , nt_nuts!=1~ 0)) %>% 
    #add the food groups
    mutate(foodsum  = group1+group2+group3+group4+group5+group6+group7+group8) %>% 
    mutate(nt_mdd  = case_when(age_months %in% 6:23 & foodsum<5 ~ 0 , age_months %in% 6:23 & foodsum>=5~ 1))
    
    #older surveys are 4 out of 7 food groups so the foodsum would add group2-group8 and the recode the sum for 4+ as yes
    # set_value_labels(nt_mdd = c("Yes" = 1, "No"=0  )) %>%
    # set_variable_labels(nt_mdd = "Child with minimum dietary diversity, 5 out of 8 food groups- last-born 6-23 months")
  
  return(x)
  
}

# // Children age 6-23 mos given multiple micronutrient powder
fn_gen_nt_ch_micro_mp <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/fe6659a1d3af7e358d158bc814e38c96bf6f5161/Chap11_NT/NT_CH_MICRO.R
  x <- x %>%
    mutate(age_months  = v008 - b3) %>%
    mutate(nt_ch_micro_mp =
             case_when(
               age_months<6 | age_months>23 | b5==0 ~ 99, 
               h80a!=1 ~ 0 ,
               h80a==1 ~ 1 )) %>%
    replace_with_na(replace = list(nt_ch_micro_mp = c(99))) 
  
  return(x)
  
}

# //Given zinc for diarrhea
# Percentage of children born in the five (or three) years preceding the survey with diarrhea in the two weeks preceding the survey who received zinc supplements
fn_gen_ch_diar_zinc <- function(x){
  
  # adding age limitation
  x$age <- (x$v008 - x$b3)/12
  x$age_months <- x$v008 - x$b3
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/fe6659a1d3af7e358d158bc814e38c96bf6f5161/Chap10_CH/CH_DIAR.R
  # //Diarrhea symptoms # //Zinc
  x <- x %>%
    mutate(ch_diar = 
             case_when(
               (h11==1 | h11==2) & b5==1 ~ 1,
               b5==1 ~ 0  )) %>%
    mutate(ch_diar_zinc =
             case_when(
               ch_diar==1 & h15e==1 & age_months < 24 ~ 1 ,
               ch_diar==1 & age_months < 24 ~ 0)) 
  
  return(x)
  
}

# //Given ORS for diarrhea
# Percentage of children born in the five (or three) years preceding the survey with diarrhea in the two weeks preceding the survey who received oral rehydration solution (ORS), that is either fluid from an ORS packet or a pre-packaged ORS fluid
fn_gen_ch_diar_ors <- function(x){
  
  # adding age limitation
  x$age <- (x$v008 - x$b3)/12
  x$age_months <- x$v008 - x$b3
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/fe6659a1d3af7e358d158bc814e38c96bf6f5161/Chap10_CH/CH_DIAR.R
  # //Diarrhea symptoms # //Zinc
  x <- x %>%
    mutate(ch_diar = 
             case_when(
               (h11==1 | h11==2) & b5==1 ~ 1,
               b5==1 ~ 0  )) %>%
    mutate(ch_diar_ors =
             case_when(
               ch_diar==1 & (h13==1 | h13==2 | h13b==1) & age_months < 24 ~ 1 ,
               ch_diar==1 & age_months < 24 ~ 0)) 
  
  return(x)
  
}

# //All basic vaccinations according to either source
# BD2017DHS
# https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap10_CH/CH_VAC.R
fn_gen_ch_allvac_either <- function(x){
  
  x <- x %>%
    mutate(age_months = b19) %>%
    mutate(agegroup = 
             case_when(
               age_months >=12 & age_months <=23 ~ 1,
               age_months >=24 & age_months <=35 ~ 2)) %>%
    filter(agegroup==1 & b5==1) %>% # select age group and live children 
    mutate(ch_bcg_either = case_when(h2%in%c(1,2,3) ~ 1, h2%in%c(0,8) ~ 0  )) %>%
    mutate(dpt1 = case_when(h3%in%c(1,2,3) ~ 1, h3%in%c(0,8) ~ 0  )) %>%
    mutate(dpt2 = case_when(h5%in%c(1,2,3) ~ 1, h5%in%c(0,8) ~ 0  )) %>%
    mutate(dpt3 = case_when(h7%in%c(1,2,3) ~ 1, h7%in%c(0,8) ~ 0  )) %>%
    mutate(dptsum = dpt1 + dpt2 + dpt3) %>%
    mutate(ch_pent3_either = case_when(dptsum >=3 ~ 1, TRUE ~ 0  )) %>%
    mutate(polio1 = case_when(h4%in%c(1,2,3) ~ 1, h4%in%c(0,8) ~ 0  )) %>%
    mutate(polio2 = case_when(h6%in%c(1,2,3) ~ 1, h6%in%c(0,8) ~ 0  )) %>%
    mutate(polio3 = case_when(h8%in%c(1,2,3) ~ 1, h8%in%c(0,8) ~ 0  )) %>%
    mutate(poliosum=polio1 + polio2 + polio3) %>%
    mutate(ch_polio3_either = case_when(poliosum >=3 ~ 1, TRUE ~ 0  )) %>%
    mutate(ch_meas_either = 
             case_when(h9%in%c(1,2,3) ~ 1, h9%in%c(0,8)   ~ 0  )) %>%
    mutate(ch_allvac_either = 
           case_when(ch_bcg_either == 1 & ch_pent3_either == 1 & ch_polio3_either == 1 & ch_meas_either == 1 ~ 1, 
                     TRUE ~ 0)) 
  
  return(x)
  
}

# Children 0-59m who had their growth monitored (previous 3 months)
# BD2017DHS
# https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Child_Growth_Monitoring.htm
# Couldn't find code on DHS github, came up with code on my own. This doesn't work for 2017 which is DHS-7, not DHS-8.
# fn_gen_nt_ch_gwmt_any <- function(x){
#   
#   x <- x %>%
#     mutate(age_months = b19) %>%
#     mutate(nt_ch_gwmt_any =
#              case_when(
#                age_months >59 | b5==0 ~ 99, # ineligible if older than 59m or not alive
#                h70a==1|h70b==1|h70c==1 ~ 1,
#                (h70a!=1 & h70b!=1 & h70c != 1) ~ 0)) %>%
#     replace_with_na(replace = list(nt_ch_gwmt_any = c(99))) 
#   
#   
#   return(x)
#   
# }
fn_gen_nt_ch_gwmt_any <- function(x){
  
  x <- x %>%
    mutate(age_months = b19) %>%
    mutate(nt_ch_gwmt_any =
             case_when(
               age_months > 23 | b5==0 ~ 99, # ineligible if older than 23m or not alive
               s321c == 1 ~ 1,
               s321c != 1 ~ 0)) %>%
    replace_with_na(replace = list(nt_ch_gwmt_any = c(99))) 
  
  
  return(x)
  
}



# KR variables - missing --------------------------------------------------

# //Mothers who received IYCF counseling in the last 6 months - NEW Indicator in DHS8
fn_gen_nt_counsel_iycf	<- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap11_NT/NT_CH_MICRO.R
  x$age_months <- x$v008 - x$b3
  x$nt_counsel_iycf <- 0
  x$nt_counsel_iycf[(x$v486 == 1)] <- 1
  x$nt_counsel_iycf[(x$age_months <6 | x$age_months > 59 | x$b5 <= 0)] <- NA
  x$nt_counsel_iycf[!is.na(x$nt_counsel_iycf) & x$nt_counsel_iycf == 0] <- 0
  x$nt_counsel_iycf[!is.na(x$nt_counsel_iycf) & x$nt_counsel_iycf == 1] <- 1
  
  return(x)
  
}

# BD 2022 doesn't have
# //Pentavalent 3rd dose vaccination according to either source
# children 12-23m
fn_gen_ch_pent3_either <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap10_CH/CH_VAC.R
  x <- x %>%
    mutate(age_months = b19) %>%
    mutate(agegroup = 
             case_when(
               age_months >=12 & age_months <=23 ~ 1,
               age_months >=24 & age_months <=35 ~ 2)) %>%
    filter(agegroup==1 & b5==1) %>% # select age group and live children 
    mutate(dpt1 = case_when(h3%in%c(1,2,3) ~ 1, h3%in%c(0,8) ~ 0  )) %>%
    mutate(dpt2 = case_when(h5%in%c(1,2,3) ~ 1, h5%in%c(0,8) ~ 0  )) %>%
    mutate(dpt3 = case_when(h7%in%c(1,2,3) ~ 1, h7%in%c(0,8) ~ 0  )) %>%
    mutate(dptsum = dpt1 + dpt2 + dpt3) %>%
    mutate(ch_pent3_either = case_when(dptsum >=3 ~ 1, TRUE ~ 0 ))
  
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

# BD 2022 doesn't have
# Children aged 12-23m
# Measles vaccination
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

# //Children age 6-59 months living in household with iodized salt 
# Percentage of children under five living in households using adequately iodized salt
fn_gen_nt_ch_micro_iod <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/fe6659a1d3af7e358d158bc814e38c96bf6f5161/Chap11_NT/NT_CH_MICRO.R
  x <- x %>%
    mutate(age = b19) %>%
    mutate(nt_ch_micro_iod =
             case_when(
               age<6 | age>59 | b5==0 | hv234a>1 ~ 99,
               hv234a==0   ~ 0, 
               hv234a==1  ~ 1)) %>%
    replace_with_na(replace = list(nt_ch_micro_iod = c(99)))
  
  return(x)
}


# Child drank or ate vitamin or mineral supplements yesterday


# BR variables ------------------------------------------------------------

# Institutional delivery - past 5 years
fn_gen_rh_del_inst <- function(x){
  
  # I adjusted code for this. Didn't see exact code at the following link.
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_DEL.R
  x <- x %>%
    mutate(period = 24) %>%
    mutate(age = b19) %>% # age of child
    mutate(rh_del_pltype =
             case_when(
               m15 >=20 & m15<30   ~ 1 ,
               m15 >=30 & m15<40   ~ 2 ,
               m15 >=10 & m15<20   ~ 3,
               m15 >=40 & m15<99   ~ 4 ,
               m15 == 99 ~ 9 ,
               age>=period ~ 99)) %>%
    replace_with_na(replace = list(rh_del_pltype = c(99))) %>%
    mutate(rh_del_inst =
             case_when(
               rh_del_pltype %in% c(1,2) ~ 1,
               rh_del_pltype %in% c(3,4) ~ 0
             ))
  return(x)
}

# //Skilled provider during delivery
fn_gen_rh_del_pvskill <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_DEL.R
  x <- x %>%
    mutate(period = 24) %>%
    mutate(age = b19) %>% # age of child
    mutate(rh_del_pv =
             case_when(
               m3a == 1   ~ 1 ,
               m3b == 1 ~ 2,
               m3c == 1 | m3d == 1 | m3e == 1 | m3f == 1~ 3 ,
               m3g == 1 ~ 4 ,
               m3h == 1 | m3i == 1 | m3j == 1 | m3k == 1 | m3l == 1 | m3m == 1 ~ 5 ,
               m3n ==1 ~ 6,
               m3a ==8 | m3a==9 ~ 9 ,
               age>=period ~ 99)) %>%
    replace_with_na(replace = list(rh_del_pv = c(99))) %>%
    mutate(rh_del_pvskill =
             case_when(
               rh_del_pv %in% c(1,2)   ~ 1 , # skilled provider
               rh_del_pv %in% c(3,4,5) ~ 2,  # unskilled provider
               rh_del_pv ==6 ~ 3 , # no one
               rh_del_pv==9 ~ 9 , # don't know
               age>=period ~ 99)) %>%
    replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
    mutate(rh_del_pvskill =
             case_when(
               rh_del_pvskill==1 ~ 1,
               rh_del_pvskill %in% c(2,3,9) ~ 0))
  return(x)
}


# IR variables ------------------------------------------------------------

# //Number of days women took iron supplements during last pregnancy (90+)
fn_gen_nt_wm_micro_iron <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap11_NT/NT_WM_NUT.R
  x$nt_wm_micro_iron <- NA
  x$nt_wm_micro_iron[x$v208 == 0] <- NA
  x$nt_wm_micro_iron[x$m45_1 == 0] <- 0
  x$nt_wm_micro_iron[x$m46_1 < 60] <- 1
  x$nt_wm_micro_iron[x$m46_1 >= 60 & x$m46_1 < 90] <- 2
  x$nt_wm_micro_iron[x$m46_1 >= 90 & x$m46_1 <= 300] <- 3
  x$nt_wm_micro_iron[x$m46_1 >= 998 | x$m45_1 >= 8] <- NA
  x$nt_wm_micro_iron_any[x$b19_01 > 24] <- NA # only for pregnancies in past two years
  
  x$nt_wm_micro_iron[!is.na(x$nt_wm_micro_iron) & x$nt_wm_micro_iron < 3] <-  0  # <90 days
  x$nt_wm_micro_iron[!is.na(x$nt_wm_micro_iron) & x$nt_wm_micro_iron >= 3] <- 1 # 90+ days
   
  #set_value_labels(nt_wm_micro_iron = c("None"=0, "<60"=1, "60-89"=2, "90+"=3, "Don't know/missing"=4))
  
  return(x)
}

# //Women took any iron supplements during last pregnancy (previous 2 years)
fn_gen_nt_wm_micro_iron_any <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap11_NT/NT_WM_NUT.R
  x$nt_wm_micro_iron_any <- NA
  x$nt_wm_micro_iron_any[x$v208 == 0] <- NA
  x$nt_wm_micro_iron_any[x$m45_1 == 0] <- 0
  x$nt_wm_micro_iron_any[x$m46_1 < 60] <- 1
  x$nt_wm_micro_iron_any[x$m46_1 >= 60 & x$m46_1 < 90] <- 2
  x$nt_wm_micro_iron_any[x$m46_1 >= 90 & x$m46_1 <= 300] <- 3
  x$nt_wm_micro_iron_any[x$m46_1 >= 998 | x$m45_1 >= 8] <- NA
  x$nt_wm_micro_iron_any[x$b19_01 > 24] <- NA # only for pregnancies in past two years
  
  x$nt_wm_micro_iron_any[!is.na(x$nt_wm_micro_iron_any) & x$nt_wm_micro_iron_any == 0] <-  0  # none
  x$nt_wm_micro_iron_any[!is.na(x$nt_wm_micro_iron_any) & x$nt_wm_micro_iron_any >= 1] <- 1 # any
  
  #set_value_labels(nt_wm_micro_iron = c("None"=0, "<60"=1, "60-89"=2, "90+"=3, "Don't know/missing"=4))
  
  return(x)
}

# //ANC 4+ visits
fn_gen_rh_anc_4vs <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_ANC.R
  x <- x %>%
    mutate(period = 24) %>%
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

# //ANC 1+ visits
fn_gen_rh_anc_1vs <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_ANC.R
  x <- x %>%
    mutate(period = 24) %>%
    mutate(age = b19_01) %>% # age of child
    mutate(rh_anc_numvs =
             case_when(
               m14_1 == 0 ~ 0 ,
               m14_1 == 1 ~ 1 ,
               m14_1  %in% c(2,3)   ~ 2 ,
               m14_1>=4 & m14_1<=90  ~ 3 ,
               m14_1>90  ~ 9 ,
               age>=period ~ 99 )) %>%
    replace_with_na(replace = list(rh_anc_numvs = c(99))) %>%
    mutate(rh_anc_1vs =
             case_when(
               rh_anc_numvs%in% c(1,2,3) ~ 1,
               rh_anc_numvs %in% c(0,9) ~ 0 ))
  
  return(x)
}

# //Number of months pregnant at time of first ANC visit
# //First ANC visit during first trimester
fn_gen_rh_anc_1tri	<- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_ANC.R
  x <-  x %>%
    mutate(period = 24) %>%
    mutate(age = b19_01) %>% # age of child
    mutate(rh_anc_moprg =
                  case_when(
                    m14_1 == 0 ~ 0 ,
                    m13_1  %in% c(0,1,2,3)   ~ 1 ,
                    m13_1  %in% c(4,5)  ~ 2 ,
                    m13_1  %in% c(6,7)~ 3,
                    m13_1>=8 & m13_1<=90 ~ 4, 
                    m13_1>90 & m13_1<100 ~ 9,
                    age >= period ~ 99 )) %>%
    replace_with_na(replace = list(rh_anc_moprg = c(99))) %>%
    mutate(rh_anc_1tri = 
             case_when(
               rh_anc_moprg == 1 ~ 1,
               rh_anc_moprg %in% c(0,2,3,4,9) ~ 0)
             )
  return(x)
}

#fn_gen_fp_cusm_w_mod <- function(x){}
#fn_gen_fp_cusy_w_mod <- function(x){}

# // Blood pressure was taken during ANC visit
fn_gen_rh_anc_bldpres <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_ANC.R
  x <- x %>%
    mutate(period = 24) %>%
    mutate(age = b19_01) %>%
    mutate(ancany =
             case_when(
               m14_1 %in% c(0,99)   ~ 0 ,
               m14_1>=1 & m14_1<=60 | m14_1==98 ~ 1,
               age >= period ~ 99)) %>%
    replace_with_na(replace = list(ancany = c(99))) %>%
    mutate(rh_anc_bldpres =
             case_when(
               m42c_1 == 1 & ancany==1 ~ 1,
               ancany==1 ~ 0 ))
  return(x)
}

# // Urine sample was taken during ANC visit
fn_gen_rh_anc_urine <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_ANC.R
  x <- x %>%
    mutate(period = 24) %>%
    mutate(age = b19_01) %>%
    mutate(ancany =
             case_when(
               m14_1 %in% c(0,99)   ~ 0 ,
               m14_1>=1 & m14_1<=60 | m14_1==98 ~ 1,
               age >= period ~ 99)) %>%
    replace_with_na(replace = list(ancany = c(99))) %>%
    mutate(rh_anc_urine =
             case_when(
               m42d_1 == 1 & ancany==1 ~ 1  ,
               ancany==1 ~ 0 ))
  return(x)
}

# // Blood sample was taken during ANC visit
fn_gen_rh_anc_bldsamp <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_ANC.R
  x <- x %>%
    mutate(period = 24) %>%
    mutate(age = b19_01) %>%
    mutate(ancany =
             case_when(
               m14_1 %in% c(0,99)   ~ 0 ,
               m14_1>=1 & m14_1<=60 | m14_1==98 ~ 1,
               age >= period ~ 99)) %>%
    replace_with_na(replace = list(ancany = c(99))) %>%
    mutate(rh_anc_bldsamp =
             case_when(
               m42e_1 == 1 & ancany==1 ~ 1  ,
               ancany==1 ~ 0 ))
  return(x)
}

# //tetanus toxoid injections
# Percentage of women (aged 15-49 years) with a birth in the last 2 years preceding the survey who received at least 2 tetanus shots during their pregnancy
fn_gen_rh_anc_toxinj <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_ANC.R
  x <- x %>%
    mutate(period = 24) %>%
    mutate(age = b19_01) %>%
    mutate(rh_anc_toxinj =
             case_when(
               m1_1 >1 & m1_1 <8 ~ 1 ,
               v208 ==0 | age>=period ~ 99,
               TRUE ~ 0)) %>%
    replace_with_na(replace = list(rh_anc_toxinj = c(99)))

}

# //neonatal tetanus
# Percentage of mothers with a last live birth in the two (or three/five) years preceding the survey who received sufficient tetanus toxoid injections to provide protection at birth. Includes mothers with two injections during the pregnancy of her last birth, or two or more injections (the last within 3 years of the last live birth), or three or more injections (the last within 5 years of the last birth), or four or more injections (the last within 10 years of the last live birth), or five or more injections at any time prior to the last birth 
fn_gen_rh_anc_neotet <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_ANC.R
  x <- x %>%
    mutate(period = 24) %>%
    mutate(age = b19_01) %>%
    mutate(ageyr=as.integer(age/12)) %>%
    mutate(tet2lastp=
             case_when(
               m1_1 > 1 & m1_1 < 8 ~ 1,
               TRUE ~ 0))
  
    x[["totet0"]] <- 0
    x[["totet0"]] <- ifelse(!is.na(x[["m1_1"]]) & x[["m1_1"]]>0 & x[["m1_1"]]<8, x[["m1_1"]], 0)
    x[["totet"]] <- x[["totet0"]]
    x[["totet"]] <- ifelse(!is.na(x[["m1a_1"]]) & x[["m1a_1"]]>0 & x[["m1a_1"]]<8, x[["m1a_1"]] + x[["totet0"]], x[["totet0"]])
    
    x <- x %>%    
      mutate(lastinj=
               case_when(
                 m1_1>0 & m1_1 <8 ~ 0,
                 m1d_1 <20 & (m1_1==0 | (m1_1>7 & m1_1<9996)) ~ (m1d_1 - ageyr),
                 TRUE ~9999)) %>%
      mutate(ttprotect = 
               case_when(
                 tet2lastp ==1 ~ 1,
                 totet>=2 & lastinj<=2 ~ 1,
                 totet>=3 &  lastinj<=4 ~ 1,
                 totet>=4 &  lastinj<=9 ~ 1,
                 totet>=5 ~ 1,
                 TRUE ~ 0))
    
    x[["rh_anc_neotet"]] <- x[["ttprotect"]]
    x[["rh_anc_neotet"]] <- ifelse(x[["bidx_01"]]!=1 | x[["age"]]>=x[["period"]], NA, x[["ttprotect"]])
  
    return(x)
}



# //Weighed during pregnancy
# // Weighed during pregnancy
# Care given during the last antenatal visit for the pregnancy
# BASE: For M42A to M42E is women who had seen someone for antenatal care for their last born child (MIDX = 1 & M2N <> 1).
# M2A-N: The type of person who gave prenatal care to the respondent prior to the last birth. A value of M2N would mean no one.
# m42a_1, m42_2, m42a_3, m42a_4, m42a_5, m42a_6
fn_gen_rh_anc_wgt <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_ANC.R
  x <- x %>%
    mutate(period = 24) %>%
    mutate(age = b19_01) %>%
    mutate(ancany =
             case_when(
               m14_1 %in% c(0,99)   ~ 0 ,
               m14_1>=1 & m14_1<=60 | m14_1==98 ~ 1,
               age >= period ~ 99)) %>%
    replace_with_na(replace = list(ancany = c(99))) %>%
    mutate(rh_anc_wgt =
             case_when(
               m42a_1 == 1 & ancany==1 ~ 1  ,
               ancany==1 ~ 0 ))
  return(x)
}

# //Took iron tablet/syrup during the pregnancy of last birth
# fn_gen_rh_anc_iron <- function(x){
# 
#   # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_ANC.R
#   x <- x %>%
#     mutate(period = 60,
#            age = b19_01) %>%
#     mutate(rh_anc_iron =
#              case_when(
#                m45_1 == 1 ~ 1 ,
#                v208 == 0 | age>=period ~ 99, # if no births reported or birth was more than 5 years ago
#                TRUE ~ 0)) %>%
#     replace_with_na(replace = list(rh_anc_iron = c(99)))
# 
#   return(x)
# }
fn_gen_rh_anc_iron <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap09_RH/RH_ANC.R
  x <- x %>%
    mutate(period = 24) %>%
    mutate(age = b19_01) %>%
    mutate(ancany =
             case_when(
               m14_1 %in% c(0,99)   ~ 0 ,
               m14_1>=1 & m14_1<=60 | m14_1==98 ~ 1,
               age >= period ~ 99)) %>%
    replace_with_na(replace = list(ancany = c(99))) %>%
    mutate(rh_anc_iron =
             case_when(
               m45_1 == 1 & ancany==1 ~ 1  ,
               ancany==1 ~ 0 ))
  
  return(x)
}

# //PNC check within two days for mother
# This one is: Among most recent live births in the 2 years preceding the survey, percentage for which the mother age 15-49 received a postnatal check during the first 2 days after birth, RH_PCMN_W_MOT
# national level estimate is 55.2
# There is also: 	Percentage of women giving birth in the two years preceding the survey who had their first postnatal checkup 1-2 days after birth, RH_PCMT_W_D12
# national level estimate is 8.6
# THIS IS CURRENTLY THE ONLY INDICATOR THAT DOESN"T HAVE A GOOD MATCH WITH STATCOMPILER
# TRY TO FIX
fn_gen_rh_pnc_wm_2days <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/fe6659a1d3af7e358d158bc814e38c96bf6f5161/Chap09_RH/RH_PNC.R
  x <- x %>%
    mutate(age = b19_01) %>% # age of child
    mutate(momcheck =
             case_when(
               # m62_1 respondent's health checked after delivery before discharge
               # m66_1 respondent's health checked after discharge/delivery at home
               (m62_1!=1 & m66_1!=1) & age<24 ~ 0, 
               (m62_1==1 | m66_1==1) & age<24 ~ 1,
               age<24 ~ 0)) %>% #select(m62_1, m66_1, age, momcheck) %>% View
    mutate(pnc_wm_time =
             case_when(
               # m64_1 who checked respondent's health before discharge
               # m63_1 how long after delivery respondent's health check before discharge
               (m64_1 >= 11 & m64_1 <= 29) & age<24 ~ as.numeric(m63_1), # tried changing m64_1 >= 11 to 10, no effect
               age<24 & momcheck == 1 ~ 999)) %>%
    mutate(pnc_wm_time1000 = 
             case_when(
               pnc_wm_time<1000 ~ 1)) %>%
    mutate(pnc_wm_time =
             case_when(
               pnc_wm_time1000==1 & (m64_1 > 30 & m64_1 < 100) & age<24 ~ 0,
               TRUE ~ pnc_wm_time )) %>%
    mutate(pnc_wm_time999 = 
             case_when(
               pnc_wm_time==999 ~ 1)) %>%
    mutate(pnc_wm_time =
             case_when(
               pnc_wm_time999==1 & (m68_1 >= 11 & m68_1 <= 29) & age<24 ~ m67_1, # tried changing m64_1 >= 11 to 10, no effect
               TRUE ~ pnc_wm_time ))  %>%
    mutate(pnc_wm_time0 =
             case_when(
               m67_1 < 1000 & m68_1 > 30 & m68_1 < 100 & age<24 ~ 1,
               TRUE ~ 0)) %>%
    mutate(pnc_wm_time =
             case_when(
               pnc_wm_time0==1 ~ 0,
               pnc_wm_time0==0 ~ as.numeric(pnc_wm_time))) %>%
    mutate(pnc_wm_time00 =
             case_when(
               momcheck==0 & age<24 ~ 1)) %>%
    mutate(pnc_wm_time =
             case_when(
               pnc_wm_time00==1 ~ 0,
               TRUE ~ as.numeric(pnc_wm_time))) %>%
    mutate(rh_pnc_wm_timing =
             case_when(
               (pnc_wm_time >=242 & pnc_wm_time<=299) | (pnc_wm_time>=306 & pnc_wm_time<900) | pnc_wm_time==0  ~ 0 ,
               pnc_wm_time  %in% c(100, 101, 102, 103)  ~ 1 ,
               (pnc_wm_time >=104 & pnc_wm_time<=123) | pnc_wm_time==200 ~ 2 ,
               (pnc_wm_time >=124 & pnc_wm_time<=171) | pnc_wm_time %in% c(201,202)~ 3,
               (pnc_wm_time >=172 & pnc_wm_time<=197) | pnc_wm_time %in% c(203,204,205,206) ~ 4, 
               (pnc_wm_time >=207 & pnc_wm_time<=241) | (pnc_wm_time >=301 & pnc_wm_time<=305)  ~ 5, 
               pnc_wm_time  %in% c(198, 199, 298, 299, 298, 399, 998, 999)  ~ 9 ,
               bidx_01!=1 | age>=24 ~ 99 )) %>%
    replace_with_na(replace = list(rh_pnc_wm_timing = c(99))) %>%
    mutate(rh_pnc_wm_2days =
             case_when(
               rh_pnc_wm_timing %in% c(1,2,3) ~ 1,
               rh_pnc_wm_timing %in% c(0,4,5,9) ~ 0,
               bidx_01!=1 | age>=24 ~ 99 )) %>%
    replace_with_na(replace = list(rh_pnc_wm_2days = c(99))) 
    # select(age, momcheck, m62_1, m63_1, m64_1, m66_1, m68_1, 
    #        pnc_wm_time, pnc_wm_time1000, pnc_wm_time999, pnc_wm_time0, pnc_wm_time00,    
    #        rh_pnc_wm_timing, rh_pnc_wm_2days) %>% View()
  
  return(x)
}

# //PNC check within two days for newborn
# This one is: 	Among most recent live births in the 2 years preceding the survey, percentage for which the newborn received a postnatal check during the first 2 days after birth, RH_PCMN_W_NBR
# national level estimate is 56.2
# There is also: Percentage of last births in the two years preceding the survey who had their first postnatal checkup 1-2 days after birth, 	RH_PCCT_C_D12
# national elvel estimate is 7.3
fn_gen_rh_pnc_nb_2days <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/fe6659a1d3af7e358d158bc814e38c96bf6f5161/Chap09_RH/RH_PNC.R
  x <- x %>%
    mutate(age = b19_01) %>% # age of child
    mutate(rh_pnc_nb_timing =
             case_when(
               m70_1!=1 & m74_1!=1 ~ 0, #newborns with no check in first 2 months
               (m76_1>=10 & m76_1<=29) & ((m75_1 >=207 & m75_1<=297) | (m75_1>=301 & m75_1<397)) ~ 0,
               (m76_1>=10 & m76_1<=29) & m75_1 ==100 ~ 1 ,
               (m76_1>=10 & m76_1<=29) & m75_1 %in% c(101,102,103) ~ 2 ,
               (m76_1>=10 & m76_1<=29) & ((m75_1 >=104 & m75_1<=123) | m75_1==200) ~ 3,
               (m76_1>=10 & m76_1<=29) & ((m75_1 >=124 & m75_1<=171) | m75_1 %in% c(201,202)) ~ 4, 
               (m76_1>=10 & m76_1<=29) & ((m75_1 >=172 & m75_1<=197) | (m75_1 >=203 & m75_1<=206))  ~ 5, 
               (m76_1>=10 & m76_1<=29) & m75_1 %in% c(198, 199, 298, 299, 398, 399, 998, 999)  ~ 9,
               (m72_1>=10 & m72_1<=29) & ((m71_1 >=207 & m71_1<=297) | (m71_1>=301 & m71_1<397)) ~ 0,
               (m72_1>=10 & m72_1<=29) & m71_1 ==100 ~ 1 ,
               (m72_1>=10 & m72_1<=29) & m71_1 %in% c(101,102,103) ~ 2 ,
               (m72_1>=10 & m72_1<=29) & ((m71_1 >=104 & m71_1<=123) | m71_1==200) ~ 3,
               (m72_1>=10 & m72_1<=29) & ((m71_1 >=124 & m71_1<=171) | m71_1 %in% c(201,202)) ~ 4, 
               (m72_1>=10 & m72_1<=29) & ((m71_1 >=172 & m71_1<=197) | (m71_1 >=203 & m71_1<=206))  ~ 5, 
               (m72_1>=10 & m72_1<=29) & m71_1  %in% c(198, 199, 298, 299, 398, 399, 998, 999)  ~ 9,
               m80_1==1 ~ 0))	  
  x[["rh_pnc_nb_timing"]] <- ifelse(x[["bidx_01"]]!=1 | x[["age"]]>=24, NA, x[["rh_pnc_nb_timing"]])
  # PNC within 2days for newborn	
  x <- x %>%
    mutate(rh_pnc_nb_2days =
             case_when(
               rh_pnc_nb_timing %in% c(1,2,3,4) ~ 1,
               rh_pnc_nb_timing %in% c(0,5,9) ~ 0,
               bidx_01!=1 | age>=24 ~ 99 )) %>%
    replace_with_na(replace = list(rh_pnc_nb_2days = c(99)))
  
  return(x)
}


# //Breastfeeding counseling during first two days after birth
# Among most recent live births in the 2 years preceding the survey, the percentage for whom counseling on breastfeeding was provided during the first 2 days after birth
# I found a variable for during first 2 days health provider: counsel on breastfeeding (m78d_1)
fn_gen_rh_pnc_wm_bfcounsel <- function(x){
  
  # during first 2 days health provider: counsel on breastfeeding
  x <- x %>%
    mutate(age = b19_01) %>% # age of child
    mutate(rh_pnc_wm_bfcounsel =
             case_when(
               m78d_1 == 1 & age < 24 ~ 1,
               m78d_1 == 0 & age < 24 ~ 0,
               #is.na(m78d_1) & age < 24 ~ 0,
               TRUE ~ NA
             ))
  
  return(x)
}


# Women with a birth in the past two years who received a vitamin A dose in the first two months after delivery
# BD2014
# Couldn't find code on DHS github, came up with code on my own
fn_gen_nt_wm_ppvita <- function(x){
  
  x <- x %>%
    mutate(age = v008-b3_01) %>% # age of child
    mutate(nt_wm_ppvita =
             case_when(
               m54_1 == 1 & age < 24 ~ 1,
               m54_1 == 0 & age < 24 ~ 0,
               TRUE ~ NA
             ))
  
  return(x)
}


# IR variables - missing --------------------------------------------------

# //Breastfeeding counseling during pregnancy
# m42h_1: na - during pregnancy: talk about breastfeeding

# //Vitamin A supplementation in postpartum women
# fn_gen_an_miam_w_vap <- function(x){}
# m54_1: na - received vitamin a dose in first 2 months after delivery

# //Woman living in household with iodized salt
# Percentage of women with a birth in the past five years living in households using adequately iodized salt
# hv234a: na - result of salt test for iodine
# variable name doesn't not appear in IR file. it appears in HR file but is all NA.
fn_gen_nt_wm_micro_iod <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/fe6659a1d3af7e358d158bc814e38c96bf6f5161/Chap11_NT/NT_WM_NUT.R
  x <- x %>%
    mutate(nt_wm_micro_iod =
             case_when(
               v208==0 | hv234a>1  ~ 99,
               hv234a==0   ~ 0, 
               hv234a==1  ~ 1)) %>%
    replace_with_na(replace = list(nt_wm_micro_iod = c(99)))
  
  return(x)
}



# HR variables ------------------------------------------------------------



# //improved water source
fn_gen_ph_wtr_improve <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap16_WS/DHS8/WS_WATER.R
  x <- x %>% 
    mutate(ph_wtr_source = case_when( 
      hv201==14 ~ 13,
      TRUE ~ hv201
    )) %>% 
    mutate(ph_wtr_improve = case_when(
            ph_wtr_source %in% c(11, 12, 13, 14, 15, 21, 31, 41, 51, 61, 62, 65, 71, 72, 73) ~ 1,
            ph_wtr_source %in% c(30, 32, 40, 42, 43, 96) ~ 0,
            ph_wtr_source==99 ~ 99)) %>%
    replace_with_na(replace = list(ph_wtr_improve = c(99)))
  
  return(x)
  
}

# //Appropriately treated water before drinking
# Percentage of households using an appropriate treatment method, including boiling, bleaching, filtering or solar disinfecting.
fn_gen_ph_wtr_trt_appr <- function(x){
  
  x <- x %>% 
    mutate(ph_wtr_trt_appr = case_when(
            hv237a==1 ~ 1,
            hv237b==1 ~ 1,
            hv237d==1 ~ 1,
            hv237e==1 ~ 1,
            TRUE ~ 0)) 

  return(x)
  
}

# //Access to improved sanitation
# Percentage of households with an improved sanitation facility
fn_gen_ph_sani_improve <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap16_WS/DHS8/WS_SANI.R
  x<- x %>% 
    mutate(ph_sani_type = hv205) %>%
    mutate(ph_sani_type = case_when(
      is.na(ph_sani_type) ~ 99,
      TRUE ~ ph_sani_type)) %>%
    mutate(ph_sani_improve = case_when(
            ph_sani_type %in% c(11, 12, 13, 15, 21, 22, 41, 51) ~ 1, # improved sanitation
            ph_sani_type %in% c(14, 23, 42, 43, 96) ~ 0, # unimproved sanitation
            ph_sani_type ==31 ~ 0, # open defecation
            ph_sani_type ==99 ~ NA)) 
  
  return(x)
}

# //Basic handwashing facility
# had to remove "hv102" condition because using HR and not PR dataset
fn_gen_ph_hndwsh_basic <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap16_WS/DHS8/WS_HNDWSH.R
  x <-  x %>%
    mutate(ph_hndwsh_basic = 
             case_when(
               hv230b==1 & hv232==1 ~ 1,
               hv230a<=3 ~ 0))
    # mutate(ph_hndwsh_basic = 
    #          case_when(
    #            hv230b==1 & hv232==1 & hv102==1 ~ 1,
    #            hv230a<=3 & hv102==1 ~ 0))
  
  return(x)
  
}


# HR variables - missing ------------------------------------------------------------

# Two indicators which I'm not sure exactly how they match DHS data:
## Households with iodized salt
## Salt iodization test above 15ppm
# However, both are missing in the BD2022DHS so can't check.

# //Households with salt tested for iodine content
# Percentage of households with salt tested for iodine content
# denominator: all households
# CN_IODZ_H_SLT
fn_gen_nt_salt_iod <- function(x){
  
  # altered the code to include all households in denominator
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/fe6659a1d3af7e358d158bc814e38c96bf6f5161/Chap11_NT/NT_SALT.R
  x <- x %>%
    mutate(nt_salt_iod =
             case_when(
               hv234a ==1  ~ 1 ,
               TRUE ~ 0))
  
  return(x)
  
}

# //Households with iodized salt
# Percentage of households with iodized salt
# denominator: all households with tested salt
# CN_IODZ_H_IOD
fn_gen_nt_salt_15ppm <- function(x){
  
  # came up with this code
  x <- x %>%
    mutate(nt_salt_15ppm = case_when(
      hv234 %in% c(15, 30) ~ 1, # 15 ppm and above, 30 ppm
      hv234 %in% c(0, 7) ~ 0, # 0 ppm (no iodine), below 15 ppm
      TRUE ~ NA
    ))
  
  return(x)
  
}

# //basic water source
# Percentage of households with basic water service, defined as an improved water source with either water on the premises or round-trip collection time is 30 minutes or less.
fn_gen_ph_wtr_basic <- function(x){
  
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap16_WS/DHS8/WS_WATER.R
  x <- x  %>% 
    mutate(ph_wtr_source = case_when( 
      hv201==14 ~ 13,
      TRUE ~ hv201
    )) %>% 
    mutate(ph_wtr_time = case_when(
      hv204 %in% c(0, 996) ~ 0,
      between(hv204, 1, 30) ~ 1,
      between(hv204, 31,900) ~ 2,
      hv204>=998 ~ 3)) %>%
    mutate(ph_wtr_improve = case_when(
      ph_wtr_source %in% c(11, 12, 13, 14, 15, 21, 31, 41, 51, 61, 62, 65, 71, 72, 73) ~ 1,
      ph_wtr_source %in% c(30, 32, 40, 42, 43, 96) ~ 0,
      ph_wtr_source==99 ~ 99)) %>%
    mutate(ph_wtr_basic = case_when(
      ph_wtr_improve==1 & ph_wtr_time<=1 ~ 1, # basic
      ph_wtr_improve==1 & ph_wtr_time>1 ~ 0,  # limited
      ph_wtr_improve==0 ~ 0))  # unimproved
  
  return(x)
  
}
