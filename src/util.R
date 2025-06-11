

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
  
  x$age_months <- x$v008 - x$b3
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


# Mothers who received IYCF counseling in the last 6 months - NEW Indicator in DHS8
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

# Child with minimum dietary diversity- last-born 6-23 months
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
# //First ANC visit during first trimester
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

check_overlap<- function(xmin, xmax, ymin, ymax) {
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

