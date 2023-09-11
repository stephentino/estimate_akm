# -------------------------------------------------------------------------------------------------------------------- #
# Copyright 2023 Stephen Tino                                                                                          #
#                                                                                                                      #
# This file is part of estimate_akm                                                                                    #
#                                                                                                                      #
# estimate_akm is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General    #
# Public License as published by the Free Software Foundation, either version 2.1 of the License, or (at your option)  #
# any later version.                                                                                                   #
#                                                                                                                      #
# estimate_akm is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied   #
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more  #
# details.                                                                                                             #
#                                                                                                                      #
# You should have received a copy of the GNU Lesser General Public License along with estimate_akm. If not, see        #
# <https://www.gnu.org/licenses/>.                                                                                     #
# -------------------------------------------------------------------------------------------------------------------- #


# main akm code -----------------------------------------------------------

estimate_akm_and_var_decomp <- function(main_data, model_name, current_date) {
  
  # estimate model and extract results --------------------------------------
  
  fe <- estimate_akm(akm_data = main_data,
                     prefix = model_name,
                     date = current_date)
  
  # variance decomposition --------------------------------------------------
  
  main_data <- get_decomp_data(main_data, fe) # this function merges the FEs into the data with earnings
  
  rm(fe)
  
  results <- var_decomp(y = main_data$log_wage, 
                        worker_fe = main_data$worker_fe,
                        firm_fe = main_data$firm_fe,
                        prefix = model_name,
                        date = current_date)
  
  # export data -------------------------------------------------------------
  
  cat("\n\nexporting akm data...\n\n")
  saveRDS(main_data, file = paste0("//tor-main/Projects2/Tino_10328/Data/akm_data/",model_name,"_akm_data_",current_date,".rds"))
  
}








# data cleaning just before running akm -----------------------------------

get_akm_data <- function(data_input,
                         compute_residuals = TRUE,
                         dual_connected = FALSE,
                         group_var = "male",
                         group_value = 1) {
  
  # this function extracts the subset of the data used to estimate the akm model
  # the data_input is a matched employer-employee dataset with the following columns:
  # main_data <- main_data[, .(casenum2021,
  #                           year,
  #                           entid_syn,
  #                           real_t4earn,
  #                           prov_of_res,
  #                           married,
  #                           age)]
  # note: if compute_residuals == TRUE, then the function regresses log_wage on time-varying covariates and recovers the residuals
  # note: if dual_connected == TRUE, then a group_var and group_value must also be supplied
  #       the group_var must be a dummy variable that splits the data into two groups (e.g. male or female)
  #       the group_value is equal to 1 or 0 (e.g. if the group_var is male, want group_value==1 for males or ==0 for females)
  #       if dual_connected == TRUE, the connected set will be the "dual connected set" common to both groups
  # 
  
  
  # need to copy the input to avoid the assignment by reference ':=' modifying the input in the global environment
  data <- copy(data_input)
  rm(data_input) 
  
  cat("\n\ndata before cleaning:\n\n")
  glimpse(data)
  
  # restrict to workers/firms observed at least twice to identify FEs -----------------------
  
  data[, n_worker := .N, by = casenum2021 ]
  data[, n_firm := .N, by = entid_syn ]
  
  data <- data[n_worker >= 2][, n_worker := NULL]
  data <- data[n_firm >= 2][, n_firm := NULL]
  
  
  #  extract list of movers to compute the connected set ---------------------------------------
  
  setkey(data,casenum2021,year)
  
  data[, lag_entid_syn := shift(entid_syn, 1, type = "lag")]
  data[, lag_casenum2021 := shift(casenum2021, 1, type = "lag")]
  
  data[, move := as.integer(0)]
  data[entid_syn != lag_entid_syn & casenum2021 == lag_casenum2021, move := as.integer(1)]
  data[year == min(year), move := NA]
  
  
  # extract connected set ---------------------------------------------------
  
  if (dual_connected == TRUE) {
    
    cat("\n\nextracting the dual connected set\n\n")
    
    # first, extract connected set for group_var == 1
    
    jdata <- data[move==1 & get(group_var)==1, .(j1=lag_entid_syn, j2=entid_syn)]
    
    output <- get_connected_set(jdata)
    
    data <- merge(x = data, 
                  y = output, 
                  all.x = TRUE, # keep all firms 
                  all.y = FALSE, 
                  by ="entid_syn")
    
    cat(paste0("\n\ncomputing connected set for",group_var,"==1:"))
    cat("\nfraction of the sample missing the connected dummy variable:\n")
    print(data[is.na(connected), .N]/data[, .N])
    
    data[is.na(connected), connected := 0]
    
    cat("\nfraction of observations in the connected set:")
    print(mean(data$connected, na.rm = TRUE))
    
    data[, connected1 := connected]
    data[, connected := NULL]
    
    rm(output)
    
    # second, extract connected set for group_var == 0
    
    jdata <- data[move==1 & get(group_var)==0, .(j1=lag_entid_syn, j2=entid_syn)]
    
    output <- get_connected_set(jdata)
    
    data <- merge(x = data, 
                  y = output, 
                  all.x = TRUE, # keep all firms 
                  all.y = FALSE, 
                  by ="entid_syn")
    
    cat(paste0("\n\ncomputing connected set for",group_var,"==0:"))
    cat("\nfraction of the sample missing the connected dummy variable:\n")
    print(data[is.na(connected), .N]/data[, .N])
    
    data[is.na(connected), connected := 0]
    
    cat("\nfraction of observations in the connected set:\n")
    print(mean(data$connected, na.rm = TRUE))
    
    data[, connected0 := connected]
    data[, connected := NULL]
    
    rm(output)
    
    # finally, compute the dual connected set as the intersection of the two
    data[, connected := as.integer(connected1==1 & connected0==1)]
    
    cat("\n\nfraction of observations in the DUAL connected set:\n")
    print(mean(data$connected, na.rm = TRUE))
    
    
  } else {
    
    cat("\n\nextracting the connected set (this is *NOT* a dual connected set)\n\n")
    
    # note: the lfe package automatically extracts the connected set,
    # but I do this before running felm from the lfe package to reduce the computational burden when estimating the model)
    
    jdata <- data[move==1, .(j1=lag_entid_syn, j2=entid_syn)]
    
    output <- get_connected_set(jdata)
    
    data <- merge(x = data, 
                  y = output, 
                  all.x = TRUE, # keep all firms 
                  all.y = FALSE, 
                  by ="entid_syn")
    
    cat("\nfraction of the sample missing the connected dummy variable:\n")
    print(data[is.na(connected), .N]/data[, .N])
    
    data[is.na(connected), connected := 0]
    
    cat("\n\nfraction of observations in the connected set:\n\n")
    print(mean(data$connected, na.rm = TRUE))
    
    rm(output)
    
  }
  
  # prepare data ----------------------------------------------------------
  
  if(dual_connected == TRUE) {
    
    cat("\n\nRestricting sample to: ")
    cat(paste0(group_var,"==",group_value))
    cat("\nfraction of sample:\n")
    print(data[get(group_var) == group_value, .N]/data[, .N])
    
    data <- data[get(group_var) == group_value]
    
  }
  
  data[, casenum2021 := as.factor(casenum2021)]
  
  data[, entid_syn := as.factor(entid_syn)]
  
  data[, year := as.factor(year)]
  
  data[, prov_of_res := as.factor(prov_of_res)]
  
  data <- data[, .(casenum2021, entid_syn, log_wage = log(real_t4earn), year, prov_of_res, age, married, connected)]
  
  data <- data[connected==1]
  
  # normalize age  ----------------------------------------------------------
  
  cat("\n\nnormalizing age....\n\n")
  # I normalize age before running akm, following the literature
  # (see for example Lie et al. 2023, Dostie et al. 2021, LMS footnote 16, and the card paper referenced by LMS)
  
  # the normalization procedure is the following:
  # first, residualize the wage using time-varying covariates (excluding age)
  # next, determine which age has the largest residualized log earnings; call this a*
  # finally, normalize age using (age-a*)/a*
  
  est <- felm(log_wage ~ married | year + prov_of_res, data = data)
  
  cat("\nfinished estimating linear model for age normalization.\n")
  
  data$resid <- est$residuals
  
  rm(est)
  
  data[, avg_resid := mean(resid, na.rm = TRUE), by = age]
  
  age_data <- unique(data[, .(age, avg_resid)])
  
  age_data[, max_resid := max(avg_resid, na.rm = TRUE)]
  
  cat("\nage used in the normalization:\n")
  normalization_factor <- age_data[which(age_data$max_resid==age_data$avg_resid), age]
  print(normalization_factor)
  
  rm(age_data)
  data[, resid := NULL]
  data[, avg_resid := NULL]
  
  data[, norm_age := (age-normalization_factor)/normalization_factor] 
  
  data[, norm_age_sq := (norm_age^2)/1E2]
  
  data[, norm_age_cubed := (norm_age^3)/1E3]
  
  data[, norm_age_quart := (norm_age^4)/1E4]
  
  
  if(compute_residuals == TRUE) {
    
    # residualize log wages for two-way FE estimation -------------------------
    
    # note: since age is perfectly colinear with birth year and the akm regression includes individual FE
    # therefore I exclude the linear term of the quartic polynomial in age in the regression below
    # this follows the literature (e.g. Dostie et al. 2021 and Li et al. 2023)
    
    cat("\nbeginning residualization of log wage...\n")
    
    est <- felm(log_wage ~ norm_age_sq + norm_age_cubed + norm_age_quart + married | year + prov_of_res, data = data)
    
    data$resid <- est$residuals
    
    rm(est)
    
    cat("\nfinished residualizing log wage.\n")
    
  }
  
  cat("\nData is ready for akm estimation.\n")
  
  
  cat("\n\ndata after cleaning:\n\n")
  glimpse(data)
  cat("\n\n")
  
  
  return(data)
  
}



# compare results from two different fixed effects models --------


compare_lfe_results <- function(est1 = NULL, fe1 = NULL, est2 = NULL, fe2 = NULL, comparison_name) {
  
  cat("\n#---------------------------------------#\n")
  cat(paste0("results from comparison: ",comparison_name))
  cat("\n#---------------------------------------#\n")
  
  
  residuals <- data.table(resid1 = est1$residuals,
                          resid2 = est2$residuals)
  
  cat("\nold column names:\n")
  print( colnames(residuals) )
  
  colnames(residuals) <- c("y1","y2")
  
  cat("\nnew column names:\n")
  print( colnames(residuals) )
  
  
  residuals[, abs_diff := abs(y1-y2)]
  residuals[, diff := y1-y2]
  
  cat("\n total differences in residuals from the different models:\n")
  cat("sum of difference: ")
  print( sum(residuals$diff) )
  cat("\nsum of absolute difference: ")
  print( sum(residuals$abs_diff) )
  
  cat("\n sum of absolute diff as a fraction of the sum of the absolute value of the residuals:\n")
  print( sum(residuals$abs_diff)/sum(abs(residuals$y1)) )
  print( sum(residuals$abs_diff)/sum(abs(residuals$y2)) )
  
  cat("\n sum of the absolute diff as a fraction of the average of the absolute value of the residuals:\n")
  print( sum(residuals$abs_diff)/mean(abs(residuals$y1)) )
  print( sum(residuals$abs_diff)/mean(abs(residuals$y2)) )
  
  if(!is.null(fe1) & !is.null(fe2)) {
    
    cat("\n variance of individual FEs:\n")
    
    print( var(fe1[fe =="casenum2021", effect]) )
    
    print( var(fe2[fe =="casenum2021", effect]) )
    
    print( var(fe1[fe =="casenum2021", effect])/var(fe2[fe =="casenum2021", effect]) )
    
    
    cat("\n variance of firm FEs:\n")
    
    print( var(fe1[fe =="entid_syn", effect]) )
    
    print( var(fe2[fe =="entid_syn", effect]) )
    
    print( var(fe1[fe =="entid_syn", effect])/var(fe2[fe =="entid_syn", effect]) )
    
    
    
    
  } else {
    
    
    
    cat("\nNo FEs supplied. Skipping variance decomposition.\n")
    
    
  }
}



# estimate akm model ------------------------------------------------------

estimate_akm <- function(akm_data, prefix = "", date = "") {
  
  cat("\nbeginning estimation of FE model....\n")
  
  est <- felm(resid ~ 1 | casenum2021 + entid_syn, data = akm_data)
  
  cat("\nestimation of FE model complete. Results:\n")
  
  print(summary(est))
  
  cat("\nExtracting FEs....\n")
  
  fe <- getfe(est)
  
  fe <- as.data.table(fe)
  
  rm(est)
  
  cat("\nExtraction of FE complete.\n")
  
  return(fe)
  
}



# prepare data for variance decomp ----------------------------------------

get_decomp_data <- function(main_data, fe) {
  
  
  # this function prepares the data for the variance decomposition by merging the FE into the mee data with earnings
  # note: both "main_data" and "fe" must be of class "data.table" for this function to work!
  
  cat("\nFE data:\n")
  glimpse(fe)
  
  main_data <- merge(x = main_data,
                     y = fe[fe=="casenum2021",
                            .(worker_fe = effect, casenum2021 = as.character(idx))],
                     all.x = TRUE,
                     all.y = TRUE,
                     by = "casenum2021")
  
  main_data <- merge(x = main_data,
                     y = fe[fe=="entid_syn",
                            .(firm_fe = effect, entid_syn = as.character(idx))],
                     all.x = TRUE,
                     all.y = TRUE,
                     by = "entid_syn")
  
  cat("\nfraction of observations missing worker or firm FEs:\n")
  print(main_data[is.na(worker_fe) & is.na(firm_fe), .N]/main_data[, .N])
  
  main_data <- main_data[!is.na(worker_fe) & !is.na(firm_fe)]
  
  main_data[, year := as.character(year)]
  main_data[, year := as.integer(year)]
  
  main_data <- main_data[, .(casenum2021,
                             entid_syn,
                             year,
                             log_wage,
                             firm_fe,
                             worker_fe)]
  
  cat("\ndata after merging with fixed effects from akm estimation:\n")
  glimpse(main_data)
  
  return(main_data)
  
}



# variance decomposition --------------------------------------------------

var_decomp <- function(y, worker_fe, firm_fe, prefix = "", date = "") {
  
  statistics <- c("var(y)",
                  "var(worker_fe)",
                  "var(firm_fe)",
                  "2*cov(worker_fe,firm_fe)",
                  "share_worker",
                  "share_firm",
                  "share_2cov")
  
  values <- c(var(y),
              var(worker_fe),
              var(firm_fe),
              2*cov(worker_fe,firm_fe),
              var(worker_fe)/var(y),
              var(firm_fe)/var(y),
              ( 2*cov(worker_fe,firm_fe) )/var(y)
  )
  
  results <- data.table(statistic = statistics, 
                        value = values)
  
  filename <- paste0("//tor-main/Projects2/Tino_10328/summary_statistics/",prefix,"_akm_var_decomp_",date,".csv")
  fwrite(results, file = filename,
         col.names = TRUE, row.names = FALSE, append = FALSE)
  
  return(results)
  
}


