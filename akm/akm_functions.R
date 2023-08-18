

# data cleaning just before running akm -----------------------------------

get_akm_data <- function(data_input) {
  
  # this function extracts the subset of the data used to estimate the akm model
  # the data_input is a matched employer-employee dataset with the following columns:
  # main_data <- main_data[, .(casenum2021,
  #                           year,
  #                           entid_syn,
  #                           real_t4earn,
  #                           prov_of_res,
  #                           married,
  #                           age)]
  
  
  
  # need to copy the input to avoid the assignment by reference ':=' modifying the input in the global environment
  data <- copy(data_input)
  rm(data_input) 
  
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
  
  # note: the lfe package automatically does this,
  # but I do this before running felm from the lfe package to reduce the computational burden when estimating the model)
  
  jdata <- data[move==1, .(j1=lag_entid_syn, j2=entid_syn)]
  
  output <- get_connected_set(jdata)
  
  data <- merge(x = data, 
                y = output, 
                all.x = TRUE, # keep all firms 
                all.y = FALSE, 
                by ="entid_syn")
  
  cat("\n\nany firms missing connected status do not employ movers. fraction of the sample:\n")
  print(data[is.na(connected), .N]/data[, .N])
  
  data[is.na(connected), connected := 0]
  
  cat("\nfraction of worker-firms in the connected set:\n")
  print(mean(data$connected, na.rm = TRUE))
  
  rm(output)
  
  # prepare data ----------------------------------------------------------
  
  data[, casenum2021 := as.factor(casenum2021)]
  
  data[, entid_syn := as.factor(entid_syn)]
  
  data[, year := as.factor(year)]
  
  data[, prov_of_res := as.factor(prov_of_res)]
  
  data <- data[, .(casenum2021, entid_syn, log_wage = log(real_t4earn), year, prov_of_res, age, married, connected)]
  
  data <- data[connected==1]
  
  # normalize age  ----------------------------------------------------------
  
  
  # I normalize age before running akm, following the literature
  # (see for example Lie et al. 2023, Dostie et al. 2021, LMS footnote 16, and the card paper referenced by LMS)
  
  # the normalization procedure is the following:
  # first, residualize the wage using time-varying covariates (excluding age)
  # next, determine which age has the largest residualized log earnings; call this a*
  # finally, normalize age using (age-a*)/a*
  
  est <- felm(log_wage ~ married | year + prov_of_res, data = data)
  
  cat("\n\n\nfinished estimating linear model for age normalization.\n")
  
  data$resid <- est$residuals
  
  rm(est)
  
  data[, avg_resid := mean(resid, na.rm = TRUE), by = age]
  
  age_data <- unique(data[, .(age, avg_resid)])
  
  age_data[, max_resid := max(avg_resid, na.rm = TRUE)]
  
  cat("\nage of maximum residuals:\n")
  normalization_factor <- age_data[which(age_data$max_resid==age_data$avg_resid), age]
  print(normalization_factor)
  
  rm(age_data)
  data[, resid := NULL]
  data[, avg_resid := NULL]
  
  data[, norm_age := (age-normalization_factor)/normalization_factor] 
  
  data[, norm_age_sq := (norm_age^2)/1000]
  
  data[, norm_age_cubed := (norm_age^3)/1000]
  
  data[, norm_age_quart := (norm_age^4)/1000]
  
  
  # residualize log wages for two-way FE estimation -------------------------
  
  # note: since age is perfectly colinear with birth year and the akm regression includes individual FE
  # therefore I exclude the linear term of the quartic polynomial in age in the regression below
  # this follows the literature (e.g. Dostie et al. 2021 and Li et al. 2023)
  
  cat("\nbeginning residualization of log wage...")
  
  est <- felm(log_wage ~ norm_age_sq + norm_age_cubed + norm_age_quart + married | year + prov_of_res, data = data)
  
  data$resid <- est$residuals
  
  rm(est)
  
  cat("\n\n\nfinished residualizing log wage. Data is ready for akm estimation.\n")
  
  return(data)
  
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
  
  cat("\n how many observations not missing worker or firm FEs:")
  print(main_data[!is.na(worker_fe) & !is.na(firm_fe), .N]/main_data[, .N])
  
  main_data <- main_data[!is.na(worker_fe) & !is.na(firm_fe)]
  
  cat("\ndata with earnings, fixed effects, and other covariates:\n")
  glimpse(main_data)
  
  return(main_data)
  
}



# variance decomposition --------------------------------------------------

var_decomp <- function(y, worker_fe, firm_fe) {
  
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
  
  return(results)
  
}


