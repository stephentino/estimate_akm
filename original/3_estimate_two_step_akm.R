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


# ----------------------------------------------------------------------------
# prepare R script -----------------------------------------------------------
# ----------------------------------------------------------------------------

rm(list = ls())
.libPaths("U:/R/Packages/4.0_top")


path_to_data <- "//tor-main/Projects2/Tino_10328/Data/mee_data_with_spells_20230831.rds"
#path_to_data <- "//tor-main/Projects2/Tino_10328/Data/akm_subsample_20230815.rds"

current_date <- "20230901"


# packages and functions ---------------------------------------------


library("data.table")
library("datawizard")
library("dplyr")
library("ggm")
library("Matrix")
library("SparseM")
library("reshape2")
library("igraph")
library("zoo")
library("broom")
library("lfe")

source("//tor-main/Projects2/Tino_10328/Code/utils/functions_1.R")
source("//tor-main/Projects2/Tino_10328/Code/utils/akm_functions.R")


# switches ---------------------------------------------------------------


full_sample <- 1
roe_subsample <- 1
immigrants_natives <- 1
different_decades_ss <- 1
different_decades_roe_ss <- 1
consecutive_roe_ss <- 1
dd_consecutive_roe_ss <- 1


use_immigrant_native_dual_connected_set <- TRUE



# log file ----------------------------------------------------------------


log_path <- file(paste0("//tor-main/Projects2/Tino_10328/Logs/3_estimate_two_step_akm_",current_date,".txt"), open = "wt") 
log_open(log_path)



# -------------------------------------------------------------------------
# prepare data and estimate akm, separately each sample  ------------------
# -------------------------------------------------------------------------



# full sample -------------------------------------------------------------



if(full_sample == 1) {
  
  cat("\n\n\nfull sample\n\n\n")
  
  model_name <- "two_step_full_sample"
  
  filename<- path_to_data
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn,
                             prov_of_res,
                             married,
                             age)]
  
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  
  
}


# roe subsample -----------------------------------------------------------


if (roe_subsample == 1) {
  
  
  cat("\n\n\nROE sample\n\n\n")
  
  # roe subsample, annual earnings ------------------------------------------
  
  model_name <- "roe_subsample"
  
  
  filename<- path_to_data 
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  
  # additional filters (compared to the baseline full sample)
  main_data[, roe_separation := as.integer(!is.na(last_year_worked) & !is.na(last_month_worked) & !is.na(first_year_worked) & !is.na(first_month_worked) )]
  main_data <- main_data[roe_separation ==1]
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn,
                             prov_of_res,
                             married,
                             age)]
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  
  
  
  # roe subsample, monthly earnings -----------------------------------------
  
  model_name <- "roe_subsample_monthly"
  
  filename<- path_to_data
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  # additional filters (compared to the baseline full sample)
  main_data[, roe_separation := as.integer(!is.na(last_year_worked) & !is.na(last_month_worked) & !is.na(first_year_worked) & !is.na(first_month_worked) )]
  main_data <- main_data[roe_separation ==1]
  main_data <- get_monthly_earnings(main_data)
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn = monthly_earnings, # note: I rename monthly earnings to real_t4earn
                             prov_of_res,
                             married,
                             age)]
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  
  
}



# immigrants & natives -----------------------------------------

if (immigrants_natives == 1 ) {
  
  cat("\n\n\nnatives sample\n\n\n")
  
  # natives -----------------------------------------------------
  
  model_name <- "native"
  
  filename<- path_to_data
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  # additional filter compared to full_sample
  main_data <- main_data[temporary_resident == 0] 
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn,
                             prov_of_res,
                             married,
                             age,
                             permanent_resident)]
  
  
  # note: I compute the dual connected set here to compare FEs across immigrants/natives
  main_data <- get_akm_data(main_data,
                            compute_residuals = TRUE, 
                            dual_connected = use_immigrant_native_dual_connected_set,
                            group_var = "permanent_resident",
                            group_value = 0) 
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  


  # immigrants --------------------------------------------------------------
  
  cat("\n\n\nimmigrant sample\n\n\n")
  
  model_name <- "immigrant"
  
  filename<- path_to_data
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)

  
  # additional filter compared to full_sample
  main_data <- main_data[temporary_resident == 0] 
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn,
                             prov_of_res,
                             married,
                             age,
                             permanent_resident)]
  
  
  # note: I compute the dual connected set here to compare FEs across immigrants/natives
  main_data <- get_akm_data(main_data,
                            compute_residuals = TRUE, 
                            dual_connected = use_immigrant_native_dual_connected_set,
                            group_var = "permanent_resident",
                            group_value = 1) 
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  

}




# different decades -------------------------------------------------------



if (different_decades_ss == 1 ) {
  
  cat("\n\n\n2001 to 2005 sample\n\n\n")
  

# 2001 to 2005 ------------------------------------------------------------
  
  model_name <- "five_years_2001to2005"
  
  filename<- path_to_data
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn,
                             prov_of_res,
                             married,
                             age)]
  
  #additional filter compared to full sample
  main_data <- main_data[year >=2001 & year <= 2005]
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  

# 2011 to 2015 ------------------------------------------------------------

  cat("\n\n\n2011 to 2015 sample\n\n\n")
  
  model_name <- "five_years_2011to2015"
  
  filename<- path_to_data
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn,
                             prov_of_res,
                             married,
                             age)]
  
  #additional filter compared to full sample
  main_data <- main_data[year >=2011 & year <= 2015]
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  
}


if(different_decades_roe_ss == 1) {
  
  cat("\n\n\nROE sample (2001 to 2005)\n\n\n")
  
  # roe subsample, annual earnings ------------------------------------------
  
  model_name <- "roe_subsample_2001to2005"
  
  
  filename<- path_to_data 
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  
  # additional filters (compared to the baseline full sample)
  main_data[, roe_separation := as.integer(!is.na(last_year_worked) & !is.na(last_month_worked) & !is.na(first_year_worked) & !is.na(first_month_worked) )]
  main_data <- main_data[roe_separation ==1]
  main_data <- main_data[year >=2001 & year <= 2005]
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn,
                             prov_of_res,
                             married,
                             age)]
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  
  
  
  # roe subsample, monthly earnings -----------------------------------------
  
  model_name <- "roe_subsample_monthly_2001to2005"
  
  filename<- path_to_data
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  # additional filters (compared to the baseline full sample)
  main_data[, roe_separation := as.integer(!is.na(last_year_worked) & !is.na(last_month_worked) & !is.na(first_year_worked) & !is.na(first_month_worked) )]
  main_data <- main_data[roe_separation ==1]
  main_data <- main_data[year >=2001 & year <= 2005]
  main_data <- get_monthly_earnings(main_data)
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn = monthly_earnings, # note: I rename monthly earnings to real_t4earn
                             prov_of_res,
                             married,
                             age)]
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  
  
  
  
  cat("\n\n\nROE sample (2011 to 2015)\n\n\n")
  
  # roe subsample, annual earnings ------------------------------------------
  
  model_name <- "roe_subsample_2011to2015"
  
  
  filename<- path_to_data 
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  
  # additional filters (compared to the baseline full sample)
  main_data[, roe_separation := as.integer(!is.na(last_year_worked) & !is.na(last_month_worked) & !is.na(first_year_worked) & !is.na(first_month_worked) )]
  main_data <- main_data[roe_separation ==1]
  main_data <- main_data[year >=2011 & year <= 2015]
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn,
                             prov_of_res,
                             married,
                             age)]
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  
  
  
  # roe subsample, monthly earnings -----------------------------------------
  
  model_name <- "roe_subsample_monthly_2011to2015"
  
  filename<- path_to_data
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  # additional filters (compared to the baseline full sample)
  main_data[, roe_separation := as.integer(!is.na(last_year_worked) & !is.na(last_month_worked) & !is.na(first_year_worked) & !is.na(first_month_worked) )]
  main_data <- main_data[roe_separation ==1]
  main_data <- main_data[year >=2011 & year <= 2015]
  main_data <- get_monthly_earnings(main_data)
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn = monthly_earnings, # note: I rename monthly earnings to real_t4earn
                             prov_of_res,
                             married,
                             age)]
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  
  
}



# consecutive subsample -----------------------------------------------------------


if (consecutive_roe_ss == 1) {
  
  
  cat("\n\n\nCONSECUTIVE ROE sample\n\n\n")
  
  # roe subsample, annual earnings ------------------------------------------
  
  model_name <- "consecutive_roe_subsample"
  
  
  filename<- path_to_data 
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  
  # additional filters (compared to the baseline full sample)
  main_data <- get_croe(main_data)
  main_data <- main_data[croe_separation ==1]
  
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn,
                             prov_of_res,
                             married,
                             age)]
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  
  
  
  # roe subsample, monthly earnings -----------------------------------------
  
  model_name <- "consecutive_roe_subsample_monthly"
  
  filename<- path_to_data
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  # additional filters (compared to the baseline full sample)
  main_data <- get_croe(main_data)
  main_data <- main_data[croe_separation ==1]
  main_data <- get_monthly_earnings(main_data)
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn = monthly_earnings, # note: I rename monthly earnings to real_t4earn
                             prov_of_res,
                             married,
                             age)]
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  
  
}



if(dd_consecutive_roe_ss == 1) {
  
  cat("\n\n\nCONSECUTIVE ROE sample (2001 to 2005)\n\n\n")
  
  # roe subsample, annual earnings ------------------------------------------
  
  model_name <- "consecutive_roe_subsample_2001to2005"
  
  
  filename<- path_to_data 
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  
  # additional filters (compared to the baseline full sample)
  main_data <- get_croe(main_data)
  main_data <- main_data[croe_separation ==1]
  main_data <- main_data[year >=2001 & year <= 2005]
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn,
                             prov_of_res,
                             married,
                             age)]
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  
  
  
  # roe subsample, monthly earnings -----------------------------------------
  
  model_name <- "consecutive_roe_subsample_monthly_2001to2005"
  
  filename<- path_to_data
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  # additional filters (compared to the baseline full sample)
  main_data <- get_croe(main_data)
  main_data <- main_data[croe_separation ==1]
  main_data <- main_data[year >=2001 & year <= 2005]
  main_data <- get_monthly_earnings(main_data)
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn = monthly_earnings, # note: I rename monthly earnings to real_t4earn
                             prov_of_res,
                             married,
                             age)]
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  
  
  
  
  cat("\n\n\nCONSECUTIVE ROE sample (2011 to 2015)\n\n\n")
  
  # roe subsample, annual earnings ------------------------------------------
  
  model_name <- "consecutive_roe_subsample_2011to2015"
  
  
  filename<- path_to_data 
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  
  # additional filters (compared to the baseline full sample)
  main_data <- get_croe(main_data)
  main_data <- main_data[croe_separation ==1]
  main_data <- main_data[year >=2011 & year <= 2015]
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn,
                             prov_of_res,
                             married,
                             age)]
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  
  
  
  # roe subsample, monthly earnings -----------------------------------------
  
  model_name <- "consecutive_roe_subsample_monthly_2011to2015"
  
  filename<- path_to_data
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  # additional filters (compared to the baseline full sample)
  main_data <- get_croe(main_data)
  main_data <- main_data[croe_separation ==1]
  main_data <- main_data[year >=2011 & year <= 2015]
  main_data <- get_monthly_earnings(main_data)
  
  main_data <- main_data[, .(casenum2021,
                             year,
                             entid_syn,
                             real_t4earn = monthly_earnings, # note: I rename monthly earnings to real_t4earn
                             prov_of_res,
                             married,
                             age)]
  
  main_data <- get_akm_data(main_data)
  
  estimate_akm_and_var_decomp(main_data, model_name, current_date)
  
  
}



log_close()
