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


rm(list = ls())

library("data.table")
library("dplyr")
library("ggm")
library("Matrix")
library("SparseM")
library("reshape2")
library("igraph")
library("zoo")
library("broom")
library("lfe")

main_directory <- "/Users/Stephen/Dropbox/40. github/estimate_akm"

source(paste0(main_directory,"/public_use/code/utils/akm_functions.R"))
source(paste0(main_directory,"/public_use/code/utils/functions_1.R"))

log_path <- file(paste0(main_directory,"/public_use/logs/3_two_step_akm.txt"), open = "wt")
log_open(log_path)



# load data ---------------------------------------------------------------

main_data <- vload(main_directory, "job_data")

main_data <- main_data[, .(person_id,
                           year,
                           firm_id,
                           age,
                           age_sq = age^2,
                           log_earnings)]


cat("\ndata before final data cleaning:\n")
glimpse(main_data)

main_data <- get_akm_data(main_data) # this function applies some filters to the data for akm estimation


# residualize log wages for two-way FE estimation -------------------------

# note: since age is perfectly colinear with birth year and the akm regression includes individual FE
# therefore I exclude the linear term of the quartic polynomial in age in the regression below
# this follows the literature (e.g. Dostie et al. 2021 and Li et al. 2023)

cat("\nbeginning residualization of log wage...")

est <- felm(log_earnings ~ age_sq | year, data = main_data)

main_data$resid <- est$residuals

rm(est)

cat("\n\nfinished residualizing log earnings. Data is ready for akm estimation.\n")


# estimate model and extract results --------------------------------------

cat("\n\n\nbeginning estimation of FE model....")

est <- felm(resid ~ 1 | person_id + firm_id, data = main_data)

cat("\n\n\n")
cat("estimation of FE model complete. Results:\n")

summary(est)

cat("\nExtracting FEs....")

fe <- getfe(est)

rm(est)

cat("\n\nExtraction of FE complete. Saving FEs...")

vsave(fe, main_directory, "two_step_est_fe_data")

cat("FEs saved.")



# variance decomposition --------------------------------------------------


fe <- as.data.table(fe)

main_data <- get_decomp_data(main_data, fe) # this function merges the FEs into the data with earnings

rm(fe)


results <- var_decomp(y = main_data$log_earnings, 
                      worker_fe = main_data$worker_fe,
                      firm_fe = main_data$firm_fe)

fwrite(results, file = paste0(main_directory,"/public_use/results/two_step_akm_variance_decomposition.csv"),
       col.names = TRUE, row.names = FALSE, append = FALSE)



log_close()