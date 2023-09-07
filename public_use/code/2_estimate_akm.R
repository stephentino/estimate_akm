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



# -------------------------------------------------------------------------


log_path <- file(paste0(main_directory,"/public_use/logs/2_estimate_akm.txt"), open = "wt")
log_open(log_path)


# load data ---------------------------------------------------------------

main_data <- vload(main_directory, "job_data")

main_data <- main_data[, .(person_id,
                           year,
                           firm_id,
                           age,
                           age_sq = age^2,
                           log_earnings)]

main_data <- get_akm_data(main_data) # this function applies some filters to the data for akm estimation

# estimate model and extract results --------------------------------------

cat("\n\n\nbeginning estimation of FE model....")

# note: the linear term in the polynomial in age is not identified when there are person and year effects
est <- felm(log_earnings ~ age_sq + year | person_id + firm_id, data = main_data) 

cat("\n\n")
cat("\nestimation of FE model complete. Results:\n")

summary(est)

cat("\n\n")

coefs <- est$coefficients

coefs <- data.table(variable = dimnames(coefs)[[1]],
                    estimate = coefs)

cat("\nSaving coefficients...\n")

vsave(coefs, main_directory, "est_coefs")

cat("\nCoefficients saved.\n")

cat("\nExtracting FEs....")

fe <- getfe(est)

cat("\nExtraction of FE complete. Saving FEs...\n")

vsave(fe, main_directory, "est_fe_data")

cat("\nFEs saved.\n")


# Variance decomposition --------------------------------------------------

fe <- as.data.table(fe)

main_data <- get_decomp_data(main_data, fe) # this function merges the FEs into the data with earnings

vsave(main_data, main_directory, "akm_data")

rm(fe)

results <- var_decomp(y = main_data$log_earnings, 
                      worker_fe = main_data$worker_fe,
                      firm_fe = main_data$firm_fe)

fwrite(results, file = paste0(main_directory,"/public_use/results/akm_variance_decomposition.csv"),
       col.names = TRUE, row.names = FALSE, append = FALSE)



log_close()