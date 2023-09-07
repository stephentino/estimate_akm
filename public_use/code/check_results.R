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


log_path <- file(paste0(main_directory,"/public_use/logs/check_results.txt"), open = "wt")
log_open(log_path)

est_data <- vload(main_directory, "akm_data")

est_data[, year := as.character(year)]

true_data <- vload(main_directory, "fe_data")

true_data <- as.data.table(true_data)

true_data <- true_data[, .(person_id = as.character(person_id), 
                           firm_id = as.character(firm_id),
                           year = as.character(year),
                           true_worker_effect = person_effect,
                           true_firm_effect = firm_effect)]

main_data <- merge(x = est_data,
                   y = true_data,
                   all.x = FALSE,
                   all.y = FALSE,
                   by = c("person_id","firm_id","year"))

rm(true_data)
rm(est_data)

main_data <- main_data[!is.na(worker_fe)]

results <- var_decomp(y = main_data$log_earnings, 
                      worker_fe = main_data$true_worker_effect,
                      firm_fe = main_data$true_firm_effect)

fwrite(results, file = paste0(main_directory,"/public_use/results/true_variance_decomposition.csv"),
       col.names = TRUE, row.names = FALSE, append = FALSE)


log_close()
