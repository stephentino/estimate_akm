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

source(paste0(main_directory,"/public_use/code/1_simulate_data.R"), print.eval= TRUE)
source(paste0(main_directory,"/public_use/code/2_estimate_akm.R"), print.eval = TRUE)
source(paste0(main_directory,"/public_use/code/3_estimate_two_step_akm.R"), print.eval = TRUE)
source(paste0(main_directory,"/public_use/code/check_results.R"), print.eval = TRUE)
