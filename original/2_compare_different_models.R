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


# Note: categorical variables to the right of the '|' symbol in the felm() function from lfe are partialled out
# anything to the left of '|' are estimated in a way similar to the lm() function from base R.
# This code tests whether the results from the estimation of a FE model differ if the categorical variables are partialed out or not
# I use a subset of the full data to increase computational speed 
# (also, it is not computationally feasible to include all categorical variables as covariates in the full data)

# in addition, this code also tests a "two step procedure", whereby the log wage is residualized before running akm



rm(list = ls())

.libPaths("U:/R/Packages/4.0_top")

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

#-----------------------------------------


log_path <- file("//tor-main/Projects2/Tino_10328/Logs/4_compare_different_models_20230822.txt", open = "wt")
log_open(log_path)



# load data ---------------------------------------------------------------

if (0 == 1) {
 
  filename<- "//tor-main/Projects2/Tino_10328/Data/mee_data_20230815.rds"
  cat("loading...\n")
  print(filename)
  main_data <- readRDS(file = filename)
  
  subset <- main_data[year>=2017 & year <= 2019 & (prov_of_res == 59 | prov_of_res== 48) ]
  
  saveRDS(subset, file = "//tor-main/Projects2/Tino_10328/Data/akm_subsample_20230815.rds")
  
}


filename<- "//tor-main/Projects2/Tino_10328/Data/akm_subsample_20230815.rds"
cat("loading...\n")
print(filename)
main_data <- readRDS(file = filename)



# prepare data ------------------------------------------------------------

main_data <- get_akm_data(main_data, compute_residuals = FALSE)

# estimate various models and extract results --------------------------------------


if (0 == 1) {

# test that I can use "1" as a covariate without issues -------------------

est1 <- felm(log_wage ~ prov_of_res + year, data = main_data)

est2 <- felm(log_wage ~ 1 | prov_of_res + year, data = main_data)

cat("\n\n\ntesting that I can use '1' as covariate:\n")
print(all.equal(est1$residuals,est2$residuals))

cat("\n\n\n")

print(summary(est1))

print(summary(est2))

rm(est1)
rm(est2)

}

# partialling out covariates vs including them on LHS ----------------------------

if (1 == 0) {
  
# simpler models: without firm and individual FEs -------------------------

# first model includes all covariates on LHS
  
est <- felm(log_wage ~ norm_age_sq + norm_age_cubed + norm_age_quart + prov_of_res + year, data = main_data)

print(summary(est))

coefs <- est$coefficients

coefs <- data.table(variable = dimnames(coefs)[[1]],
                    estimate = coefs)


cat("\n\nresults from model 1:\n\n")

coefs

# -------------

# second model partials out the year and prov FEs

est2 <- felm(log_wage ~ norm_age_sq + norm_age_cubed + norm_age_quart | prov_of_res + year, data = main_data)

print(summary(est2))

coefs2 <- est2$coefficients
  
coefs2 <- data.table(variable = dimnames(coefs2)[[1]],
                                estimate = coefs2)

fe2 <- getfe(est2)

fe2 <- as.data.table(fe2)

cat("\n\nresults from model 2:\n\n")

coefs2

fe2[fe=="year"]

fe2[fe=="prov_of_res"]

}

if (1 == 0) {
  
# models with firm and individual FEs --------------------------------------

# year, prov effects, and married dummy included covariates on LHS
  
  
main_data[, married := as.factor(married)]
  
est3 <- felm(log_wage ~ norm_age_sq + prov_of_res + year + married | casenum2021 + entid_syn, data = main_data)

print(summary(est3))

coefs3 <- est3$coefficients

coefs3 <- data.table(variable = dimnames(coefs3)[[1]],
                    estimate = coefs3)


fe3 <- getfe(est3)

fe3 <- as.data.table(fe3)

cat("\n\nresults from model 3:\n\n")

coefs3


# year, prov, and married effects partialled out

est4 <- felm(log_wage ~ norm_age_sq  | casenum2021 + entid_syn + prov_of_res + year + married, data = main_data)

print(summary(est4))

coefs4 <- est4$coefficients

coefs4 <- data.table(variable = dimnames(coefs4)[[1]],
                     estimate = coefs4)

fe4 <- getfe(est4)

fe4 <- as.data.table(fe4)


cat("\n\nresults from model 4:\n\n")

print(coefs4)

fe4[fe=="year"]

fe4[fe=="prov_of_res"]

fe4[fe=="married"]


# --------

compare_lfe_results(est1 = est3,
                    fe1 = fe3,
                    est2 = est4,
                    fe2 = fe4, 
                    comparison_name = "partial out covariates")
  
}

# comparing one step vs two step procedure ------------------------------------------------------

if (1 == 0) {

# first model residualizes log wages first, then estimates akm with residuals
  
est5 <- felm(log_wage ~ norm_age_sq + norm_age_cubed + norm_age_quart + year, data = main_data)

main_data$resid <- est5$residuals

est6 <- felm(resid ~ 1| entid_syn + casenum2021, data = main_data)

fe6 <- getfe(est6)

fe6 <- as.data.table(fe6)


# second model estimates akm with additional covariates 

est7 <- felm(log_wage ~ norm_age_sq + norm_age_cubed + norm_age_quart + year | entid_syn + casenum2021, data = main_data)

fe7 <- getfe(est7)

fe7 <- as.data.table(fe7)

compare_lfe_results(est1 = est6,
                    fe1 = fe6,
                    est2 = est7,
                    fe2 = fe7, 
                    comparison_name = "two step procedure")
}



# note: the variance decomposition is very similar if either the one step or two step procedure is used
# the estimates are not identical, however, because this is *not* the FWL theorem
# in order for the FWL theorem to apply, we also need to residualize the covariates 
# the estimates are "nearly identical", see LMS footnote 16 for similar findings

log_close()


