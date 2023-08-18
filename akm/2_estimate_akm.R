
# Author: Stephen Tino


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


log_path <- file("//tor-main/Projects2/Tino_10328/Logs/2_estimate_akm_20230816.txt", open = "wt")
log_open(log_path)


# load data ---------------------------------------------------------------

filename<- "//tor-main/Projects2/Tino_10328/Data/mee_data_20230816.rds"
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


# prepare data ------------------------------------------------------------

# restrict to workers/firms observed at least twice to identify FEs -----------------------

main_data[, n_worker := .N, by = casenum2021 ]
main_data[, n_firm := .N, by = entid_syn ]

main_data <- main_data[n_worker >= 2][, n_worker := NULL]
main_data <- main_data[n_firm >= 2][, n_firm := NULL]


#  extract list of movers to compute the connected set ---------------------------------------

setkey(main_data,casenum2021,year)

main_data[, lag_entid_syn := shift(entid_syn, 1, type = "lag")]
main_data[, lag_casenum2021 := shift(casenum2021, 1, type = "lag")]

main_data[, move := as.integer(0)]
main_data[entid_syn != lag_entid_syn & casenum2021 == lag_casenum2021, move := as.integer(1)]
main_data[year == min(year), move := NA]


# extract connected set ---------------------------------------------------

# note: the lfe package automatically does this,
# but I do this before running felm from the lfe package to reduce the computational burden when estimating the model)

jdata <- main_data[move==1, .(j1=lag_entid_syn, j2=entid_syn)]

output <- get_connected_set(jdata)

main_data <- merge(x = main_data, 
                   y = output, 
                   all.x = TRUE, # keep all firms 
                   all.y = FALSE, 
                   by ="entid_syn")

cat("\n\nany firms missing connected status do not employ movers. fraction of the sample:\n")
main_data[is.na(connected), .N]/main_data[, .N]

main_data[is.na(connected), connected := 0]

cat("\nfraction of worker-firms in the connected set:\n")
mean(main_data$connected, na.rm = TRUE)


# more data prep ----------------------------------------------------------

main_data[, casenum2021 := as.factor(casenum2021)]

main_data[, entid_syn := as.factor(entid_syn)]

main_data[, year := as.factor(year)]

main_data[, prov_of_res := as.factor(prov_of_res)]

main_data <- main_data[, .(casenum2021, entid_syn, log_wage = log(real_t4earn), year, prov_of_res, age, married, connected)]

main_data <- main_data[connected==1]


# normalize age  ----------------------------------------------------------


# normalization of age before running akm follows dostie, card, li, and parent 
# I do this by: first, residualizing the wage using time-varying covariates
# next, determining which age has the largest residualized log earnings 
# finally, normalizing age subtracting and dividing by it  

est <- felm(log_wage ~ married | year + prov_of_res, data = main_data)

cat("\n\n\nfinished estimating linear model for age normalization.\n")

main_data$resid <- est$residuals

rm(est)

main_data[, avg_resid := mean(resid, na.rm = TRUE), by = age]

age_data <- unique(main_data[, .(age, avg_resid)])

age_data[, max_resid := max(avg_resid, na.rm = TRUE)]

cat("\nage of maximum residuals:\n")
normalization_factor <- age_data[which(age_data$max_resid==age_data$avg_resid), age]
print(normalization_factor)

main_data[, norm_age := (age-normalization_factor)/normalization_factor] 

main_data[, norm_age_sq := (norm_age^2)/1000]

main_data[, norm_age_cubed := (norm_age^3)/1000]

main_data[, norm_age_quart := (norm_age^4)/1000]


# estimating the model is quite memory intensive; I need to clear the environment as much as possible
rm(jdata)
rm(output)
rm(age_data)
main_data[, resid := NULL]
main_data[, avg_resid := NULL]
main_data[, age := NULL]


# estimate model and extract results --------------------------------------

cat("\n\n\nbeginning estimation of FE model....")

# note [1]: make sure that the individual and firm FE are the first two on the RHS of '|' in felm()
# this is important because only the first two factors are used to report connected components

# note [2]: it is difficult to estimate this model and extract the coefficients due to memory issues
# what seems to work at this point is to partial out all categorical variables except "married",

est <- felm(log_wage ~ norm_age_sq + norm_age_cubed + norm_age_quart + married | casenum2021 + entid_syn + year + prov_of_res, data = main_data)

cat("\n\n\n")
cat("estimation of FE model complete. Results:\n")

summary(est)

cat("\n\n\n")

coefs <- est$coefficients

coefs <- data.table(variable = dimnames(coefs)[[1]],
                    estimate = coefs)

cat("\n\nSaving coefficients...\n")

saveRDS(coefs, file = "//tor-main/Projects2/Tino_10328/Data/coefs_20230816.rds")

cat("\nCoefficients saved.\n")

cat("\nExtracting FEs....")

fe <- getfe(est)

cat("\n\nExtraction of FE complete. Saving FEs...")

saveRDS(fe, file = "//tor-main/Projects2/Tino_10328/Data/fe_20230816.rds")

cat("FEs saved.")


# check that my connected set matches lfe's ------------------------------------------------------------

# the getfe() function produces a variable "comp" which shows which firms and individuals are in the largest connected set
# I expect all individuals and firms are in the largest connected set since I already filtered out firms not in the connected set before running the lfe package
# Below, I check to make sure that this is the case and there are no errors in my code


fe <- as.data.table(fe)

firm_fe <-  fe[fe == "entid_syn", .(comp, firm_effect = effect, entid_syn = as.character(idx))] 

main_data[, entid_syn := as.character(entid_syn)]

main_data <- merge(x = main_data,
                   y = firm_fe, 
                   all.x = TRUE,
                   all.y = TRUE,
                   by = "entid_syn")

cat("\n\n\ninvestigating whether connected sets differ....\n\n")
main_data[comp==1, .N]/main_data[, .N]
main_data[comp!=1, .N]
main_data[comp !=1 & connected==1, .N]/main_data[, .N]



log_close()