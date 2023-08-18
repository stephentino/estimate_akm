
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
source("//tor-main/Projects2/Tino_10328/Code/utils/akm_functions.R")


log_path <- file("//tor-main/Projects2/Tino_10328/Logs/5_estimate_akm_with_residuals_20230817.txt", open = "wt")
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


cat("\ndata before final data cleaning:\n")
glimpse(main_data)

main_data <- get_akm_data(main_data) # this function applies some filters to the data for akm estimation

cat("\ncleaned data ready for akm estimation:\n")
glimpse(main_data)

cat("\nexporting akm data...\n")
saveRDS(main_data, file = "//tor-main/Projects2/Tino_10328/Data/akm_data_20230817.rds")


# estimate model and extract results --------------------------------------


cat("\n\n\nbeginning estimation of FE model....")

set.seed(2012)
main_data[, x := rnorm(n = .N)] # create a covariate to run the felm() function

est <- felm(resid ~ x | casenum2021 + entid_syn, data = main_data)

cat("\n\n\n")
cat("estimation of FE model complete. Results:\n")

summary(est)

cat("\nExtracting FEs....")

fe <- getfe(est)

rm(est)

cat("\n\nExtraction of FE complete. Saving FEs...")

saveRDS(fe, file = "//tor-main/Projects2/Tino_10328/Data/resid_fe_20230817.rds")

cat("FEs saved.")



# variance decomposition --------------------------------------------------


fe <- as.data.table(fe)

main_data <- get_decomp_data(main_data, fe) # this function merges the FEs into the data with earnings

rm(fe)


results <- var_decomp(y = main_data$resid, 
                      worker_fe = main_data$worker_fe,
                      firm_fe = main_data$firm_fe)

fwrite(results, file = "//tor-main/Projects2/Tino_10328/summary_statistics/akm_with_residuals_var_decomp_20230817.csv",
       col.names = TRUE, row.names = FALSE, append = FALSE)



log_close()