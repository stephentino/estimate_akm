
# Author: Stephen Tino (2023)

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

source("//tor-main/Projects2/Tino_10328/Code/utils/functions_1.R")


log_path <- file("//tor-main/Projects2/Tino_10328/Logs/1A_merge_and_clean_20230831.txt", open = "wt")
log_open(log_path)


#------------------------------------------------#
# construct matched employer-employee (MEE) data #
#------------------------------------------------#


# load T4ROE data -----------------------------------------------------------

# note: the T4ROE data contains data on *jobs* (individual-firm pairs)
# it must be merged with the T1PMF data which contains info on *individuals* and the NALMF data which contains info on *firms* to construct the MEE data
# before merging, I decrease the size of the data by restricting to "primary jobs" (the job that pays the individual the most in each year)

cat("\n\n")
filename <- "//tor-main/Projects2/Tino_10328/Data/t4roe_2001_2019_noFilters_20230512"
cat("loading data:\n")
print(filename)
roe_data <- readRDS(file = filename)

roe_data <- roe_data[,.(casenum2021,entid_syn,year,t4earn,naics_scan)]

glimpse(roe_data)


# keep only "primary jobs" (those that pay the most in any year)
roe_data[, max_t4earn := max(t4earn), by = .(casenum2021,year)]
roe_data <- roe_data[max_t4earn == t4earn]

roe_data[, max_t4earn := NULL]

# sometimes there is more than one "primary job"
# pick one randomly in these cases so that each individual-year is unique

set.seed(1992)

roe_data[,random_number := runif(.N)]

setkey(roe_data,casenum2021,year,random_number)

roe_data[,obs_number := seq_len(.N), by = .(casenum2021,year)]

roe_data <- roe_data[obs_number == 1]

roe_data[,obs_number := NULL]
roe_data[,random_number := NULL]

setkey(roe_data,casenum2021,year)

cat("\n\n")
cat("ROE data before merging...\n")
glimpse(roe_data)



# load t1pmf data -----------------------------------------------------------

# the t1pmf data contains info on *individuals* from their tax returns
# will apply some filters before merging with the t4roe data 


cat("\n\n")
filename <- "//tor-main/Projects2/Tino_10328/Data/t1pmf_workingAge_20230530"
print("loading data:\n")
print(filename)
pmf_data <- readRDS(file = filename)

glimpse(pmf_data)

pmf_data[,t1_uiearn := NULL]
pmf_data[,sactype := NULL]
pmf_data[,t1_birth_year_harmonized := NULL]


# adjust earnings for inflation -------------------------------------------

# note (1): 18733 in 2012 dollars is the full-time equivalent threshold (LDS, 2023)

# note (2): the t1_t4earn variable in the t1pmf should be equal to the sum of all t4 earnings for each individual
# I will later filter out jobs that are less than the full-time equivalent threshold of $18,733 (2012 dollars)
# I apply this also to individuals' total earnings in the t1pmf to reduce the size of the t1pmf data before merging with the t4roe job data 

cpi_data <- readRDS(file = "//tor-main/Projects2/Tino_10328/Data/provincial_cpi")

pmf_data <- merge(x = pmf_data, 
                  y = cpi_data, 
                  all.x = FALSE, 
                  all.y = FALSE, 
                  by = c("prov_of_res","year"))

cat("\n\n filtering out individuals in the t1pmf data based on FTE:\n")

pmf_data[( t1_t4earn/(cpi/100) ) >= 18733, .N ]/pmf_data[,.N]
pmf_data <- pmf_data[( t1_t4earn/(cpi/100) ) >= 18733]

pmf_data[, t1_t4earn := NULL]


# merge t1pmf and t4roe datasets -----------------------------------------------------------


cat("\n\n")
cat("\nPMF data before merging...")
glimpse(pmf_data)

main_data <- merge(x = pmf_data, 
                   y = roe_data, 
                   by = c("casenum2021","year"), 
                   all.x = FALSE, 
                   all.y = FALSE)

cat("\n\n")
cat("\ndimensions of data before/after merge...\n")

print(dim(main_data)/dim(pmf_data))
print(dim(main_data)/dim(roe_data))

glimpse(main_data)

rm(roe_data)
rm(pmf_data)

# adjust the T4ROE earnings for inflation -------------------------------------------

#note: 18733 in 2012 dollars is the full-time equivalent threshold (LDS, 2023)

cat("\n\n")
cat("filtering out individuals in the t4roe data based on FTE:\n")

main_data[, real_t4earn :=  t4earn/(cpi/100)]

main_data[ real_t4earn >= 18733, .N ]/main_data[,.N]
main_data <- main_data[ real_t4earn >= 18733]

main_data[, t4earn := NULL]



# more data cleaning ------------------------------------------------------


cat("\n\n")
cat("\nremove individuals with missing sex or sex ==3 : \n")

main_data[t1_sex_harmonized >2, .N]/main_data[,.N]
main_data <- main_data[t1_sex_harmonized <= 2]

main_data[,male := as.integer(t1_sex_harmonized==1)]

main_data[,t1_sex_harmonized := NULL]

cat("\n\n")
cat("\nremove individuals with unstated marital status")

main_data[marst==0,.N]/main_data[,.N]
main_data <- main_data[marst !=0]

main_data[, married := as.integer( marst ==1 | marst == 2)]

main_data[, marst := NULL]




# filter out individuals who are primarily self-employed ------------------

# the definition of self-employment income here follows Dostie's AKM papers (e.g. Dostie, Li, Card et al.):
# it is the sum of farming income, business income, fishing income, commission, and other income from an unincorporated profession
# I created the list of individuals with self-employment income less than employment income previously using the full t1pmf data
# here, I load that list of individuals and filter out individuals not in the last from the main data


cat("\n\n")

# import the list of individuals with self emp less than t4 earnings to filter self-employed individuals out
filename <- "//tor-main/Projects2/Tino_10328/Data/stata_export/t1pmf_2001_2019_20230831_EmpGreaterSelfEmpGross.txt"
print("loading data:\n")
print(filename)
employee_data <-  fread(file = filename)

old_dim <- dim(main_data)

main_data <- merge(x = main_data, 
                   y = employee_data, 
                   by = c("casenum2021","year"), 
                   all.x = FALSE, 
                   all.y = FALSE)

dim(main_data)/old_dim

rm(employee_data)

cat("\n\n")
cat("data after restricting to individuals with emp income > self-emp income...\n")
glimpse(main_data)



# load NALMF data ---------------------------------------------------------


cat("\n\n")
filename <- "//tor-main/Projects2/Tino_10328/Data/stata_export/nalmf_2001_2019_20230512.txt"
print("loading data:\n")
print(filename)
firm_data <- fread(file = filename)

glimpse(firm_data)



# data cleaning -----------------------------------------------------------

# I use pd7 employment (averaged over all months) as my preferred measure of employment
# this follows LDS (2023)

firm_data[, employment := pd7_avgemp_nonzero]
firm_data[, pd7_avgemp_nonzero := NULL]

cat("\n\n")
cat("\nkeep incorporated firms only\n")

firm_data[incorporated==1,.N]/firm_data[,.N]
firm_data <- firm_data[incorporated == 1]

cat("\n\n")
cat("\nremove firms that have less than 2 workers\n")

firm_data[is.na(employment), employment := 0]
firm_data <- firm_data[employment >= 2]


firm_data <- firm_data[,.(entid_syn,year,value_added,total_revenue,employment)]

glimpse(firm_data)



# merge firm data with rest -----------------------------------------------

old_dim <- dim(main_data)

main_data <- merge(x = main_data, 
                   y = firm_data, 
                   by = c("entid_syn","year"), 
                   all.x = FALSE, 
                   all.y = FALSE)

print(dim(main_data)/old_dim)

rm(firm_data)


# more data cleaning ------------------------------------------------------


main_data[, naics_two := floor(naics_scan/100) ]

cat("\n\n")
cat("keep firms in the business sector only: (using industry variable from the T4ROE)\n")

main_data[naics_two!=91 & naics_two != 61 & naics_two != 62 & !is.na(naics_scan), .N]/main_data[,.N]
main_data <- main_data[naics_two!=91 & naics_two != 61 & naics_two != 62 & !is.na(naics_scan)]


# adjust for inflation ----------------------------------------------------

fpi_data <- readRDS(file = "//tor-main/Projects2/Tino_10328/Data/national_cpi")

# for now, I use the national CPI as my "firm price index" 
# I could redo this using industry-specific deflators

main_data <- merge(x = main_data, 
                   y = fpi_data, 
                   all.x = TRUE, 
                   all.y = FALSE, 
                   by = "year")

main_data[, real_va := value_added/(national_cpi/100)]
main_data[, real_revenue := total_revenue/(national_cpi/100)]


main_data[, value_added := NULL]
main_data[, total_revenue := NULL]
main_data[, cpi := NULL]
main_data[, national_cpi := NULL]
main_data[, naics_two := NULL]



# more data cleaning ------------------------------------------------------


cat("\n\nremove small firms and firms with low value added:")
main_data[real_va > 100, .N]/main_data[, .N]
main_data <- main_data[real_va > 100]

main_data[real_revenue > 50000, .N]/main_data[, .N]
main_data <- main_data[real_revenue > 50000]




# add permanent and temporary resident status -----------------------------


cat("loading imdb data...\n")
print(filename)
imdb_data <- fread(file = "//tor-main/Projects2/Tino_10328/Data/stata_export/imdb_20230425.txt")
imdb_data <- imdb_data[,.(casenum2021,landing_month,landing_year)]
cat("\n\n")

cat("loading temporary resident data...\n")
tr_data <- fread(file = "//tor-main/Projects2/Tino_10328/Data/stata_export/tr_20230425.txt")
tr_data[, temporary_resident := as.integer(1)]
tr_data <- unique(tr_data[,.(casenum2021,temporary_resident)])
cat("\n\n")



main_data <- merge(x = main_data,
                   y = tr_data,
                   by = c("casenum2021"),
                   all.x = TRUE,
                   all.y = FALSE)
rm(tr_data)

main_data <- merge(x = main_data,
                   y = imdb_data,
                   by = c("casenum2021"),
                   all.x = TRUE,
                   all.y = FALSE)


cat("\n data after merge...\n")
glimpse(main_data)
cat("\n\n")

cat("\nindividuals in the IMDB data missing landing_year")
sum(is.na(imdb_data$landing_year))
rm(imdb_data)


# need to keep track of temporary residents and permanent residents
# some individuals arrive first as temporary residents and then get PR
# therefore, need to be careful about assigning TR/PR status to individuals
# above, I checked that all individuals in the IMDB have landing_year, so can use this to assign PR status


main_data[is.na(temporary_resident), temporary_resident := as.integer(0)]

main_data[is.na(landing_year), permanent_resident := as.integer(0) ]

main_data[!is.na(landing_year) & year < landing_year, temporary_resident := as.integer(1)]
main_data[!is.na(landing_year) & year < landing_year, permanent_resident := as.integer(0)]

main_data[!is.na(landing_year) & year >= landing_year, permanent_resident := as.integer(1)]
main_data[!is.na(landing_year) & year >= landing_year, temporary_resident := as.integer(0)]

main_data[, landing_year := NULL]
main_data[, landing_month := NULL]

cat("\n\nnumber missing PR status: ")
print(main_data[is.na(permanent_resident), .N])

cat("\n\nnumber missing TR status: ")
print(main_data[is.na(temporary_resident), .N])

cat("\n\n")

# export data -------------------------------------------------------------


cat("\n\n")
cat("data to export:\n")
glimpse(main_data)


cat("\n\n")
cat("\nexporting the data to the following path:\n")
mee_file_path_export <- "//tor-main/Projects2/Tino_10328/Data/mee_data_20230831.rds"
print(mee_file_path_export)
saveRDS(main_data, file = mee_file_path_export)


# summary of data cleaning steps ------------------------------------------


cat("\n\n")
cat("Filters applied: ------------------------ \n")
cat("ages 25 to 59 only\n")
cat("removed individuals with sex other than male or female\n")
cat("removed individuals with unknown marital status\n")
cat("removed self-employed individuals\n")
cat("primary jobs only (in instances where there were multiple primary jobs, chose one randomly)\n")
cat("full time equivalent (FTE) only\n")
cat("removed firms with small value added per worker or little revenue\n")
cat("firms in the business sector only\n")
cat("firm-year included only if the firm employs at least two workers in that year\n")
cat("deflated nominal values\n")





log_close()
