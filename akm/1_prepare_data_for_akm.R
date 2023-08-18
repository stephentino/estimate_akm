
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


log_path <- file("//tor-main/Projects2/Tino_10328/Logs/1_prepare_data_for_akm_20230816.txt", open = "wt")
log_open(log_path)


#------------------------------------------------#
# construct matched employer-employee (MEE) data #
#------------------------------------------------#


# load T4ROE data -----------------------------------------------------------

# note: the T4ROE data contains data on *jobs* (individual-firm pairs)
# it must be merged with the T1PMF data which contains info on *individuals* and the NALMF data which contains info on *firms* to construct the MEE data
# before merging, I decrease the size of the data by restricting to "primary jobs" (the job that pays the individual the most in each year)

cat("\n\n\n\n\n\n")
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

cat("\n\n\n\n\n\n")
cat("ROE data before merging...\n")
glimpse(roe_data)



# load t1pmf data -----------------------------------------------------------

# the t1pmf data contains info on *individuals* from their tax returns
# will apply some filters before merging with the t4roe data 


cat("\n\n\n\n\n\n")
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

cat("\n\n\n filtering out individuals in the t1pmf data based on FTE:\n")

pmf_data[( t1_t4earn/(cpi/100) ) >= 18733, .N ]/pmf_data[,.N]
pmf_data <- pmf_data[( t1_t4earn/(cpi/100) ) >= 18733]

pmf_data[, t1_t4earn := NULL]


# merge t1pmf and t4roe datasets -----------------------------------------------------------


cat("\n\n\n\n\n\n")
cat("\nPMF data before merging...")
glimpse(pmf_data)

main_data <- merge(x = pmf_data, 
                   y = roe_data, 
                   by = c("casenum2021","year"), 
                   all.x = FALSE, 
                   all.y = FALSE)

cat("\n\n\n\n\n\n")
cat("\ndimensions of data before/after merge...\n")

print(dim(main_data)/dim(pmf_data))
print(dim(main_data)/dim(roe_data))

glimpse(main_data)

rm(roe_data)
rm(pmf_data)

# adjust the T4ROE earnings for inflation -------------------------------------------

#note: 18733 in 2012 dollars is the full-time equivalent threshold (LDS, 2023)

cat("\n\n\n\n\n\n")
cat("filtering out individuals in the t4roe data based on FTE:\n")

main_data[, real_t4earn :=  t4earn/(cpi/100)]

main_data[ real_t4earn >= 18733, .N ]/main_data[,.N]
main_data <- main_data[ real_t4earn >= 18733]

main_data[, t4earn := NULL]



# more data cleaning ------------------------------------------------------


cat("\n\n\n\n\n\n")
cat("\nremove individuals with missing sex or sex ==3 : \n")

main_data[t1_sex_harmonized >2, .N]/main_data[,.N]
main_data <- main_data[t1_sex_harmonized <= 2]

main_data[,male := as.integer(t1_sex_harmonized==1)]

main_data[,t1_sex_harmonized := NULL]

cat("\n\n\n\n\n\n")
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


cat("\n\n\n\n\n\n")

# import the list of individuals with self emp less than t4 earnings to filter self-employed individuals out
filename <- "//tor-main/Projects2/Tino_10328/Data/stata_export/t1pmf_2001_2019_20230525_EmpGreaterSelfEmpGrossNoFish.txt"
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

cat("\n\n\n\n\n\n")
cat("data after restricting to individuals with emp income > self-emp income...\n")
glimpse(main_data)



# load NALMF data ---------------------------------------------------------


cat("\n\n\n\n\n\n")
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

cat("\n\n\n\n\n\n")
cat("\nkeep incorporated firms only\n")

firm_data[incorporated==1,.N]/firm_data[,.N]
firm_data <- firm_data[incorporated == 1]

cat("\n\n\n\n\n\n")
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

cat("\n\n\n")
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


cat("\n\n\nremove small firms and firms with low value added:")
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
cat("\n\n\n\n")

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

main_data[!is.na(landing_year) & year >= landing_year, permanent_resident := as.integer(1)]
main_data[!is.na(landing_year) & year >= landing_year, temporary_resident := as.integer(0)]

main_data[, landing_year := NULL]
main_data[, landing_month := NULL]



# export data -------------------------------------------------------------


cat("\n\n\n\n\n\n")
cat("data to export:\n")
glimpse(main_data)


cat("\n\n\n\n\n\n")
cat("\nexporting the data to the following path:\n")
mee_file_path_export <- "//tor-main/Projects2/Tino_10328/Data/mee_data_20230816.rds"
print(mee_file_path_export)
saveRDS(main_data, file = mee_file_path_export)

# remove the file to save RAM (will reload it later)
rm(main_data)



#-------------------------------------------------------------#
# add data on roe separations and start/end of each job spell #
#-------------------------------------------------------------#


# issue here is that there are three separate datasets to merge
# (was forced to do things like this because of memory issues with the pc)
# first is data on "short spells", which are unique individual-firm pairs in the full t4roe data
# the data on short spells does not contain info on start/end dates on spells
# the second is data on "long spells", which are individual-firm observations that appear more than once in the data
# the data on long spells already contains start/end dates on spells, but these start/end dates only appear for the *last* year of the individual-firm pair 
# (it is missing otherwise. e.g. if a firm-individual pair appears in 2009, 2010, 2011, only 2011 will have the start/end date of the spell...
# ... 2009 and 2010 will have missing start/end dates, even if the start date is something like October 2009.)
# the goal is to "fill out" the start/end dates on spells so that the start/end dates are available in each year for all individual-firm pairs 


# load data on spells  ---------------------------


# data containing spell ID's for "long spells" (individual-firm pairs that appear more than once in the data)
filename <- "//tor-main/Projects2/Tino_10328/Data/ROE_long_spells_20230801"
cat("\n\n\nloading data:\n")
print(filename)
long_spell_data <- readRDS(file = filename)

glimpse(long_spell_data)

long_spell_data[, spell := as.integer(spell)]


# -------------------------------------------------------------------------



# data containing spell ID's for "short spells"  (individual-firm pairs that appear exactly once)
filename <- "//tor-main/Projects2/Tino_10328/Data/ROE_short_spells_20230801"
cat("\n\n\nloading data:\n")
print(filename)
short_spell_data <- readRDS(file = filename)

glimpse(short_spell_data)

short_spell_data[, spell := as.integer(spell)]


# -------------------------------------------------------------------------



# data with start month/year and end month/year
filename <- "//tor-main/Projects2/Tino_10328/Data/ROE_start_end_dates_20230801"
cat("\n\n\nloading data:\n")
print(filename)
month_data <- readRDS(file = filename)

month_data[, roe_separation := NULL]



#  merge start/end dates into short spell data (long spell data already has these) ---------------

short_spell_data <- merge(x = short_spell_data,
                          y = month_data,
                          all.x = TRUE,
                          all.y = FALSE,
                          by = c("casenum2021","year","entid_syn"))



# data cleaning -----------------------------------------------------------


setkey(short_spell_data, casenum2021,  entid_syn, year, spell)


# rather than use "na.locf" from zoo package to "fill" out missing values of dates for long spells, 
# it is computationally faster to use the merge function in the following way:
# first, merge the spells into the data on start/end dates so that each start/end date is associated with a spell
# then, merge this back into the spell data using the spell variable (as opposed to the year) variable so that all years of a spell have the start/end dates

month_data <- merge(x = month_data,
                    y = long_spell_data,
                    all.x = FALSE,
                    all.y = FALSE,
                    by = c("casenum2021","year","entid_syn"))

# there should be no missing date variables in the merged data, since there are none in month data; I will check anyway

cat("\n\n\n check for missing data:\n")
month_data[is.na(last_year_worked), .N]

cat("\n\n\n")
glimpse(month_data)

# I should not need to use the unique function since each individual-firm pair has start/end dates at most once; I will use it anyway to check for any errors

month_data <- unique(month_data[, . (casenum2021,entid_syn,spell,first_month_worked,first_year_worked,last_month_worked,last_year_worked) ])

cat("\n\n\nafter unique(.) function:")
glimpse(month_data)

long_spell_data <- merge(x =long_spell_data,
                         y = month_data,
                         all.x = TRUE,
                         all.y = FALSE,
                         by = c("casenum2021","entid_syn","spell"))

rm(month_data)

# -----------------------------------------------------------------------

cat("\n count if missing last_year_worked/etc. and not missing roe_separation:\n")

# note: the "roe separation" variable was previously calculated in "3.01_export_job_spells"  
# it is equal to 1 if the job spell has start/end dates available, and zero otherwise
# it was created because there were memory/RAM issues with mapping the start/end dates to the full T4ROE data before any filters were applied
# (now that I have filtered the data, I have reduced the dataset size so I can map the individual/firm pairs who pass the filters to the start/end dates)

# if my code ran correctly, should be the case that all job spells with roe_separation ==1 have been mapped to start/end dates,
# and also that there are no job spells with start/end dates that have roe_separation == 0 

long_spell_data[is.na(last_year_worked) & roe_separation == 1, .N]
long_spell_data[is.na(first_year_worked) & roe_separation == 1, .N]
long_spell_data[is.na(last_month_worked) & roe_separation == 1, .N]
long_spell_data[is.na(first_month_worked) & roe_separation == 1, .N]
long_spell_data[!is.na(last_year_worked) & roe_separation == 0, .N]
long_spell_data[!is.na(first_year_worked) & roe_separation == 0, .N]
long_spell_data[!is.na(last_month_worked) & roe_separation == 0, .N]
long_spell_data[!is.na(first_month_worked) & roe_separation == 0, .N]



# load mee data -----------------------------------------------------------


filename <- mee_file_path_export
cat("\n\n\nloading data:\n")
print(filename)
main_data <- readRDS(file = filename)


glimpse(main_data)


cat("\n\n\n\n\n")


# merge mee data with spell data ------------------------------------------

# will first drop individuals who do not pass main filters from the spell data before merging everything with the main data
# I do this using the merge function in the following way:
# I merge the short spell data with the individual-firm-year's in the main data and dropping ind-firm-year's who are not in both datasets
# I then do the same for the long spell data
# this is done to save RAM, since merging everything at once requires too much memory 
# after this is complete, I can merge everything into the main data to make sure the final dataset also includes all of the important covariates in the main data


short_spell_data <- merge(x = main_data[, .(casenum2021,year,entid_syn)],
                          y = short_spell_data,
                          all.x = FALSE,
                          all.y = FALSE,
                          by = c("casenum2021","year","entid_syn"))

long_spell_data <- merge(x = main_data[, .(casenum2021,year,entid_syn)],
                         y = long_spell_data,
                         all.x = FALSE,
                         all.y = FALSE,
                         by = c("casenum2021","year","entid_syn"))

spell_data <- rbind(short_spell_data,long_spell_data)

rm(long_spell_data,short_spell_data)


main_data <- merge(x = main_data,
                   y = spell_data[, .(casenum2021, year, entid_syn, first_month_worked, first_year_worked, last_month_worked, last_year_worked)],
                   all.x = TRUE,
                   all.y = FALSE,
                   by = c("casenum2021","year","entid_syn"))

cat("\n\n\n\n")

rm(spell_data)

#-------------------------------------------------


cat("\ncheck casenum2021-year uniquly identifies observations:\n")
main_data[, N := .N, by = .(casenum2021,year)]
table(main_data$N)
main_data[, N := NULL]


# summary of data cleaning steps ------------------------------------------


cat("\n\n\n\n")
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
cat("constructed start/end dates for each job spell that is issued an ROE\n")



# export data -------------------------------------------------------------


cat("\n\ndata to export:")

glimpse(main_data)

cat("\n\n\nexporting the data to the following path:\n")
file_path_export <- "//tor-main/Projects2/Tino_10328/Data/mee_data_with_spells_20230816.rds"
print(file_path_export)
saveRDS(main_data, file = file_path_export)



log_close()
