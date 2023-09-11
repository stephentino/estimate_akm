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


log_path <- file("//tor-main/Projects2/Tino_10328/Logs/1B_add_spells_20230831.txt", open = "wt")
log_open(log_path)



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

month_data <- unique(month_data[, . (casenum2021,
                                     entid_syn,
                                     spell,
                                     first_month_worked,
                                     first_year_worked,
                                     last_month_worked,
                                     last_year_worked) ])

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


filename <- "//tor-main/Projects2/Tino_10328/Data/mee_data_20230831.rds"
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
                   y = spell_data[, .(casenum2021, 
                                      year, 
                                      entid_syn,
                                      spell,
                                      first_month_worked, 
                                      first_year_worked, 
                                      last_month_worked, 
                                      last_year_worked)],
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



# export data -------------------------------------------------------------


cat("\n\ndata to export:")

glimpse(main_data)

cat("\n\n\nexporting the data to the following path:\n")
file_path_export <- "//tor-main/Projects2/Tino_10328/Data/mee_data_with_spells_20230831.rds"
print(file_path_export)
saveRDS(main_data, file = file_path_export)



log_close()
