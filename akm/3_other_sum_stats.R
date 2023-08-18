
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

source("//tor-main/Projects2/Tino_10328/Code/utils/functions_1.R")


log_path <- file("//tor-main/Projects2/Tino_10328/Logs/3_other_sum_stats_20230816.txt", open = "wt")
log_open(log_path)



# load data ---------------------------------------------------------------


filename <- "//tor-main/Projects2/Tino_10328/Data/mee_data_with_spells_20230816.rds"
cat("loading...\n")
print(filename)
main_data <- readRDS(file = filename)


main_data[, roe_separation := as.integer(!is.na(first_month_worked) &
                                         !is.na(first_year_worked) &
                                         !is.na(last_month_worked) &
                                         !is.na(last_year_worked)
                                         )]


main_data <- main_data[, .(casenum2021,
                           year,
                           entid_syn,
                           t4earn = real_t4earn,
                           male,
                           age,
                           married,
                           permanent_resident,
                           temporary_resident,
                           value_added = real_va,
                           total_revenue = real_revenue,
                           prov_of_res,
                           roe_separation,
                           employment)]


# prepare data ------------------------------------------------------------


main_data[, foreign_born := as.integer(permanent_resident ==1 | temporary_resident == 1)]

main_data[,log_productivity := log(value_added/employment)]

# these are the same agre groupings as Li et et al. Table 2
main_data[age >= 25 & age <= 29, age_group := 1]
main_data[age >= 30 & age <= 39, age_group := 2]
main_data[age >= 40 & age <= 49, age_group := 3]
main_data[age >= 50 & age <= 59, age_group := 4]
main_data[,btw_25_29 := as.integer(age_group == 1)]
main_data[,btw_30_39 := as.integer(age_group == 2)]
main_data[,btw_40_49 := as.integer(age_group == 3)]
main_data[,btw_50_59 := as.integer(age_group == 4)]

main_data[,ontario := as.integer(prov_of_res==35)]

main_data[,quebec := as.integer(prov_of_res==24)]

main_data[,BC := as.integer(prov_of_res==59)]


# restrict the sample to firms and workers observed at least twice (required to identify fixed effects)

main_data[,n_ind := .N, by = .(casenum2021)]
mean(main_data$n_ind)
main_data <- main_data[n_ind >=2]

main_data[,n_employee := .N, by = .(entid_syn,year)]
mean(main_data$n_employee)
main_data <- main_data[n_employee >= 2]

main_data[, n_ind := NULL]
main_data[, n_employee := NULL]


# calculate the fraction of men at each firm (after applying the other filters above)

main_data[, frac_male := mean(male), by = .(entid_syn,year)]


# extract the maximal connected set for various subsamples ---------------------------

# first, need to identify "movers", which are used to compute the maximal connected set

setkey(main_data,casenum2021,year)

main_data[, lag_entid_syn := shift(entid_syn, 1, type = "lag")]
main_data[, lag_casenum2021 := shift(casenum2021, 1, type = "lag")]

main_data[, move := as.integer(0)]
main_data[entid_syn != lag_entid_syn & casenum2021 == lag_casenum2021, move := as.integer(1)]
main_data[year == min(year), move := NA]

# second, need to compute the maximal connected set for each subsample and merge the results with main_data

subsamples <- c("roe","mr","immigrant","native", "full_sample")

main_data[, roe := roe_separation]
main_data[, mr := as.integer(roe_separation==0)]
main_data[, immigrant := permanent_resident]
main_data[, native := as.integer(permanent_resident == 0 & temporary_resident == 0)]
main_data[, full_sample := 1]

old_dim <- dim(main_data)

for (i in 1:length(subsamples)) {
  
  subsample <- subsamples[i]

  jdata <- main_data[move==1 & get(subsample)==1, .(j1=lag_entid_syn, j2=entid_syn)]
  
  output <- get_connected_set(jdata, subsample)
  
  main_data <- merge(x = main_data, 
                     y = output, 
                     all.x = TRUE, 
                     all.y = FALSE, 
                     by ="entid_syn")
  
  # firms who do not employ any individuals who move are automatically excluded from the connected set; set these NA's to zero 
  main_data[is.na(get(subsample)), paste0(subsample,"_connected") := as.integer(0)] 

  
}


cat("\n\n\n\n\n\n")
cat("dimension change from adding connected sets:\n")
dim(main_data)/old_dim



#---------------#
# SUMMARY STATS #
#---------------#

# these are the list of variables for the summary statistics tables:

median_varlist <- c("t4earn","employment") 

other_varlist <- c("age",
                   "btw_25_29",
                   "btw_30_39",
                   "btw_40_49",
                   "btw_50_59",
                   "male",
                   "t4earn",
                   "quebec",
                   "ontario",
                   "BC",
                   "frac_male",
                   "foreign_born",
                   "log_productivity",
                   "roe_separation")




# summary stats by roe_separation -----------------------------------------

# will compute summary stats for roe_separation == 0 vs. roe_separation == 1, separately for the connected set vs full sample

sum_stats <- get_sum_stats_by_group(main_data,
                                    roe,
                                    "full_sample",
                                    all_of(median_varlist),
                                    all_of(other_varlist))



connected_sum_stats <- get_sum_stats_by_group(main_data[roe_connected==1],
                                              roe,
                                              "roe_connected_set",
                                              all_of(median_varlist),
                                              all_of(other_varlist))


sum_stats <- merge(x = sum_stats,
                   y = connected_sum_stats,
                   all.x = TRUE,
                   all.y = TRUE,
                   by = c("temp_id","variables"))

setkey(sum_stats, temp_id)

sum_stats[, temp_id := NULL]

fwrite(sum_stats, file = "//tor-main/Projects2/Tino_10328/summary_statistics/3_other_sum_stats_roe_20230816.csv", append = FALSE, row.names = FALSE, col.names = TRUE)

rm(sum_stats)



# sum stats by immigrant status -------------------------------------------

# will compute summary statistics for immigrants vs natives, separately for full sample, connected set, and dual connected set

# note: I exclude temporary residents, who are neither immigrants (i.e., those with permanent residence) nor natives

sum_stats <- get_sum_stats_by_group(main_data[temporary_resident == 0],
                                    immigrant,
                                    "full_sample",
                                    all_of(median_varlist),
                                    all_of(other_varlist))

connected_sum_stats <- get_sum_stats_by_group(main_data[temporary_resident == 0 & full_sample_connected==1],
                                              immigrant,
                                              "connected_set",
                                              all_of(median_varlist),
                                              all_of(other_varlist))
 
dual_sum_stats <- get_sum_stats_by_group(main_data[temporary_resident == 0 & immigrant_connected==1 & native_connected==1],
                                         immigrant,
                                         "dual_connected_set",
                                         all_of(median_varlist),
                                         all_of(other_varlist))


sum_stats <- merge(x = sum_stats,
                   y = connected_sum_stats,
                   all.x = TRUE,
                   all.y = TRUE,
                   by = c("temp_id","variables"))

sum_stats <- merge(x = sum_stats,
                   y = dual_sum_stats,
                   all.x = TRUE,
                   all.y = TRUE,
                   by = c("temp_id","variables"))

setkey(sum_stats, temp_id)

sum_stats[, temp_id := NULL]

fwrite(sum_stats, file = "//tor-main/Projects2/Tino_10328/summary_statistics/3_other_sum_stats_immigrant_20230816.csv", append = FALSE, row.names = FALSE, col.names = TRUE)

log_close()


