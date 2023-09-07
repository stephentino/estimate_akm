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


###############################
# functions (general purpose) #
###############################


# load/save data ---------------------------------------------------------------

vload <- function(main_directory, dataname) {
  cat("\n\n")
  filename <- paste0(main_directory,"/public_use/data/",dataname,".rds")
  cat("loading data:\n")
  print(filename)
  loaded_data <- readRDS(file = filename)
  cat("\n\n")
  return(loaded_data)
}

vsave <- function(data, main_directory, dataname) {
  cat("\n\n")
  cat("\nexporting the data to the following path:\n")
  filename <- paste0(main_directory,"/public_use/data/",dataname,".rds")
  print(filename)
  saveRDS(data, file = filename)
  cat("\n\n")
}

# open and close log files ------------------------------------------------


log_open <- function(log_path) {
  
  cat("number of current redirections for output:\n")
  print(sink.number())
  cat("\nconnection currently used for error messages (should be 2):\n")
  print(sink.number(type = "message"))

  
  sink(log_path)
  sink(log_path, type = "message")
  
  cat("\n#---------------------#\n")
  cat("start date/time\n")
  print(date())
  cat("\n#---------------------#\n")
  
}



log_close <- function() {
  
  cat("\n#---------------------#\n")
  cat("end date/time\n")
  print(date())
  cat("\n#---------------------#\n")
  
  
  sink(type = "message")
  sink()
  cat("number of current redirections for output:\n")
  print(sink.number())
  cat("\nconnection currently used for error messages (should be 2):\n")
  print(sink.number(type = "message"))

}




# general summary stats table, group by categorical variable ------------------------------------


get_sum_stats_by_group <- function(main_data, group_var, sample_name, median_vars, other_vars) {
  
  # this function creates a summary statistics table similar to Li et al.'s Table 2
  # it has three arguments: 1. the dataset 
  #                         2. the group_var (to compare summary statistics, e.g. by gender, the group variable should be a dummy equal to 1 for female), 
  #                         3. the "other_vars" (means of these are calculated)
  #                         4. the "median vars" (medians of these are calculated)
  #                         5. name of the sample (e.g. the connected set or full sample) for the output table
  
  # typical "median_vars": employment,real_t4earn
  # typical "other_vars": age,btw_25_29,btw_30_39,btw_40_49,btw_50_59,real_t4earn,quebec,ontario,BC,frac_male,foreign_born,log_productivity,roe_separation

  
  sum_stats <- main_data %>%
    group_by({{ group_var }}) %>%
    summarise_at(vars({{ other_vars }}), .funs = list(mean = mean)) 
  
  medians <- main_data %>%
    group_by({{ group_var }}) %>%
    summarise_at(vars({{ median_vars }}), .funs = list(median = median)) 
  
  n_observations <- main_data %>%
    group_by({{ group_var }}) %>%
    tally() %>%
    rename(n_observations = n) %>%
    mutate(n_observations = n_observations/1E6) %>%
    rename(n_observations_1E6 = n_observations)
  
  worker_data <- main_data %>% 
    select(person_id,{{ group_var }}) %>%
    distinct()
  
  n_workers <- worker_data %>%
    group_by({{ group_var }}) %>%
    tally() %>%
    rename(n_workers = n) %>%
    mutate(n_workers = n_workers/1E6) %>%
    rename(n_workers_1E6 = n_workers)
  
  firm_data <- main_data %>%
    select(firm_id,{{ group_var }}) %>%
    distinct()
  
  n_firms <- firm_data %>%
    group_by({{ group_var }}) %>%
    tally() %>%
    rename(n_firms = n) %>%
    mutate(n_firms = n_firms/1E3) %>%
    rename(n_firms_1E3 = n_firms)
  
  sum_stats <- full_join(x = sum_stats,
                         y = medians)
  
  sum_stats <- round(sum_stats, digits = 2)
  
  
  sum_stats <- full_join(x = sum_stats,
                         y = n_observations)
  
  sum_stats <- full_join(x = sum_stats,
                     y = n_workers)
  
  sum_stats <- full_join(x = sum_stats,
                     y = n_firms)
  
  variables <- colnames(sum_stats)

  sum_stats <- as.data.table(sum_stats)
  
  sum_stats <- transpose(sum_stats)
  
  setnames(sum_stats, 
           "V1", paste0(sample_name,"0"),
           skip_absent=TRUE)
  
  setnames(sum_stats, "V2",
           paste0(sample_name,"1"),
           skip_absent=TRUE)
  
  sum_stats <- cbind(variables, sum_stats)

  sum_stats[, temp_id := .I]

  return(sum_stats)
}



