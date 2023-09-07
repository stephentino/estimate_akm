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

options(crayon.enabled = FALSE)

library("data.table")
library("tidyr")
library("dplyr")

main_directory <- "/Users/Stephen/Dropbox/40. github/estimate_akm"

source(paste0(main_directory,"/public_use/code/utils/akm_functions.R"))
source(paste0(main_directory,"/public_use/code/utils/functions_1.R"))
       
# -------------------------------------------------------------------------

log_path <- file(paste0(main_directory,"/public_use/logs/1_simulate_data.txt"), open = "wt")
log_open(log_path)


p_move <- 0.25 # probability of a worker switching firms from period t to t+1

set.seed(1992)

n_jobs <- 1E5

job_data <- data.table(person_id = 1:n_jobs,
                       birth_year = floor( runif(n_jobs, min = 1960, max = 1980) ),
                       firm_id1 = floor( abs( rnorm( n_jobs) )*1E2 ))

for(i in 2:19) {
  job_data[, paste0("move",i) := as.integer( runif(n_jobs) < p_move ) ]
  job_data[get(paste0("move",i))==1, paste0("firm_id",i) := floor( abs( rnorm( .N ) )*1E2 ) ]
  job_data[get(paste0("move",i))==0, paste0("firm_id",i) := get(paste0("firm_id",i-1)) ]
}

job_data <- job_data %>%
  select(-starts_with("move"))

job_data <- job_data %>%
  pivot_longer(cols = starts_with("firm_id"),
               names_to = "year",
               values_to = "firm_id",
               names_prefix = "firm_id") %>%
  mutate(year = as.numeric(year)+2000)

n_firms <- max(job_data$firm_id)

# will now create earnings variables using an akm structure (person and firm effects will be recovered later)

job_data <- job_data %>%
  select(person_id) %>%
  distinct() %>%
  mutate(person_effect = rnorm(n = n())*10) %>%
  full_join(y = job_data, multiple= "all")

job_data <- job_data %>%
  select(firm_id) %>%
  distinct() %>%
  mutate(firm_effect = rnorm(n = n())*5) %>%
  full_join(y = job_data, multiple = "all")

job_data <- job_data %>%
  select(year) %>%
  distinct() %>%
  mutate(year_effect = rnorm(n = n())*2) %>%
  full_join(y = job_data, multiple = "all")

job_data <- job_data %>%
  mutate(age = year - birth_year)

fe_data <- job_data %>%
  select(person_id, firm_id, year, person_effect, firm_effect, year_effect)

job_data <- job_data %>%
  mutate(log_earnings = firm_effect + person_effect + year_effect + 0.001*age - 0.002*age^2 + rnorm( n() ) ) %>%
  select(-ends_with("effect")) %>%
  as.data.table()

cat("\njob data for export:\n")

setkey(job_data, person_id, year)

glimpse(job_data)

saveRDS(job_data, file = paste0(main_directory, "/public_use/data/job_data.rds"))

saveRDS(fe_data, file = paste0(main_directory, "/public_use/data/fe_data.rds"))


log_close()