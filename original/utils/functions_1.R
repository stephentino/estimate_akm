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

# modified cat and print functions for making comments in log files -------



ncat <- function(...) {
  cat(paste0("\n\n",...,"\n\n"))
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



# compute monthly earnings ------------------------------------------------


get_monthly_earnings <- function(data_input) {
  
  data <- copy(data_input)
  rm(data_input)
  
  data[, roe_separation := as.integer(
    !is.na(last_year_worked) & 
      !is.na(last_month_worked) & 
      !is.na(first_year_worked) & 
      !is.na(first_month_worked) )]
  
  data[, monthly_earnings := real_t4earn/12]
  data[year == first_year_worked, monthly_earnings := real_t4earn/(13-first_month_worked)]
  data[year == last_year_worked, monthly_earnings := real_t4earn/last_month_worked]
  data[roe_separation == 0, monthly_earnings := NA]
  
  return(data)
  
}



# add variables for advantaged vs rest-of-world immigrants ----------------


add_birthplace <- function(data_input) {
  
  
  data <- copy(data_input)
  rm(data_input)
  
  cat("loading imdb data...\n")
  imdb_data <- fread(file = "//tor-main/Projects2/Tino_10328/Data/stata_export/imdb_20230425.txt")
  
  
  # extract subset of advantaged immigrants  --------------------------------
  
  adv_country_codes <- c(11124, #Canada
                         11840, #USA 
                         21040, #Austria
                         21056, #Belgium
                         21250, #France
                         21276, #Germany
                         21438, #Liechtenstein
                         21442, #Luxembourg
                         21492, #Monaco
                         21528, #Netherlands
                         21756, #Switzerland
                         23208, #Denmark
                         23246, #Finland
                         23372, #Ireland
                         23578, #Norway
                         23752, #Sweden
                         23826, #UK
                         51036, #Australia
                         51554) #New Zealand
  
  imdb_data[ country_birth %in% adv_country_codes,
             adv_country := 1]
  
  cat("\nshare of individuals from adv countries in IMDB data:\n")
  print(imdb_data[ adv_country==1, .N]/imdb_data[, .N])
  
  
  imdb_data <- imdb_data[adv_country == 1, .(casenum2021,adv_country)]
  
  
  data <- merge(x = data,
                y = imdb_data,
                all.x = TRUE,
                all.y = FALSE,
                by = "casenum2021")
  
  data[is.na(adv_country), adv_country := 0]
 
  data[, foreign_born := as.integer(permanent_resident ==1 | temporary_resident == 1)]
  
  data[foreign_born==1 & adv_country==1, type := "adv"]
  data[foreign_born==1 & adv_country==0, type := "other"]
  data[foreign_born==0, type := "native"]
  
  data[, type := as.factor(type)]
  
  return(data)
  
  
}


# extract connected set of firms ------------------------------------------


get_connected_set <- function(jdata, prefix = NULL) {
  
  # this function takes a list of employers of movers and extracts the connected set of these employers
  # individuals are defined as "movers" if in period t they have a different employer than in period t-1
  # the input data, "jdata", should be a data.table with two columns:
  # the first column is the entid_syn in period t-1
  # the second column is the entid_syn in period t
  # the "prefix" is used to name the column that indicates whether firms are in the connected set or not
  
  # -------------------------------
  
  # EXAMPLE CODE to create jdata from the matched employee-employer (mee) data:
  
  # setkey(main_data, casenum2021, year)
  
  # main_data[, lag_entid_syn := shift(entid_syn,1,type = "lag")]
  # main_data[, lag_casenum2021 := shift(casenum2021,1,type = "lag")]
  
  # main_data[, move := as.integer(0)]
  # main_data[entid_syn != lag_entid_syn & casenum2021 == lag_casenum2021, move := as.integer(1)]
  # main_data[year == min(year), move := NA]
  
  # jdata <- main_data[move==1,.(j1=lag_entid_syn,j2=entid_syn)]
  
  
  # -------------------------
  
  
  # the graph is undirected, so I include an edge for j2->j1 if j1->j2, following BLM
  jdata <- rbind(jdata, jdata[,.(j1=j2,j2=j1)])
  
  # I need to create numeric firm ID's for the graph(.) function, since the current firm ID's are string variables
  # note: if J is the number of firms, these ID's should run from 1 to J 
  
  firm_ids <- unique(jdata[,.(j1)])
  
  setkey(firm_ids, j1)
  
  firm_ids[,fid1 := .I]
  
  jdata <- merge(x = jdata, 
                 y = firm_ids, 
                 by = c("j1"))
  
  firm_ids2 <- firm_ids[,.(fid2 = fid1, j2=j1)]
  
  jdata <- merge(x = jdata, 
                 y = firm_ids2, 
                 by = c("j2"))
  
  # jdata may have duplicate rows if two individuals move between the same firms
  # remove these to create a unique list of edges in the graph
  
  jdata <- unique(jdata)
  
  setkey(jdata,fid1,fid2)
  
  # -----------------------------------------------------------
  
  # find maximum connected subgraph 
  
  J <- max(jdata$fid1,jdata$fid2) # number of firms
  
  edges <- unique(jdata[,.(fid1,fid2)]) 
  
  g <-graph(edges = t(as.matrix(edges)), n = J, directed = FALSE)
  
  cs <- components(g) # "components" of the graph include connected subgraphs and sizes (i.e. number of firms)
  
  largest_conset <- which(cs$csize==max(cs$csize)) # this is the maximal connected subgraph
  
  cset <- which(cs$membership==largest_conset) # identifies the firm ID's in the largest connected subgraph
  
  firm_ids$connected <- as.integer(firm_ids$fid1 %in% cset)
  
  # -----------------------------------------------------
  
  # prepare results to export 
  
  list_of_connected_firms <- firm_ids[,.(entid_syn = j1, connected)]
  
  if(length(prefix) > 0) {
    
    setnames(list_of_connected_firms,
             "connected",
             column_name <- paste0(prefix,"_connected"),
             skip_absent = TRUE)
    
  }
  
  # export results
  
  return(list_of_connected_firms)
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
    select(casenum2021,{{ group_var }}) %>%
    distinct()
  
  n_workers <- worker_data %>%
    group_by({{ group_var }}) %>%
    tally() %>%
    rename(n_workers = n) %>%
    mutate(n_workers = n_workers/1E6) %>%
    rename(n_workers_1E6 = n_workers)
  
  firm_data <- main_data %>%
    select(entid_syn,{{ group_var }}) %>%
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




# calculate observed hhi (at market level) -----------------------------------------------------------


get_hhi <- function(data_input, naics_digits = 2) {
  
  data <- copy(data_input)
  rm(data_input)


  
  data[is.na(sac_syn), sac_syn := -999]
  data[, naics_ndigit := floor( ( naics_scan*(10^naics_digits) )/1E4 ) ]
  data[, worker := 1]
  
  data[, emp_observed := sum(worker, na.rm = TRUE), by = .(entid_syn, sac_syn, naics_ndigit, year)]
  
  firm_data <- unique(data[, .(entid_syn, 
                               sac_syn, 
                               year, 
                               naics_ndigit, 
                               employment = emp_observed)])
  
  firm_data[, market_employment := sum(employment, na.rm = TRUE), by = .(sac_syn, naics_ndigit, year)]
  
  firm_data[, industry_employment := sum(employment, na.rm = TRUE), by = .(naics_ndigit, year)]
  
  firm_data[, market_share := employment/market_employment]
  
  firm_data[, industry_share := employment/industry_employment]
  
  firm_data[, share_sq := market_share^2]
  
  firm_data[, hhi := sum(share_sq, na.rm = TRUE), by = .(sac_syn, naics_ndigit, year)]
  
  firm_data[, hhi := hhi*1E4]
  
  market_data <- unique(firm_data[, .(sac_syn, naics_ndigit, year, hhi)])
  
  data <- merge(x = data,
                y = market_data,
                all.x = TRUE,
                all.y = TRUE,
                by = c("sac_syn","naics_ndigit", "year"))
  
  firm_data <- unique(firm_data[, .(entid_syn, 
                                    sac_syn, 
                                    naics_ndigit, 
                                    year, 
                                    market_share = market_share*100, 
                                    industry_share = industry_share*100)])
  
  data <- merge(x = data,
                y = firm_data,
                all.x = TRUE,
                all.y = TRUE,
                by = c("entid_syn","sac_syn","naics_ndigit","year"))
  
  rm(firm_data)
  rm(market_data)
  
  data[, naics_ndigit := NULL]
  data[, worker := NULL]
  data[, emp_observed := NULL]
  
  return(data)
  
}




# identify which individuals in the mee data (with spells) have consecutive ROEs --------

get_croe <- function(data_input) {
  
  data <- copy(data_input)
  rm(data_input)
  
  # additional filters (compared to the baseline full sample)
  data[, roe_separation := as.integer(!is.na(last_year_worked) & !is.na(last_month_worked) & !is.na(first_year_worked) & !is.na(first_month_worked) )]
  setkey(data, casenum2021, year)
  data[, l1_roe_separation := shift(roe_separation, type = "lag", n=1)]
  data[, l1_casenum2021 := shift(casenum2021, type = "lag", n=1)]
  data[is.na(l1_roe_separation), l1_roe_separation := 0]
  
  data[, croe_separation := as.integer( l1_roe_separation == 1 & roe_separation == 1 & casenum2021 == l1_casenum2021 )]
  
  cat("\nfraction of sample with consecutive ROEs:")
  print(data[croe_separation==1, .N]/data[, .N])
  
  data[, l1_casenum2021 := NULL]
  data[, l1_roe_separation := NULL]
  
  return(data)
  
}

#
#
#
