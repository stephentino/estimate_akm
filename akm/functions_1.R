#############
# functions #
#############



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




# ------------------------------------------------------- #
# important functions that will NOT be uploaded to github #
# ------------------------------------------------------- #



# summary statistics table similar to li et al.  --------------------------


get_li_sum_stats <- function(main_data) {
  
  sum_stats <- main_data %>%
    group_by(female) %>%
    summarise_at(vars(age,btw_25_29,btw_30_39,btw_40_49,btw_50_54,real_t4earn,quebec,ontario,BC,frac_male,foreign_born,log_productivity), .funs = list(mean = mean)) 
  
  median_employment <- main_data %>%
    group_by(female) %>%
    summarise_at(vars(employment), .funs = list(median = median)) %>%
    rename(employment_median = median)
  
  n_observations <- main_data %>%
    group_by(female) %>%
    tally() %>%
    rename(n_observations = n) %>%
    mutate(n_observations = n_observations/1E6) %>%
    rename(n_observations_1E6 = n_observations)
  
  worker_data <- unique(main_data[,.(casenum2021,female)])
  
  n_workers <- worker_data %>%
    group_by(female) %>%
    tally() %>%
    rename(n_workers = n) %>%
    mutate(n_workers = n_workers/1E6) %>%
    rename(n_workers_1E6 = n_workers)
  
  firm_data <- unique(main_data[,.(entid_syn,female)])
  
  n_firms <- firm_data %>%
    group_by(female) %>%
    tally() %>%
    rename(n_firms = n) %>%
    mutate(n_firms = n_firms/1E3) %>%
    rename(n_firms_1E3 = n_firms)
  
  sum_stats <- merge(x = sum_stats,
                     y = median_employment,
                     by = "female")
  
  sum_stats <- round(sum_stats, digits = 3)
  
  
  sum_stats <- merge(x = sum_stats,
                     y = n_observations,
                     by = "female")
  
  sum_stats <- merge(x = sum_stats,
                     y = n_workers,
                     by = "female")
  
  sum_stats <- merge(x = sum_stats,
                     y = n_firms,
                     by = "female")
  
  column_names <- colnames(sum_stats)
  
  sum_stats <- transpose(sum_stats)
  
  sum_stats <- cbind(column_names, sum_stats)
  
  sum_stats <- as.data.table(sum_stats)
  
  return(sum_stats)
}





# old functions  ------------------------------------------


# check data before and after applying a filter
check.filter <- function(dt0,dt1) {
  
  dim.before <- dim(dt0)
  dim.after <- dim(dt1)
  
  cat("dim before")
  print(dim.before)
  
  cat("dim after")
  print(dim.after)
  
  cat("dim change")
  print(dim.after/dim.before)
}


# inspect two datasets before merging them
inspect.x.y <- function(dt.x,dt.y) {
  # inspect data before merge
  cat("\n colnames of x")
  print(colnames(dt.x))
  cat("\n dimensions of x \n")
  print(dim(dt.x))
  
  cat("\n colnames of y \n")
  print(colnames(dt.y))
  cat("\n dimensions of y \n")
  print(dim(dt.y))
}





# inspect a dataset
inspect.x <- function(dt.x) {
  
  cat("\n dimensions of data: \n")
  print(dim(dt.x))
  
  cat("\n number of rows of data (in millions): \n ")
  print(nrow(dt.x)/1E6)
  
  cat("\n colnames of x: \n")
  print(colnames(dt.x))
  
  
}

merge.x.y <- function(dt.x, dt.y, byvec) {
  
  
  dt.x[, in.x := 1]
  dt.y[, in.y := 1]
  
  # merge demographic data with T4ROE data
  dt <- merge(x = dt.x, y = dt.y, all.x = TRUE, all.y = TRUE, by = byvec)
  
  # inspect data after merge
  cat("colnames and dimensions for merged data")
  print(dim(dt))
  print(colnames(dt))
  
  # keep track of which observations merged
  dt[(in.x == 1 & in.y == 1), merge := 3]
  dt[(is.na(in.y) & in.x == 1), merge := 1]
  dt[(in.y == 1 & is.na(in.x)), merge := 2]
  
  dt.x[, in.x := NULL]
  dt.y[, in.y := NULL]
  return(dt)
  
}



# verify merge worked well
check.merge <- function(dt) {
  cat("\n summarize merge \n")
  cat("\n[3 = in both, 2= in y only, 1= in x only]\n")
  tab.dt <- table(dt$merge)
  
  cat("\n in levels \n")
  print(tab.dt)
  
  cat("\n in percent \n")
  print(tab.dt/sum(tab.dt))
  
}


check.missing <- function(x) {
  
  z <- is.na(x)
  nmiss <- sum(z)
  pmiss <- mean(z)
  
  cat("\nnumber missing:\n")
  print(nmiss)
  
  cat("\nfraction missing\n")
  print(pmiss)
  
  
  
}


percent.dummy <- function(x) {
  cat("\npercent of observations with diff strictly less than -1:\n")
  print(paste0(round(100*mean(x), digits = 4),"%"))
}


#
#
#



