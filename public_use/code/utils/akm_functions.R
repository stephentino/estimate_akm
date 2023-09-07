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



##############################
# functions (akm estimation) #
##############################


# prepare data for akm ----------------------------------------------------

get_akm_data <- function(data_input, age_normalization = FALSE) {
  
  data <- copy(data_input)
  rm(data_input)
  
  # restrict to workers/firms observed at least twice to identify FEs -----------------------
  
  data[, n_worker := .N, by = person_id ]
  data[, n_firm := .N, by = firm_id ]
  
  data <- data[n_worker >= 2][, n_worker := NULL]
  data <- data[n_firm >= 2][, n_firm := NULL]
  
  
  #  extract list of movers to compute the connected set ---------------------------------------
  
  setkey(data,person_id,year)
  
  data[, lag_firm_id := shift(firm_id, 1, type = "lag")]
  data[, lag_person_id := shift(person_id, 1, type = "lag")]
  
  data[, move := as.integer(0)]
  data[firm_id != lag_firm_id & person_id == lag_person_id, move := as.integer(1)]
  data[year == min(year), move := NA]
  
  
  # extract connected set ---------------------------------------------------
  
  # note: the lfe package automatically does this,
  # but I do this before running felm from the lfe package to reduce the computational burden when estimating the model)
  
  jdata <- data[move==1, .(j1=lag_firm_id, j2=firm_id)]
  
  output <- get_connected_set(jdata)
  
  data <- merge(x = data, 
                y = output, 
                all.x = TRUE, # keep all firms 
                all.y = FALSE, 
                by ="firm_id")
  
  cat("\n\nany firms missing connected status do not employ movers. fraction of the sample:\n")
  data[is.na(connected), .N]/data[, .N]
  
  data[is.na(connected), connected := 0]
  
  cat("\nfraction of worker-firms in the connected set:\n")
  print(mean(data$connected, na.rm = TRUE))
  
  
  # more data prep ----------------------------------------------------------
  
  data[, person_id := as.factor(person_id)]
  
  data[, firm_id := as.factor(firm_id)]
  
  data[, year := as.factor(year)]

  data <- data[connected==1]
  
  if(age_normalization == TRUE) {
    # normalize age  ----------------------------------------------------------
    
    # normalization of age before running akm follows dostie, card, li, and parent 
    # I do this by: first, residualizing the wage using time-varying covariates
    # next, determining which age has the largest residualized log earnings 
    # finally, normalizing age subtracting and dividing by it  
    
    est <- felm(log_earnings ~ year, data = data)
    
    cat("\n\n\nfinished estimating linear model for age normalization.\n")
    
    data$resid <- est$residuals
    
    rm(est)
    
    data[, avg_resid := mean(resid, na.rm = TRUE), by = age]
    
    age_data <- unique(data[, .(age, avg_resid)])
    
    age_data[, max_resid := max(avg_resid, na.rm = TRUE)]
    
    cat("\nage of maximum residuals:\n")
    normalization_factor <- age_data[which(age_data$max_resid==age_data$avg_resid), age]
    print(normalization_factor)
    
    data[, norm_age := (age-normalization_factor)/normalization_factor] 
    
    data[, norm_age_sq := (norm_age^2)/100]
    
    data[, norm_age_cubed := (norm_age^3)/1000]
    
    data[, norm_age_quart := (norm_age^4)/10000]
    
    rm(age_data)
    data[, resid := NULL]
    data[, avg_resid := NULL]
  }
  
  
  rm(jdata)
  rm(output)

  
  return(data)
}


# extract connected set of firms ------------------------------------------

get_connected_set <- function(jdata, prefix = NULL) {
  
  # this function takes a list of employers of movers and extracts the connected set of these employers
  # individuals are defined as "movers" if in period t they have a different employer than in period t-1
  # the input data, "jdata", should be a data.table with two columns:
  # the first column is the firm_id in period t-1
  # the second column is the firm_id in period t
  # the "prefix" is used to name the column that indicates whether firms are in the connected set or not
  
  # -------------------------------
  
  # EXAMPLE CODE to create jdata from the matched employee-employer (mee) data:
  
  # setkey(main_data, person_id, year)
  
  # main_data[, lag_firm_id := shift(firm_id,1,type = "lag")]
  # main_data[, lag_person_id := shift(person_id,1,type = "lag")]
  
  # main_data[, move := as.integer(0)]
  # main_data[firm_id != lag_firm_id & person_id == lag_person_id, move := as.integer(1)]
  # main_data[year == min(year), move := NA]
  
  # jdata <- main_data[move==1,.(j1=lag_firm_id,j2=firm_id)]
  
  
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
  
  list_of_connected_firms <- firm_ids[,.(firm_id = j1, connected)]
  
  if(length(prefix) > 0) {
    
    setnames(list_of_connected_firms,
             "connected",
             column_name <- paste0(prefix,"_connected"),
             skip_absent = TRUE)
    
  }
  
  # export results
  
  return(list_of_connected_firms)
}


# prepare data for variance decomp ----------------------------------------

get_decomp_data <- function(main_data, fe) {
  
  
  # this function prepares the data for the variance decomposition by merging the FE into the mee data with earnings
  # note: both "main_data" and "fe" must be of class "data.table" for this function to work!
  
  cat("\nFE data:\n")
  glimpse(fe)
  
  main_data <- merge(x = main_data,
                     y = fe[fe=="person_id",
                            .(worker_fe = effect, person_id = as.character(idx))],
                     all.x = TRUE,
                     all.y = TRUE,
                     by = "person_id")
  
  main_data <- merge(x = main_data,
                     y = fe[fe=="firm_id",
                            .(firm_fe = effect, firm_id = as.character(idx))],
                     all.x = TRUE,
                     all.y = TRUE,
                     by = "firm_id")
  
  cat("\n how many observations not missing worker or firm FEs:")
  print(main_data[!is.na(worker_fe) & !is.na(firm_fe), .N]/main_data[, .N])
  
  main_data <- main_data[!is.na(worker_fe) & !is.na(firm_fe)]
  
  cat("\ndata with earnings, fixed effects, and other covariates:\n")
  glimpse(main_data)
  
  return(main_data)
  
}



# variance decomposition --------------------------------------------------

var_decomp <- function(y, worker_fe, firm_fe) {
  
  statistics <- c("var(y)",
                  "var(worker_fe)",
                  "var(firm_fe)",
                  "2*cov(worker_fe,firm_fe)",
                  "share_worker",
                  "share_firm",
                  "share_2cov")
  
  values <- c(var(y),
              var(worker_fe),
              var(firm_fe),
              2*cov(worker_fe,firm_fe),
              var(worker_fe)/var(y),
              var(firm_fe)/var(y),
              ( 2*cov(worker_fe,firm_fe) )/var(y)
  )
  
  results <- data.table(statistic = statistics, 
                        value = values)
  
  return(results)
  
}


