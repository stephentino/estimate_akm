# Author: Stephen Tino


# load/save data ----------------------------------------------------------


vload <- function(main_directory, dataname) {
  
  filename<- paste0(main_directory,"/Data/",dataname,".rds")
  
  ncat(paste0("loading data: ",filename))
  
  loaded_data <- readRDS(file = filename)
  
  return(loaded_data)
  
}

vsave <- function(data_to_save, main_directory, dataname, date) {
  
  filename <- paste0(main_directory,"/Data/",dataname,"_",date,".rds")
  
  ncat(paste0("saving data: ",filename))
  
  saveRDS(data_to_save, file = filename)
  
}