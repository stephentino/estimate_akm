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
