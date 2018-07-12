library(data.table)
library(rcrossref)
library(dplyr)
library(saveRDS)

source("WoS_functions.R")
load("store.Rdata")

# Split into batches
issn_vector <- store$issn
n_cores <- 6
chunk_size <- length(issn_vector)/n_cores
store_batches <- split(issn_vector, ceiling(seq_along(issn_vector)/chunk_size))

#change this for each machine
machine_id <- 1

# Collect
store_dois <- list()
#for(i in machine_id){
  i = machine_id
  temp_dois <- list()
  for(j in 1:length(store_batches[[i]])){
    #j=1  
    # Process
    temp_dois[[j]] <- get_dois2(issn = store_batches[[i]][j],from_year = 1900,to_year = 2017)
    print(j)
    save(temp_dois,file=paste0('res_',j,'.Rdata'))
  }
  # Add to big list
 # temp_dois <- temp_dois[sapply(temp_dois,is.data.table)]
 # temp_dois <- rbindlist(temp_dois)
 # store_dois <- rbindlist(list(store_dois,temp_dois))
  # Print status
 # print(i)
  # Export
  #saveRDS(object = temp_dois,file = paste0('dois_',i,'.rds'))
 # rm(temp_dois)
#}
