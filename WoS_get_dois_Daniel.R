library(data.table)
library(rcrossref)
library(dplyr)


source("get_dois_function.R")
load("store.Rdata")

# Split into batches
chunk_size <- 50
issn_vector <- store$issn
store_batches <- split(issn_vector, ceiling(seq_along(issn_vector)/chunk_size))

# Collect
store_dois <- list()
for(i in 1:length(store_batches)){
  #i = 1
  temp_dois <- list()
  for(j in 1:length(store_batches[[i]])){
    #j=1  
    # Process
    temp_dois[[j]] <- get_dois(issn = store_batches[[i]][j],from_year = 1900,to_year = 2017)
    print(j)
  }
  # Add to big list
  temp_dois <- temp_dois[sapply(temp_dois,is.data.table)]
  temp_dois <- rbindlist(temp_dois)
  store_dois <- rbindlist(list(store_dois,temp_dois))
  # Print status
  print(i)
  # Export
  saveRDS(object = temp_dois,file = paste0('dois_',i,'.rds'))
  rm(temp_dois)
}
