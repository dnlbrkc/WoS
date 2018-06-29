library(data.table)
library(rcrossref)
library(dplyr)

store3 <- list()

source("get_dois_function.R")
load("store.Rdata")

# help-function to round-up
roundUp <- function(x,to=10)
{
  to*(x%/%to + as.logical(x%%to))
}

for(i in 1:length(store$issn)){
  ##  Test
  #i = 1
  
  # Round up: how many articles can we collect for the i'th journal
  current_max <- store$total_dois[i]
  if(current_max==0){
    next
  }else{
    current_max <- roundUp(x = current_max, to = 1000) # round to closest 1000
  }
  
  # Loop until current_max has been reached (by 1000)
  offset_seq <- seq(from=0,to = current_max, by = 1000)
  offset_seq <- offset_seq[-length(offset_seq)]
  store2 <- list()
  for(j in 1:length(offset_seq)){
    
    store2[[j]] <-get_dois(store$issn[i],1900,2017 )
    
  }
  
  # Bind & store in "store3"
  store2 <- store2[sapply(store2,is.data.frame)]
  store2 <- lapply(store2,as.data.table)
  store2 <- rbindlist(store2)
  store3[[i]] <- store2
  
  # Print
  print(i)
}
