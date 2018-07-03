library(data.table)
library(rcrossref)
library(dplyr)


source("get_dois_function.R")
load("store.Rdata")


store_dois <- list()

for(i in 1:length(store$issn)){

  store_dois[[i]] <-  get_dois(store$issn[i],'1900','2017')
  
  # Print
  print(i)
}
