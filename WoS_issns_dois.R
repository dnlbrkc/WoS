library(data.table)
library(rcrossref)
library(dplyr)



# STEP 1
# ------------------------------------------------------------------------
store <- list()
os <- 0
for(i in 1:3){ #1000
  meta <- cr_journals(limit = 1000,
                      offset = os,
                      works=FALSE)
  
  store[[i]] <- cr_journals(limit = 1000,
                            offset = os,
                            works=FALSE)$data
  os <- os + 1000
  print(i)
}


#get rid of missing lists
dims <- which(simplify2array(lapply(store, function(x) length(x)))>0)
max.dims <- dims[length(dims)]
store <- lapply(1:max.dims, function(x) store[[x]])

#extract title and issn
store[[1]] <- NULL #this one is empty

store <- lapply(store,function(x) x %>% select(title,issn,total_dois))
store <- lapply(store,function(x) x %>% filter(!is.na(issn)))

#convert to data table
store <- lapply(store,as.data.table)
store <- rbindlist(store)
# ------------------------------------------------------------------------



# STEP 2
# ------------------------------------------------------------------------

# help-function to round-up
roundUp <- function(x,to=10)
{
  to*(x%/%to + as.logical(x%%to))
}

store3 <- list()
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
    if(j==1){
      store2[[j]] <- cr_journals(issn = store$issn[i], 
                                 offset = NULL,
                                 limit = 1000,
                                 select = c('DOI','title'),
                                 works=T,
                                 cursor_max = current_max,
                                 cursor = "*")$data
    }else{
      store2[[j]] <- cr_journals(issn = store$issn[i], 
                                 offset = offset_seq[j], 
                                 limit = 1000,
                                 select = c('DOI','title'),
                                 works=T,
                                 cursor_max = current_max,
                                 cursor = "*")$data
    }
    #print(j)
  }
  
  # Bind & store in "store3"
  store2 <- store2[sapply(store2,is.data.frame)]
  store2 <- lapply(store2,as.data.table)
  store2 <- rbindlist(store2)
  store3[[i]] <- store2
  
  # Print
  print(i)
}
# ------------------------------------------------------------------------