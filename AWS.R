library(data.table)
library(rcrossref)
library(dplyr)

get_dois2 <- function(issn,from_year,to_year){
  
  # Store
  dois_dt <- list()
  
  # Retrieve
  issn_tick <- 0
  for(year in rev(from_year:to_year)){
    issn_tick <- issn_tick + 1
    # Within-year
    # -------------------
    min <- 0
    status <- 0
    ticker <- 0
    dois_year_dt <- list()
    for(ticker in 1:10){
      temp <- cr_works(filter = c(issn=c(issn),
                                  from_pub_date=paste0((year-1),"-01-01"),
                                  until_pub_date=paste0(year,"-01-01")),
                       offset = min,
                       limit = 1000)
      if(!is.null(temp)){
        temp_doi <- temp$data$DOI
        temp_issn <- temp$data$ISSN
        temp_issued <- temp$data$issued
        if(is.null(temp_doi)){temp_doi <- NA}
        if(is.null(temp_issn)){temp_issn <- NA}
        if(is.null(temp_issued)){temp_issued <- NA}
        dois_year_dt[[ticker]] <- data.table(doi=temp_doi, #temp$data$DOI,
                                             issn=temp_issn, #temp$data$ISSN,
                                             issued=temp_issued) #temp$data$issued)
      }else{
        if(ticker > 1){
          dois_year_dt <- rbindlist(dois_year_dt)
          return(dois_year_dt)
        }else{
          return('No entries found')
        }
      }
      print(ticker)
    }
    # -------------------
    
    # Rbind
    dois_dt[[issn_tick]] <- rbindlist(dois_year_dt)
    # Next year...
    #year <- year - 1
  }
  
  
  # Rbind
  dois_dt <- rbindlist(dois_dt)
  # Return
  return(dois_dt)
}


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
