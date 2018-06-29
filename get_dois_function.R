get_dois <- function(issn,from_year,to_year){
  
  # Store
  dois_dt <- list()
  
  dates <- paste0(rep(c(from_year:to_year),each=12),c('-01-01','-02-01','-03-01','-04-01','-05-01','-06-01','-07-01','-08-01','-09-01','-10-01','-11-01','-12-01'))
  dates2 <- paste0(rep(c(from_year:to_year),each=12),c('-01-01','-02-01','-03-01','-04-01','-05-01','-06-01','-07-01','-08-01','-09-01','-10-01','-11-01','-12-01'))
  
  dates <- dates[-length(dates)]
  dates2 <- dates2[-1]
  
  
  date_dt <- data.table(date_from=dates,date_to=dates2)
  date_dt <- date_dt[rev(1:nrow(date_dt)),]
  
  
  #date_dt <- apply(date_dt,2,as.character)
  # Retrieve
  issn_tick <- 0
  
  for(year in 1:nrow(date_dt)){
    issn_tick <- issn_tick + 1
    # Within-year
    # -------------------
    min <- 0
    status <- 0
    ticker <- 0
    
    dois_year_dt <- list()
    while(status==0){
      ticker <- ticker + 1
      temp <- cr_works(filter = c(issn=c(issn),
                                  from_pub_date=as.character(date_dt[year,1]),
                                  until_pub_date=as.character(date_dt[year,2]) ),
                       offset = min,
                       limit = 1000)
      
      if(!is.null(temp) && temp$meta$total_results != 0){
        temp_doi <- temp$data$DOI
        temp_issn <- temp$data$ISSN
        temp_issued <- temp$data$issued
        if(is.null(temp_doi)){temp_doi <- NA}
        if(is.null(temp_issn)){temp_issn <- NA}
        if(is.null(temp_issued)){temp_issued <- NA}
        dois_year_dt[[ticker]] <- data.table(doi=temp_doi, #temp$data$DOI,
                                             issn=temp_issn, #temp$data$ISSN,
                                             issued=temp_issued) #temp$data$issued)
      }
      if(nrow(temp$data)<1000){
        status <- 1
      }
      min <- min + 1000
      print(ticker)
    }
    # -------------------
    
    # Rbind
    dois_dt[[issn_tick]] <- rbindlist(dois_year_dt)
    # Next year...
    #year <- year - 1
    #print(year)
  }
  
  
  # Rbind
  dois_dt <- rbindlist(dois_dt)
  # Return
  return(dois_dt)
}