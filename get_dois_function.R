get_dois <- function(issn,from_year,to_year){
  
  # Test
  #issn = store_batches[[i]][j]
  #from_year = 1900
  #to_year = 2017
  
  # Store
  dois_dt <- list()
  
  #loop by months
  dates <- paste0(rep(c(from_year:to_year),each=12),c('-01-01','-02-01','-03-01','-04-01','-05-01','-06-01','-07-01','-08-01','-09-01','-10-01','-11-01','-12-01'))
  dates2 <- paste0(rep(c(from_year:to_year),each=12),c('-01-01','-02-01','-03-01','-04-01','-05-01','-06-01','-07-01','-08-01','-09-01','-10-01','-11-01','-12-01'))
  dates <- dates[-length(dates)]
  dates2 <- dates2[-1]
  
  date_dt_month <- data.table(date_from=dates,date_to=dates2)
  date_dt_month <- date_dt_month[rev(1:nrow(date_dt_month)),]
  
  #loop by year
  dates <- paste0(rep(c(from_year:to_year),each=1),c('-01-01'))
  dates2 <- paste0(rep(c(from_year:to_year),each=1),c('-01-01'))
  dates <- dates[-length(dates)]
  dates2 <- dates2[-1]
  
  date_dt_year <- data.table(date_from=dates,date_to=dates2)
  date_dt_year <- date_dt_year[rev(1:nrow(date_dt_year)),]
  
  #date_dt <- apply(date_dt,2,as.character)
  # Retrieve

  
  
  
  issn_tick <- 0
  
  for(year in 1:nrow(date_dt_year)){ #loop through years by default
    
    #check if there are more than 10k entries
    check_error <- cr_works(filter = c(issn=c(issn),
                                       from_pub_date=as.character(date_dt_year[year,1]),
                                       until_pub_date=as.character(date_dt_year[year,2]) ),
                            offset = 9000,
                            limit = 1000)
    
    #if there are fewer than 10k entries we loop by year otherwise by month
    if(nrow(check_error$data ) != 1000){
      date_dt <- date_dt_year
      cond = 0
    } else { #divide year into months
      months <- date_dt_month %>% 
        filter(date_from >=  as.character(date_dt_year[year,1]) &
                 date_from <=  as.character(date_dt_year[year,2]))
      date_dt <- months
      cond = 1
    }
    
    
    if(cond == 0){ #collect for 1 year
      
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
        #print(ticker)
      }
      # -------------------
      # Rbind
      dois_dt[[issn_tick]] <- rbindlist(dois_year_dt)
      
    } else { #divide year into months
      
      for(year in 1:nrow(date_dt) ){
        issn_tick <- issn_tick + 1
        
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
          #print(ticker)
        }
        # -------------------
        # Rbind
        dois_dt[[issn_tick]] <- rbindlist(dois_year_dt)
      }
    }
  }
  
  
  # Rbind
  dois_dt <- rbindlist(dois_dt)
  # Return
  return(dois_dt)
}