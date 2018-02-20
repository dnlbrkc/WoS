# ------------------------------------------
#
# SoS project
# * Data collection
# > Functions
#
# ------------------------------------------



# (0) Load needed libraries
# ------------------------------------------
library(rvest)
library(stringi)
library(data.table)
library(rcrossref)
library(rscopus)
library(devtools)
library(parallel)

# Appr.1
#devtools::install_github("ropensci/alm")
#library(alm)

# Appr.2
#install_github("ropensci/rentrez")
library(rentrez)
# ------------------------------------------



# (1) Scrape issn's
# ------------------------------------------
get_issn <- function(page){
  issn <- read_html(page) %>% html_text()
  issn <- stringi::stri_split(str = issn, regex = "ISSN:")
  issn <- sapply(issn,function(x)gsub(substr(x,1,10),pattern = " ",replacement = ""))
  issn <- issn[sapply(issn,function(x)stri_detect(str = x,regex = "-"))]
  return(issn)
}
# ------------------------------------------



# Get pubmed id (Appr.2)
# ------------------------------------------
get_pubmedid <- function(doi){
  pubmedid <- entrez_search(db = "pubmed", term = paste0(doi,"[doi]"))$id
  if(is.list(pubmedid)){pubmedid <- NA}
  return(data.table(doi=doi,pubmedid=pubmedid))
}
# ------------------------------------------



# (2) Obtain doi's via crossref 
# ------------------------------------------
get_dois <- function(issn,from_year,to_year){
  
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
    while(status==0){
      ticker <- ticker + 1
      temp <- cr_works(filter = c(issn=c(issn),
                                  from_pub_date=paste0((year-1),"-01-01"),
                                  until_pub_date=paste0(year,"-01-01")),
                       offset = min,
                       limit = 1000)
      if(!is.null(temp)){
        dois_year_dt[[ticker]] <- data.table(doi=temp$data$DOI,
                                             issn=temp$data$ISSN,
                                             issued=temp$data$issued)
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
  }
  
  
  # Rbind
  dois_dt <- rbindlist(dois_dt)
  # Return
  return(dois_dt)
}

# Example 1
# science_dois <- get_dois(issn = '0036-8075',
#                          from_year = 2005, 
#                          to_year = 2006)

# Example 2
# library(rcrossref)
# ex <- cr_works(filter = c(issn=c(issns[100])),limit = 1000)
# dim(ex$data)
# ex$data
# ------------------------------------------



# Get abstract (functions) -- OLD
# -----------------------------------
# get_abstract <- function(doi){
#   abstract <- abstract_retrieval(id = doi,
#                                  identifier = c("doi"), 
#                                  http_end = NULL)
#   content <- abstract$content$`abstracts-retrieval-response`$coredata$`dc:description`
#   if(is.null(content)){
#     return(NA)
#   }else{
#     return(list(paper$content$`abstracts-retrieval-response`$coredata$`dc:description`,
#                 paper$content$`abstracts-retrieval-response`$authkeywords,
#                 paper$content$`abstracts-retrieval-response`$item))
#   }
# }
# -----------------------------------





# //////////////////////////////////////////////////////////////
#                     NEW SET OF FUNCTIONS
#                          2018-02-10
# //////////////////////////////////////////////////////////////





# Get article information
# ------------------------------------------
get_article_info <- function(doi=NULL,
                             pubmed_id=NULL,
                             api_key=NULL){
  
  ## Test
  #doi = science_dois$doi[192]
  #doi = "10.1126/science.aaf8287"
  
  # Set API key
  # * This could be done outside!
  if(!is.null(api_key)){
    set_api_key(api_key)
  }
  
  # Retrieve abstract
  if(!is.null(doi)){
    id <- doi
    abstract <- abstract_retrieval(id = doi,
                                            identifier = c("doi"),
                                            http_end = NULL)
  }else{
    id <- pubmed_id
    abstract <- abstract_retrieval(id = pubmed_id,
                                            identifier = c("pubmed_id"),
                                            http_end = NULL)
  }
  
  # Extract useful information
  basics_dt <- get_basics(abstract)
  authors_dt <- get_authors_dt(abstract,doi)
  reference_dt <- get_reference_dt(abstract,doi)
  abstrct <- get_abstract(abstract = abstract,
                          doi=basics_dt$doi,
                          pubmed_id=basics_dt$pubmed_id)
  
  # Return
  return(list(basics_dt=basics_dt,
              authors_dt=authors_dt,
              reference_dt=reference_dt,
              abstract_dt=abstrct))
}
# ------------------------------------------


# Get author from abstract object
# ------------------------------------------
get_authors_dt <- function(abstract,doi=NULL){
  
  extract_author_info <- function(author){
    aff_id <- author$affiliation[['@id']]
    if(is.null(aff_id)){aff_id <-NA}
    given_name <- author[['ce:given-name']]
    if(is.null(given_name)){given_name<-NA}
    return(data.table(author_id=author[['@auid']],
                      seq=author[['@seq']],
                      given_name=given_name,
                      initials=author[['ce:initials']],
                      surname=author[['ce:surname']],
                      affiliation_id=aff_id))
  }
  
  authors <- abstract$content$`abstracts-retrieval-response`$authors$author
  author_dt <- lapply(authors,extract_author_info)
  author_dt <- rbindlist(author_dt)
  author_dt[,affiliation_id := as.character(affiliation_id)]
  
  # Add affiliation details
  extract_aff_details <- function(affiliation){
    #affiliation <- affiliations[[1]]
    return(data.table(affiliation_id=affiliation[['@id']],
                      affiliation_name=affiliation$affilname,
                      affiliation_country=affiliation$`affiliation-country`))
  }
  affiliations <- abstract$content$`abstracts-retrieval-response`$affiliation
  if(!is.null(affiliations) & !is.list(affiliations[[1]])){affiliations <- list(affiliations)}
  affiliation_dt <- lapply(affiliations,extract_aff_details)
  affiliation_dt <- rbindlist(affiliation_dt)
  if(nrow(affiliation_dt)>0){
    author_dt <- merge(x=author_dt,y=affiliation_dt,by="affiliation_id",all.x=T,all.y=F)
  }else{
    author_dt[,c("affiliation_id","affiliation_name","affiliation_country") := list(NA,NA,NA)]
  }
  # Add doi?
  if(!is.null(doi)){author_dt[,doi:=doi]}
  # Return
  return(author_dt)
}
## Test
#auth_dt <- get_authors_dt(abstract = abstract)
# ------------------------------------------


# Get reference dt
# ------------------------------------------
get_reference_dt <- function(abstract,doi=NULL){
  
  extract_reference_info <- function(reference){
    pubyear <- reference$`ref-info`$`ref-publicationyear`[['@first']]
    if(is.null(pubyear)){pubyear <- NA}
    sourcetitle <- reference$`ref-info`$`ref-sourcetitle`
    if(is.null(sourcetitle)){sourcetitle <- NA}
    return(data.table(id=reference[['@id']],
                      itemid=reference$`ref-info`$`refd-itemidlist`$itemid[['$']],
                      itemid_type=reference$`ref-info`$`refd-itemidlist`$itemid[['@idtype']],
                      pub_year=pubyear,
                      sourcetitle=sourcetitle))
  }
  references <- abstract$content$`abstracts-retrieval-response`$item$bibrecord$tail$bibliography$reference
  if(is.null(references)){return(NULL)}
  reference_dt <- lapply(references,extract_reference_info)
  reference_dt <- rbindlist(reference_dt)
  reference_dt[,seq:=.I]
  setcolorder(reference_dt,c(1,6,4,2,3,5))
  # Add doi?
  if(!is.null(doi)){reference_dt[,doi:=doi]}
  # Return
  return(reference_dt)
}
## Test
#ref_dt <- get_reference_dt(abstract = abstract)
# ------------------------------------------


# Get basics
# ------------------------------------------
get_basics <- function(abstract){
  # Core-data-part
  coredata <- abstract$content$`abstracts-retrieval-response`$coredata
  eid <- coredata$eid;if(is.null(eid)){eid <- NA}
  pubmed_id=coredata$`pubmed-id`;if(is.null(pubmed_id)){pubmed_id <- NA}
  doi <- coredata$`prism:doi`;if(is.null(doi)){doi<-NA}
  # Keywords
  keywords <- abstract$content$`abstracts-retrieval-response`$authkeywords
  if(is.null(keywords)){keywords <- NA}
  # Dicipline
  subject_areas <- abstract$content$`abstracts-retrieval-response`$`subject-areas`
  if(!is.null(subject_areas)){
    subject_areas <- sapply(subject_areas[[1]],function(x)x[[2]])
    subject_areas <- paste(subject_areas,collapse=" - ")
  }else{subject_areas<-NA}
  
  return(data.table(eid=eid,
                    pubmed_id=pubmed_id,
                    doi=doi,
                    title=coredata$`dc:title`,
                    keywords=keywords,
                    journal_name=coredata$`prism:publicationName`,
                    journal_id=coredata$`source-id`,
                    issn=coredata$`prism:issn`,
                    volume=coredata$`prism:volume`,
                    issue_id=coredata$`prism:issueIdentifier`,
                    cover_date=coredata$`prism:coverDate`,
                    subject_areas=subject_areas))
}
## Test
#basics_dt <- get_basics(abstract)
# ------------------------------------------


# Get abstract
# ------------------------------------------
get_abstract <- function(abstract,doi=NA,pubmed_id=NA){
  abstrct <- abstract$content$`abstracts-retrieval-response`$coredata$`dc:description`
  if(is.null(abstrct)){abstrct <- NA}
  return(data.table(doi=doi,
                    pubmed_id=pubmed_id,
                    abstract=abstrct))
}
# ------------------------------------------
