# (1) Scrape journal-issn-numbers
# ------------------------------------------
library(rvest)
library(stringi)
library(data.table)
library(rcrossref)

# Go through these...
S <- paste0("http://mjl.clarivate.com/cgi-bin/jrnlst/jlresults.cgi?PC=D&mode=print&Page=",1:20)
SS <- paste0("http://mjl.clarivate.com/cgi-bin/jrnlst/jlresults.cgi?PC=SS&mode=print&Page=",1:10)
H <- paste0("http://mjl.clarivate.com/cgi-bin/jrnlst/jlresults.cgi?PC=H&mode=print&Page=",1:10)
ES <- paste0("http://mjl.clarivate.com/cgi-bin/jrnlst/jlresults.cgi?PC=EX&mode=print&Page=",1:20)

get_issn <- function(page){
  issn <- read_html(page) %>% html_text()
  issn <- stringi::stri_split(str = issn, regex = "ISSN:")
  issn <- sapply(issn,function(x)gsub(substr(x,1,10),pattern = " ",replacement = ""))
  issn <- issn[sapply(issn,function(x)stri_detect(str = x,regex = "-"))]
  return(issn)
}

S_issn <- SS_issn <- H_issn <- ES_issn <- list()
S_issn <- lapply(S,function(x) try(get_issn(page = x)))
SS_issn <- lapply(SS,function(x) try(get_issn(page = x)))
H_issn <- lapply(H,function(x) try(get_issn(page = x)))
ES_issn <- lapply(ES,function(x) try(get_issn(page = x)))
issns <- unlist(list(S_issn,SS_issn,H_issn,ES_issn))
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
      dois_year_dt[[ticker]] <- data.table(doi=temp$data$DOI,
                                           issn=temp$data$ISSN,
                                           issued=temp$data$issued)
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
science_dois <- get_dois(issn = '0036-8075',
                         from_year = 2005, 
                         to_year = 2006)

# Example 2
# library(rcrossref)
# ex <- cr_works(filter = c(issn=c(issns[100])),limit = 1000)
# dim(ex$data)
# ex$data
# ------------------------------------------



# (3) Translate between DOI and Pubmed ID
# ------------------------------------------

# APPROACH 1
# ===========
library(devtools)
devtools::install_github("ropensci/alm")
library(alm)

out <- alm_ids(doi='10.1371/journal.pone.0029797', info='detail')
out <- alm_ids(doi=dois_dt[1:1000]$doi)
dois_dt$doi[2]
# ===========


# APPROACH 2
# ===========
library(devtools)
install_github("ropensci/rentrez")
library(rentrez)

get_pubmedid <- function(doi){
  pubmedid <- entrez_search(db = "pubmed", term = paste0(doi,"[doi]"))$id
  if(is.list(pubmedid)){pubmedid <- NA}
  return(data.table(doi=doi,pubmedid=pubmedid))
}

# Standard
pubmed_dt <- lapply(dois_dt[1:10]$doi, function(d) get_pubmedid(doi = d))
pubmed_dt <- rbindlist(pubmed_dt)

# Parallel (faster?)
dois <- dois_dt[1:1000]$doi
library(parallel)
cl <- parallel::makeCluster(7)
varlist <- c("data.table","get_pubmedid","entrez_search","dois")
parallel::clusterExport(cl = cl,varlist = varlist,envir = environment())
system.time(pubmed_dt <- parLapply(cl = cl,
                       X = dois,
                       fun = function(x) try(get_pubmedid(doi = x))))
parallel::stopCluster(cl)
pubmed_dt <- rbindlist(pubmed_dt)
pubmed_dt[!is.na(pubmedid)]
# ===========
# ------------------------------------------





# (4) Get article, references, etc. via scopus
# ------------------------------------------
library(rscopus)

# Set api key
set_api_key("c2c55699f1c9d5642e2e0c744291fdc5")

# Examle (DOI)
article <- rscopus::article_retrieval(id = science_dois$doi[5],
                                      identifier = c("doi"),
                                      http_end = NULL)
abstract <- rscopus::abstract_retrieval(id = science_dois$doi[5],
                                        identifier = c("doi"),
                                        http_end = NULL)

# Example (PUBMEDID)
article <- rscopus::article_retrieval(id = pubmed_dt[!is.na(pubmedid),]$pubmedid[5],
                                      identifier = c("pubmed_id"),
                                      http_end = NULL)
abstract <- rscopus::abstract_retrieval(id = pubmed_dt[!is.na(pubmedid),]$pubmedid[5],
                                        identifier = c("pubmed_id"),
                                        http_end = NULL)

# As a function
# -----------------------------------
get_abstract <- function(doi){
  abstract <- abstract_retrieval(id = doi,
                                 identifier = c("doi"), 
                                 http_end = NULL)
  content <- abstract$content$`abstracts-retrieval-response`$coredata$`dc:description`
  if(is.null(content)){
    return(NA)
  }else{
    return(list(paper$content$`abstracts-retrieval-response`$coredata$`dc:description`,
                paper$content$`abstracts-retrieval-response`$authkeywords,
                paper$content$`abstracts-retrieval-response`$item))
  }
}
abstracts <- lapply(science_dois$doi,function(d) get_abstract(doi = d))
# -----------------------------------
# ------------------------------------------



# (5) Get abstracts via entrez
# ------------------------------------------
#https://cran.r-project.org/web/packages/rentrez/vignettes/rentrez_tutorial.html
raw_abs <- entrez_fetch(db="pubmed", id= pubmed_dt[!is.na(pubmedid),]$pubmedid[5], rettype="abstract")
cat(raw_abs)
# ------------------------------------------



# (6) Get article information
# -----------------------------------

# For one
article_info <- get_article_info(doi = science_dois$doi[5])

# For multiple
article_info <- lapply(science_dois$doi[1:2],function(d) try(get_article_info(doi = d)))
# -----------------------------------
