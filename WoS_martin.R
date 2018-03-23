# ------------------------------------------
#
# SoS project
# * Data collection
# > Main
#
# ------------------------------------------


# Set user
# ------------------------------------------
computer <- "martin work"
if(computer=="martn work"){
  git_folder <- "C:/Users/marar08/Documents/WoS/"
}else if(computer=="daniel"){
  git_source <- ""
}
# ------------------------------------------



# (0) Load functions & libraries
# ------------------------------------------
setwd(git_source)
source("WoS_functions.R")
# ------------------------------------------



# (1) Scrape journal-issn-numbers
# ------------------------------------------

# Go through these...
S <- paste0("http://mjl.clarivate.com/cgi-bin/jrnlst/jlresults.cgi?PC=D&mode=print&Page=",1:20)
SS <- paste0("http://mjl.clarivate.com/cgi-bin/jrnlst/jlresults.cgi?PC=SS&mode=print&Page=",1:10)
H <- paste0("http://mjl.clarivate.com/cgi-bin/jrnlst/jlresults.cgi?PC=H&mode=print&Page=",1:10)
ES <- paste0("http://mjl.clarivate.com/cgi-bin/jrnlst/jlresults.cgi?PC=EX&mode=print&Page=",1:20)

S_issn <- SS_issn <- H_issn <- ES_issn <- list()
S_issn <- lapply(S,function(x) try(get_issn(page = x)))
SS_issn <- lapply(SS,function(x) try(get_issn(page = x)))
H_issn <- lapply(H,function(x) try(get_issn(page = x)))
ES_issn <- lapply(ES,function(x) try(get_issn(page = x)))
issns <- unlist(list(S_issn,SS_issn,H_issn,ES_issn))
# ------------------------------------------



# Get doi's via cross ref
# ------------------------------------------

# Example (Science dois)
system.time(science_dois <- get_dois(issn = '0036-8075',
                         from_year = 2000, 
                         to_year = 2017))
science_dois
# ------------------------------------------



# (3) Translate between DOI and Pubmed ID
# ------------------------------------------

# APPROACH 1
# ===========
out <- alm_ids(doi='10.1371/journal.pone.0029797', info='detail')
out <- alm_ids(doi=dois_dt[1:1000]$doi)
dois_dt$doi[2]
# ===========


# APPROACH 2
# ===========
# Standard
pubmed_dt <- lapply(dois_dt[1:10]$doi, function(d) get_pubmedid(doi = d))
pubmed_dt <- rbindlist(pubmed_dt)
# Parallel (faster?)
dois <- dois_dt[1:1000]$doi
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
# Set api key
set_api_key("c2c55699f1c9d5642e2e0c744291fdc5")
rscopus::generic_elsevier_api(api_key = )
set_

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

# Using function (outdated)
#abstracts <- lapply(science_dois$doi,function(d) get_abstract(doi = d))
# ------------------------------------------



# (5) Get abstracts via entrez
# ------------------------------------------
#https://cran.r-project.org/web/packages/rentrez/vignettes/rentrez_tutorial.html
raw_abs <- entrez_fetch(db="pubmed", id= pubmed_dt[!is.na(pubmedid),]$pubmedid[5], rettype="abstract")
cat(raw_abs)
# ------------------------------------------



# (6) Get article information
# -----------------------------------

# Set api key
api_key <- "b8a2b819586396f53d4b1dbe8e40cb82"
set_api_key(api_key)

# For one
article_info <- get_article_info(doi = science_dois$doi[5])

# For multiple
article_info <- lapply(science_dois$doi[1:10],function(d) try(get_article_info(doi = d)))

# For multiple (parallel)
cl <- parallel::makeCluster(7)
varlist <- c("data.table","get_article_info","get_basics","get_authors_dt",
             "get_reference_dt","get_abstract","abstract_retrieval","set_api_key",
             "api_key","rbindlist","setcolorder")
parallel::clusterExport(cl = cl,varlist = varlist,envir = environment())
system.time(ai_dt <- parLapply(cl = cl,
                                   X = science_dois$doi,
                                   fun = function(d) try(get_article_info(doi = d,api_key = api_key))))
parallel::stopCluster(cl)
a <- sapply(ai_dt,is.list)
a <- which(a==FALSE)
science_dois$doi[a]
length(ai_dt)

# Extract list elements
basics_dt <- rbindlist(lapply(ai_dt[sapply(ai_dt,is.list)],function(x)x$basics_dt))
authors_dt <- rbindlist(lapply(ai_dt[sapply(ai_dt,is.list)],function(x)x$authors_dt))
reference_dt <- rbindlist(lapply(ai_dt[sapply(ai_dt,is.list)],function(x)x$reference_dt))
abstract_dt <- rbindlist(lapply(ai_dt[sapply(ai_dt,is.list)],function(x)x$abstract_dt))

# Check pubmed_ids for failed ones
pubmed_dt <- lapply(science_dois$doi[a][1:10], function(d) get_pubmedid(doi = d))
pubmed_dt <- rbindlist(pubmed_dt)
tst <- get_article_info(pubmed_id = pubmed_dt[!is.na(pubmedid)]$pubmedid[1],api_key = api_key)
# -----------------------------------



