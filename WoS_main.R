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
if(computer=="martin work"){
  git_source <- "C:/Users/marar08/Documents/WoS/"
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



# (2) Get doi's via cross ref
# ------------------------------------------

pnas_issn <- '1091-6490'
science_issn <- '0036-8075'

# Example (Science dois)
system.time(dois <- get_dois(issn = pnas_issn,
                             from_year = 1950, 
                             to_year = 1980))
science_dois2 <- copy(science_dois) # 1980-1999
dois <- rbindlist(list(dois,science_dois2)) # 1950-1999
dois[,issued := lubridate::as_date(issued)]
dois <- dois[order(issued,decreasing = F)]
dois <- dois[!is.na(doi)]
dois <- dois[!is.na(issued)]
#science_dois_1880_1999 <- science_dois[issued <= lubridate::as_date("1999-12-31")]
# ------------------------------------------



# (3) Get article information
# -----------------------------------

# Martin api keys
m_api_keys <- c("491dbbb3c51a864fb3fe0687c14a13c6",
              "b3b19b96818de6f942e29b02af817219",
              "60700745d816adfd2235243f6f0d3250",
              "3b58854fc85e5848d9a8b84acd58976d",
              "59fbaedb32e77515a5364506bb468732",
              "a8fbf252049526799088b72014abc1ec",
              "694e41851d3fa74806ed2f2d454cc806",
              "0cf5cf1d069994ab5791422a8b4cdf92",
              "d29d0e1af710f5dbe6475fee9918b053",
              "da9b7da0b687699e0cc891a24e0f7060")
# Daniel api keys
d_api_keys <- c("6cddb0fc46ee0d33a2c16d6461c69520",
                "a516238b4e04da76c9767608c2ea3221",
                "0806f72909f9cad92e7652068bb126f8",
                "d2d26693022965b488797d7a05ee3a38",
               "0c7a0fd92a573fc2912605bd39fe5d13",
                "681e64e196ade9f967f96bbec5d40293",
                "d6fceef5f04beca904706fb080348eaf",
                "2340ad2b603d98724cfd574ad9c093e6",
                "6594db80bd9975a67e41c2e2d8f40fee",
                "6ac6ceeea10cc86bf4caea38eb341901")
api_keys <- c(m_api_keys,d_api_keys)
set_api_key(api_keys[1])

# For one
# =============
#article_info <- get_article_info(doi = science_dois$doi[5])


# For multiple (non-parallel)
# =============
ai_dt <- lapply(science_dois$doi[1:10],function(d) try(get_article_info(doi = d)))


# For multiple (parallel)
# =============

# Split into chunks
chunk_size <- 10000
doi_vector <- dois$doi
#doi_vector <- science_dois_1880_1999$doi
doi_chunks <- split(doi_vector, ceiling(seq_along(doi_vector)/chunk_size))

# Process by chunk
res <- list()
process_n_chunks <- 1:length(doi_chunks)
#process_n_chunks <- 11:18
for(i in process_n_chunks){
  
  # Set api-key
  api_key <- api_keys[7+i]
  set_api_key(api_key)
  
  # Collect
  ncores <- detectCores() - 1
  cl <- parallel::makeCluster(ncores)
  varlist <- c("data.table","get_article_info","get_basics","get_authors_dt",
               "get_reference_dt","get_abstract","abstract_retrieval","set_api_key",
               "api_key","rbindlist","setcolorder","unlist","setkeyv")
  parallel::clusterExport(cl = cl,varlist = varlist,envir = environment())
  system.time(ai_dt <- parLapply(cl = cl,
                                 X = doi_chunks[[i]],
                                 fun = function(d) try(get_article_info(doi = d,api_key = api_key))))
  parallel::stopCluster(cl)
  gc()
  
  # Add to res list
  res[[i]] <- ai_dt
  rm(ai_dt)
}

# Check errors
a <- sapply(ai_dt,is.list)
a <- which(a==FALSE)
science_dois$doi[a]
ai_dt[a]
ai_dt[!a]
length(ai_dt)

## Extract list elements (single api-key/batch)
# basics_dt <- rbindlist(lapply(ai_dt[sapply(ai_dt,is.list)],function(x)x$basics_dt))
# authors_dt <- rbindlist(lapply(ai_dt[sapply(ai_dt,is.list)],function(x)x$authors_dt))
# reference_dt <- rbindlist(lapply(ai_dt[sapply(ai_dt,is.list)],function(x)x$reference_dt))
# abstract_dt <- rbindlist(lapply(ai_dt[sapply(ai_dt,is.list)],function(x)x$abstract_dt))

## Extract list elements (multiple api-key/batch)
basics_dt <- authors_dt <- reference_dt <- abstract_dt <- list()
for(i in 1:length(res)){
  basics_dt[[i]] <- rbindlist(lapply(res[[i]][sapply(res[[i]],is.list)],function(x)x$basics_dt))
  authors_dt[[i]] <- rbindlist(lapply(res[[i]][sapply(res[[i]],is.list)],function(x)x$authors_dt))
  reference_dt[[i]] <- rbindlist(lapply(res[[i]][sapply(res[[i]],is.list)],function(x)x$reference_dt))
  abstract_dt[[i]] <- rbindlist(lapply(res[[i]][sapply(res[[i]],is.list)],function(x)x$abstract_dt))
}
basics_dt <- rbindlist(basics_dt)
authors_dt <- rbindlist(authors_dt)
reference_dt <- rbindlist(reference_dt)
abstract_dt <- rbindlist(abstract_dt)

# Export
getwd()
#export_name <- "science_1970_1999 (2018-04-11).rds"
#export_name <- "PNAS_2000_2017 (2018-05-17).rds"
export_name <- "PNAS_1949_1998 (2018-05-22).rds"
saveRDS(object = list(basics_dt=basics_dt,
                      authors_dt=authors_dt,
                      reference_dt=reference_dt,
                      abstract_dt=abstract_dt),
        file = export_name)
#saveRDS(object = ai_dt, file = "ai_dt(2018-03-22).rds")

# Check pubmed_ids for failed ones
pubmed_dt <- lapply(science_dois$doi[a][1:10], function(d) get_pubmedid(doi = d))
pubmed_dt <- rbindlist(pubmed_dt)
tst <- get_article_info(pubmed_id = pubmed_dt[!is.na(pubmedid)]$pubmedid[1],api_key = api_key)
# -----------------------------------
