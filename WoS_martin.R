# (1) Scrape journal-issn-numbers
# ------------------------------------------
library(rvest)
library(stringi)

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

S_issn <- lapply(S,function(x) try(get_issn(page = x)))
SS_issn <- lapply(SS,function(x) try(get_issn(page = x)))
H_issn <- lapply(H,function(x) try(get_issn(page = x)))
ES_issn <- lapply(ES,function(x) try(get_issn(page = x)))
issns <- unlist(list(S_issn,SS_issn,H_issn,ES_issn))
# ------------------------------------------


# (2) Obrain doi's via crossref 
# ------------------------------------------

# Example
library(rcrossref)
ex <- cr_works(filter = c(issn=c(issns[100])),limit = 1000)
dim(ex$data)
ex$data
# ------------------------------------------


# (3) Get article, references, etc. via scopus
# ------------------------------------------
library(rscopus)

# Set api key
set_api_key("c2c55699f1c9d5642e2e0c744291fdc5")

# Example
article <- rscopus::article_retrieval(id = ex$data$DOI[1],
                           identifier = c("doi"),
                           http_end = NULL)
# ------------------------------------------
