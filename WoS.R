library(rwos)
library(dplyr)

sid <- wos_authenticate()

#Download all articles published in Science

res <- wos_search(sid, "((SO=(SCIENCE)) AND (DT=(Article)))")
pubs <- wos_retrieve(res, first = 1, count = res$results)


#Remove duplicates
ids <- table(pubs$uid)
duplicates <- which(ids!=1)
dupL <- ids[duplicates]

select <- sapply(1:length(dupL), 
                 function(x) which(pubs$uid==names(dupL[x])))

if(nrow(select)==2){
pubs2 <- pubs[-c(select[2,]),]
} else {
  print("More than 2 entries found, check code")
}

save(pubs2,file="science.Rdata")



#Get missing DOI information by matching web of science titles with CrossRef 
library(rcrossref)
retreive_doi=cr_works(filter = c(issn=c('0036-8075')),flq = c(query.title=pubs$title[1]))

doi <- retreive_doi[[2]]$DOI[1]


########################################################################################
#since the DOI information is not enough for scopus, we need a way to obtain pubmed IDs.
########################################################################################

library(RISmed)

#search journal and publication type on Pubmed
search_topic <- '(("science"[Journal]) AND "journal article"[Publication Type]) AND ("1900"[Date - Completion] : "2016"[Date - Completion]) '

#not sure yet how to get all results
search_query <- EUtilsSummary(search_topic, 
                              retstart=0,
                              retmax=20000)
#get PUBMED ID for each entry
pubmed_ids <- QueryId(search_query)



# #alternatively search for specific title
# search_topic <- 'REPRESSION OF INTERFERON ACTION - INDUCED DEDIFFERENTIATION OF EMBRYONIC CELLS[Title]'
# search_query <- EUtilsSummary(search_topic, retstart=0,retmax=1000, mindate=1900,maxdate=2018)
# 
# #get pubmed ID for that entry
# QueryId(search_query)




##################################################################################
#Get abstract, keywords and references for each paper using SCOPUS
##################################################################################

library(rscopus)
set_api_key("fe18beed32b3e133b6b157ec0e455266") #you can get your own API key by registering on the SCOPUS website

abstracts <- list()
counter=0
for(ID in pubmed_ids){
  counter=counter+1
  paper <- abstract_retrieval(ID,
                              identifier = c("pubmed_id"), 
                              http_end = NULL)
  
  
  abstracts[[counter]] <- paper$content$`abstracts-retrieval-response`$coredata$`dc:description`
}

#identify missing abstracts
missing <- sapply(1:length(abstracts), function(x) is.null(abstracts[[x]]))
missing <- which(missing==T)


#get abstract



#get keywords
paper$content$`abstracts-retrieval-response`$authkeywords

#get references
paper$content$`abstracts-retrieval-response`$item
