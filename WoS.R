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


# 
# #trying to find link to "cited references page" based on the WOS ID of each paper
# sessionID <- "7C205FA0D1F0CF17AFE9D924B20D9BD7"
# WOS_ID <- "WOS:A1968A428000023"
# 
# 
# WOS_ID <- pubs2$uid[2]
# link <- paste0("http://apps.webofknowledge.com/InterService.do;jsessionid=",sessionID,
#         "?product=WOS&toPID=WOS&action=AllCitationService&isLinks=yes&highlighted_tab=WOS&last_prod=WOS&fromPID=WOS&returnLink=http%3a%2f%2fapps.webofknowledge.com%2ffull_record.do%3fhighlighted_tab%3dWOS%26last_prod%3dWOS%26search_mode%3dAdvancedSearch%26qid%",
#         "3d2%27log_event%3dyes%26product%3dWOS%26SID%3dD62RGf3DTL1AwixErdZ%26viewType%3dfullRecord%26doc%3d1%26page%3d1&srcDesc=RET2WOS&srcAlt=Back+to+Web+of+Science&UT=",
#         WOS_ID,
#         "&search_mode=CitedRefList&SID=D62RGf3DTL1AwixErdZ&parentProduct=WOS&parentQid=2&parentDoc=1&recid=",
#         WOS_ID,
#         "&PREC_REFCOUNT=6&fromRightPanel=true")
# 
# 
# 
# a <-"http://apps.webofknowledge.com/InterService.do?product=WOS&toPID=WOS&action=AllCitationService&isLinks=yes&highlighted_tab=WOS&last_prod=WOS&fromPID=WOS&returnLink=http%3a%2f%2fapps.webofknowledge.com%2ffull_record.do%3fhighlighted_tab%3dWOS%26last_prod%3dWOS%26search_mode%3dAdvancedSearch%26qid%3d60%26log_event%3dyes%26product%3dWOS%26SID%3dD62RGf3DTL1AwixErdZ%26viewType%3dfullRecord%26doc%3d1%26page%3d1&srcDesc=RET2WOS&srcAlt=Back+to+Web+of+Science&UT=WOS:A1970G983200030&search_mode=CitedRefList&SID=D62RGf3DTL1AwixErdZ&parentProduct=WOS&parentQid=60&parentDoc=1&recid=WOS:A1970G983200030&PREC_REFCOUNT=19&fromRightPanel=true"
# b<- "http://apps.webofknowledge.com/InterService.do?product=WOS&toPID=WOS&action=AllCitationService&isLinks=yes&highlighted_tab=WOS&last_prod=WOS&fromPID=WOS&returnLink=http%3a%2f%2fapps.webofknowledge.com%2ffull_record.do%3fhighlighted_tab%3dWOS%26last_prod%3dWOS%26search_mode%3dAdvancedSearch%26qid%3d51%26log_event%3dyes%26product%3dWOS%26SID%3dD62RGf3DTL1AwixErdZ%26viewType%3dfullRecord%26doc%3d1%26page%3d1&srcDesc=RET2WOS&srcAlt=Back+to+Web+of+Science&UT=WOS:A1968A428000023&search_mode=CitedRefList&SID=D62RGf3DTL1AwixErdZ&parentProduct=WOS&parentQid=51&parentDoc=1&recid=WOS:A1968A428000023&PREC_REFCOUNT=6&fromRightPanel=true"


#Get missing DOI information by matching web of science titles with CrossRef 
library(rcrossref)
retreive_doi=cr_works(filter = c(issn=c('0036-8075')),flq = c(query.title=pubs$title[1]))

doi <- retreive_doi[[2]]$DOI[1]



#########################################
#Get abstract for a paper using SCOPUS
#########################################

library(rscopus)
set_api_key("fe18beed32b3e133b6b157ec0e455266") #you can get your own API key by registering on the SCOPUS website

example_doi <- '4321261'


paper <- abstract_retrieval(example_doi,
                            identifier = c("pubmed_id"), 
                            http_end = NULL)

#get abstract
paper$content$`abstracts-retrieval-response`$coredata$`dc:description`


#get keywords
paper$content$`abstracts-retrieval-response`$authkeywords

#get references
paper$content$`abstracts-retrieval-response`$item

########################################################################################
#since the DOI information is not enough for scopus, we need a way to obtain pubmed IDs.
########################################################################################

library(RISmed)

#search journal and publication type on Pubmed
search_topic <- '("Science"[Journal]) AND "journal article"[Publication Type]'

#not sure yet how to get all results
search_query <- EUtilsSummary(search_topic, 
                              retstart=0,
                              retmax=10000, 
                              mindate=1900,
                              maxdate=2018)
#get PUBMED ID for each entry
QueryId(search_query)



#alternatively search for specific title
search_topic <- 'REPRESSION OF INTERFERON ACTION - INDUCED DEDIFFERENTIATION OF EMBRYONIC CELLS[Title]'
search_query <- EUtilsSummary(search_topic, retstart=0,retmax=1000, mindate=1900,maxdate=2018)

#get pubmed ID for that entry
QueryId(search_query)


