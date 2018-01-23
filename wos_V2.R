###########################
#STEP 1: SEARCH FOR JOURNAL 
###########################

library(RISmed)

#search journal and publication type on Pubmed
search_topic <- '("SCIENCE"[Journal]) NOT (("addresses"[Publication Type] OR "autobiography"[Publication Type] OR "bibliography"[Publication Type] OR "biography"[Publication Type] OR "book illustrations"[Publication Type] OR "case reports"[Publication Type] OR "comment"[Publication Type] OR "congresses"[Publication Type] OR "consensus development conference"[Publication Type] OR "consensus development conference, nih"[Publication Type] OR "dataset"[Publication Type] OR "dictionary"[Publication Type] OR "directory"[Publication Type] OR "editorial"[Publication Type] OR "festschrift"[Publication Type] OR "government publications"[Publication Type] OR "guideline"[Publication Type] OR "historical article"[Publication Type] OR "interview"[Publication Type] OR "lectures"[Publication Type] OR "letter"[Publication Type] OR "news"[Publication Type] OR "newspaper article"[Publication Type] OR "personal narratives"[Publication Type] OR "popular works"[Publication Type] OR "portraits"[Publication Type] OR "practice guideline"[Publication Type] OR "review"[Publication Type] OR "scientific integrity review"[Publication Type])) '

#not sure yet how to get all results using 'retstart' and 'retmax'
search_query <- EUtilsSummary(search_topic, 
                              retstart=0,
                              retmax=20000,
                              mindate=1900,
                              maxdate=2016)

#get PUBMED ID for each entry
pubmed_ids <- QueryId(search_query)


#################################################################
#STEP 2: DOWNLOAD ABSTRACT KEYWORDS AND REFERENCES FOR EACH PAPER
#ONLY WORKS FROM IP THAT HAS ELSEVIER SUBSCRIPTION###############
#################################################################

library(rscopus)
set_api_key("fe18beed32b3e133b6b157ec0e455266") 

abstracts <- list()
counter=0
for(ID in pubmed_ids){
  counter=counter+1
  paper <- abstract_retrieval(ID,
                              identifier = c("pubmed_id"), 
                              http_end = NULL)
  
  abstracts[[counter]] <- list(paper$content$`abstracts-retrieval-response`$coredata$`dc:description`,
                               paper$content$`abstracts-retrieval-response`$authkeywords,
                               paper$content$`abstracts-retrieval-response`$item)
                               
}

##################################
#STEP 3: IDENTIFY MISSING ENTRIES#
##################################

missing <- sapply(1:length(abstracts), function(x) is.null(abstracts[[x]][[1]]))
missing <- which(missing==T)

#extract titles and identify which one's have missing info
records <- EUtilsGet(search_query)
pubmed_data <- data.frame('Title'=ArticleTitle(records))

##################################################################################
#STEP 4: GET DOI FOR TITLES THAT HAVE MISSING ABSTRACT, KEYWORDS AND REFS#########
##################################################################################

library(rcrossref)

titles <- pubmed_data$Title[missing]

dois <- vector()
c = 0
for (t in titles){
c=c+1
ret_doi=cr_works(filter = c(issn=c('0036-8075')),flq = c(query.title=as.character(t)))
dois[c] <- ret_doi[[2]]$DOI[1]
}

#try to get abstract based on doi:
abstracts_2 <- list()
counter=0
for(ID in dois){
  counter=counter+1
  paper <- abstract_retrieval(ID,
                              identifier = c("doi"), 
                              http_end = NULL)
  
  content <- paper$content$`abstracts-retrieval-response`$coredata$`dc:description`
  if(is.null(content)){
  abstracts_2[[counter]] <- NA
  } else {
  abstracts_2[[counter]] <- list(paper$content$`abstracts-retrieval-response`$coredata$`dc:description`,
                                 paper$content$`abstracts-retrieval-response`$authkeywords,
                                 paper$content$`abstracts-retrieval-response`$item)
  }
}




########################################################
#STEP 5: MERGE RESULTS BASED ON DOI AND PUBMED #########
########################################################
c=0
for(i in missing){
  c=c+1
  abstracts[[i]] <- abstracts_2[[c]]
}


