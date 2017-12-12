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

#trying to find link to "cited references page" based on the WOS ID of each paper
sessionID <- "7C205FA0D1F0CF17AFE9D924B20D9BD7"
WOS_ID <- "WOS:A1968A428000023"


WOS_ID <- pubs2$uid[2]
link <- paste0("http://apps.webofknowledge.com/InterService.do;jsessionid=",sessionID,
        "?product=WOS&toPID=WOS&action=AllCitationService&isLinks=yes&highlighted_tab=WOS&last_prod=WOS&fromPID=WOS&returnLink=http%3a%2f%2fapps.webofknowledge.com%2ffull_record.do%3fhighlighted_tab%3dWOS%26last_prod%3dWOS%26search_mode%3dAdvancedSearch%26qid%",
        "3d2%27log_event%3dyes%26product%3dWOS%26SID%3dD62RGf3DTL1AwixErdZ%26viewType%3dfullRecord%26doc%3d1%26page%3d1&srcDesc=RET2WOS&srcAlt=Back+to+Web+of+Science&UT=",
        WOS_ID,
        "&search_mode=CitedRefList&SID=D62RGf3DTL1AwixErdZ&parentProduct=WOS&parentQid=2&parentDoc=1&recid=",
        WOS_ID,
        "&PREC_REFCOUNT=6&fromRightPanel=true")



a <-"http://apps.webofknowledge.com/InterService.do?product=WOS&toPID=WOS&action=AllCitationService&isLinks=yes&highlighted_tab=WOS&last_prod=WOS&fromPID=WOS&returnLink=http%3a%2f%2fapps.webofknowledge.com%2ffull_record.do%3fhighlighted_tab%3dWOS%26last_prod%3dWOS%26search_mode%3dAdvancedSearch%26qid%3d60%26log_event%3dyes%26product%3dWOS%26SID%3dD62RGf3DTL1AwixErdZ%26viewType%3dfullRecord%26doc%3d1%26page%3d1&srcDesc=RET2WOS&srcAlt=Back+to+Web+of+Science&UT=WOS:A1970G983200030&search_mode=CitedRefList&SID=D62RGf3DTL1AwixErdZ&parentProduct=WOS&parentQid=60&parentDoc=1&recid=WOS:A1970G983200030&PREC_REFCOUNT=19&fromRightPanel=true"
b<- "http://apps.webofknowledge.com/InterService.do?product=WOS&toPID=WOS&action=AllCitationService&isLinks=yes&highlighted_tab=WOS&last_prod=WOS&fromPID=WOS&returnLink=http%3a%2f%2fapps.webofknowledge.com%2ffull_record.do%3fhighlighted_tab%3dWOS%26last_prod%3dWOS%26search_mode%3dAdvancedSearch%26qid%3d51%26log_event%3dyes%26product%3dWOS%26SID%3dD62RGf3DTL1AwixErdZ%26viewType%3dfullRecord%26doc%3d1%26page%3d1&srcDesc=RET2WOS&srcAlt=Back+to+Web+of+Science&UT=WOS:A1968A428000023&search_mode=CitedRefList&SID=D62RGf3DTL1AwixErdZ&parentProduct=WOS&parentQid=51&parentDoc=1&recid=WOS:A1968A428000023&PREC_REFCOUNT=6&fromRightPanel=true"






