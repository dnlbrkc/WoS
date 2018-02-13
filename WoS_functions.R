##
## WoS functions
##
## ----------------


# Get article information
# ------------------------------------------
get_article_info <- function(doi=NULL,
                             pubmed_id=NULL){
  
  ## Test
  #doi = science_dois$doi[9]
  
  # Retrieve abstract
  if(!is.null(doi)){
    id <- doi
    abstract <- rscopus::abstract_retrieval(id = doi,
                                            identifier = c("doi"),
                                            http_end = NULL)
  }else{
    id <- pubmed_id
    abstract <- rscopus::abstract_retrieval(id = pubmed_id,
                                            identifier = c("pubmed_id"),
                                            http_end = NULL)
  }
  
  # Extract useful information
  basics_dt <- get_basics(abstract)
  authors_dt <- get_authors_dt(abstract)
  reference_dt <- get_reference_dt(abstract)
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
get_authors_dt <- function(abstract){
  
  extract_author_info <- function(author){
    aff_id <- author$affiliation[['@id']]
    if(is.null(aff_id)){aff_id <-NA}
    return(data.table(author_id=author[['@auid']],
                      seq=author[['@seq']],
                      given_name=author[['ce:given-name']],
                      initials=author[['ce:initials']],
                      surname=author[['ce:surname']],
                      affiliation_id=aff_id))
  }
  
  authors <- abstract$content$`abstracts-retrieval-response`$authors$author
  author_dt <- lapply(authors,extract_author_info)
  author_dt <- rbindlist(author_dt)
  
  # Add affiliation details
  extract_aff_details <- function(affiliation){
    #affiliation <- affiliations[[1]]
    return(data.table(affiliation_id=affiliation[['@id']],
                      affiliation_name=affiliation$affilname,
                      affiliation_country=affiliation$`affiliation-country`))
  }
  affiliations <- abstract$content$`abstracts-retrieval-response`$affiliation
  affiliation_dt <- lapply(affiliations,extract_aff_details)
  affiliation_dt <- rbindlist(affiliation_dt)
  author_dt <- merge(x=author_dt,y=affiliation_dt,by="affiliation_id",all.x=T,all.y=F)
  return(author_dt)
}
## Test
#auth_dt <- get_authors_dt(abstract = abstract)
# ------------------------------------------


# Get reference dt
# ------------------------------------------
get_reference_dt <- function(abstract){
  
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
  return(reference_dt)
}
## Test
#ref_dt <- get_reference_dt(abstract = abstract)
# ------------------------------------------


# Get basics
# ------------------------------------------
get_basics <- function(abstract){
  coredata <- abstract$content$`abstracts-retrieval-response`$coredata
  eid <- coredata$eid;if(is.null(eid)){eid <- NA}
  pubmed_id=coredata$`pubmed-id`;if(is.null(pubmed_id)){pubmed_id <- NA}
  doi <- coredata$`prism:doi`;if(is.null(doi)){doi<-NA}
  return(data.table(eid=eid,
                    pubmed_id=pubmed_id,
                    doi=doi,
                    title=coredata$`dc:title`,
                    journal_name=coredata$`prism:publicationName`,
                    journal_id=coredata$`source-id`,
                    issn=coredata$`prism:issn`,
                    volume=coredata$`prism:volume`,
                    issue_id=coredata$`prism:issueIdentifier`,
                    cover_date=coredata$`prism:coverDate`))
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
