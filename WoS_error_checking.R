# ------------------------------------------
#
# SoS project
# * Error-checking 
#
# ------------------------------------------


# Set user
# ------------------------------------------
computer <- "martin work"
if(computer=="martin work"){
  git_source <- "C:/Users/marar08/Documents/WoS/"
  data_source <- "C:/Users/marar08/Desktop/work/science_of_science/data/"
}else if(computer=="daniel"){
  git_source <- ""
}
# ------------------------------------------



# (0) Load functions & libraries
# ------------------------------------------
setwd(git_source)
source("WoS_functions.R")
# ------------------------------------------


# (1) Import
# ------------------------------------------
setwd(data_source)
data_files <- list.files(data_source)
ai_dt <- readRDS(file = "ai_dt.rds")
scienceDOI <- readRDS(file = "scienceDOI.rds")
# ------------------------------------------


# (2)
# ------------------------------------------

# Set api key
api_key <- "c2c55699f1c9d5642e2e0c744291fdc5"
set_api_key(api_key)

# For one
article_info <- get_article_info(doi = scienceDOI$doi[8])

# Try multiple
tst <- lapply(science_dois$doi[1:10],function(x)try(get_article_info(doi = x)))
tst <- lapply(scienceDOI$doi[1:50],function(x)try(get_article_info(doi = x)))
tst[sapply(tst,is.character)]
# ------------------------------------------