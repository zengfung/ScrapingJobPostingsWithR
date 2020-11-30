library(XML)
library(RCurl)
library(httr)

# function to get data
getJobInfo = function(result){
  jobTitle = result$Title
  if (is.null(jobTitle)){
    return (list(title = NA))
  }
  
  jobLocation = result$LocationText
  jobCompany = result$Company$Name

  # prevent reCAPTCHA with Sys.sleep
  # Sys.sleep(1)
  jobURL = result$TitleLink
  jobLink = suppressMessages(htmlParse(GET(jobURL)))
  jobDescription = xpathSApply(jobLink, "//div[@name='value_description']//text()", xmlValue)
  
  return(list(title = jobTitle,
              source = "monster.com",
              company = jobCompany,
              location = jobLocation,
              description = paste(jobDescription, collapse = "@@"),
              link = jobURL))
}

getTotalResults = function(doc){
  nresult_str = xmlValue(getNodeSet(doc, "//header[@class='title']/h2[@class='figure']"))
  m = regexpr("[0-9]+", nresult_str)
  n = as.integer(regmatches(nresult_str, m))
  return (n)
}


# function to get data for different searches
getJobPostings = function(search){
  url = "https://www.monster.com"
  p = "/jobs/search/pagination/"
  loc = "san francisco"
  intcid = "skr_navigation_nhpso_searchMain"
  isDynamicPage = "true"
  isMKPagination = "true"
  pg = 1
  doc = htmlParse(GET(url, path = "/jobs/search/", query = list(q = search, where = loc)))

  totalResults = getTotalResults(doc)
  cat("Total results:", totalResults, "\n")
  # get all search results
  allJobListings = list()
  while (length(allJobListings) < totalResults){
    cat("Searching: ", search, "; Scraping page ", pg, "\n")

    urlLink = GET(url, path = p, query = list(q = search, where = loc, intcid = intcid, isDynamicPage = isDynamicPage, isMKPagination = isMKPagination, page = as.character(pg), total = as.character((pg-1)*26)))
    results = content(urlLink, "parsed")
    # get all results in current page
    jobListings = sapply(results, getJobInfo)
    # remove "rows" that contains NA (extracted ads from website)
    checkRows = sapply(jobListings, function(x) x$title)
    allJobListings = c(allJobListings, jobListings[!is.na(checkRows)])

    pg = pg + 1
  }
  
  return(allJobListings)
}

searches = c("data scientist", "data analyst", "statistician")

# obtain all search results
jobListings = sapply(searches, getJobPostings)
print(sapply(jobListings, length)) # DS: 821, DA: 417, S: 3
fullJobListing = c(jobListings[[1]], jobListings[[2]], jobListings[[3]])
monster = data.frame(matrix(unlist(fullJobListing), nrow=length(fullJobListing), byrow=T))
names(monster) = names(fullJobListing[[1]])

# save data frame as RData
save(monster, file = "monster.RData")

# https://www.monster.com/jobs/search/pagination/?q=Data-Scientist&where=california&intcid=skr_navigation_nhpso_searchMain&isDynamicPage=true&isMKPagination=true&page=4&total=78
