library(XML)
library(httr)
library(RCurl)

# function to get data
getJobInfo = function(result){
  jobTitle = xmlValue(getNodeSet(result, ".//div[@class='col big col-mobile-inline']//div[@class='data-results-title dark-blue-text b']"), trim = TRUE)
  primaryResult = xpathSApply(result, ".//div[@class='col big col-mobile-inline']//div[@class='data-details']//*", xmlValue)
  jobLocation = ifelse(length(primaryResult)==3, primaryResult[2], primaryResult[1])
  jobCompany = ifelse(length(primaryResult)==3, primaryResult[1], NA)
  jobEmploymentType = ifelse(length(primaryResult)==3, primaryResult[3], primaryResult[2])
  
  # prevent reCAPTCHA with Sys.sleep
  Sys.sleep(1)
  jobURL = paste0("https://www.careerbuilder.com", 
                  xpathSApply(result, ".//a[@class='data-results-content block job-listing-item']", xmlGetAttr, "href"))
  jobLink = htmlParse(GET(jobURL))
  jobDescription = xpathSApply(jobLink, "//div[@class='col big col-mobile-full']//p", xmlValue)
  jobPreferredSkills = xpathSApply(jobLink, "//div[@class='bloc']//h4[@class='dark-blue-text pb']/..//div", xmlValue)
  
  return(list(title = jobTitle,
              source = "careerbuilder.com",
              company = jobCompany,
              location = jobLocation,
              employmentType = jobEmploymentType,
              preferredSkills = paste(jobPreferredSkills, collapse="@@"),
              description = paste(jobDescription, collapse = "@@"),
              link = jobURL))
}

# function to get data for different searches
getJobPostings = function(search){
  url = "https://www.careerbuilder.com"
  p = "jobs"
  doc = htmlParse(GET(url, path = p, query = list(keywords = search, location = "california")))

  # get all search results
  allJobListings = list()
  nextPageLink = NA # setting up as non-NULL value
  while (!is.null(nextPageLink)){
    results = getNodeSet(doc, "//div[@class='data-results-content-parent relative']")
    jobListings = lapply(results, getJobInfo)
    # remove "rows" that contains NA (extracted ads from website)
    checkRows = sapply(jobListings, function(x) x$title)
    allJobListings = c(allJobListings, jobListings[!is.na(checkRows)])
    
    Sys.sleep(1)
    nextPageLink = xpathSApply(doc, "//div[@id='load_more_jobs']/a", xmlGetAttr, "data-load-more")
    doc = htmlParse(GET("https://careerbuilder.com", path = nextPageLink))
  }
  
  return(allJobListings)
}

searches = c("data scientist", "data analyst", "statistician")

# obtain all search results
jobListings = lapply(searches, getJobPostings)
print(sapply(jobListings, length))
fullJobListing = c(jobListings[[1]], jobListings[[2]], jobListings[[3]])
careerBuilder = data.frame(matrix(unlist(fullJobListing), nrow=length(fullJobListing), byrow=T))
names(careerBuilder) = names(fullJobListing[[1]])

# save data frame as RData file
save(careerBuilder, "careerBuilder.RData")
