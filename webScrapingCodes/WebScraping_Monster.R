library(XML)
library(RCurl)
library(httr)

# function to get data
getJobInfo = function(result){
  jobTitle = xmlValue(getNodeSet(result, ".//div[@class='flex-row']//div[@class='summary']//header[@class='card-header']//h2"), trim = TRUE)
  jobLocation = xmlValue(getNodeSet(result, ".//div[@class='flex-row']//div[@class='summary']//div[@class='location']//span[@class='name']"), trim = TRUE)
  jobCompany = xmlValue(getNodeSet(result, ".//div[@class='flex-row']//div[@class='summary']//div[@class='company']//span[@class='name']"), trim = TRUE)

  # prevent reCAPTCHA with Sys.sleep
  Sys.sleep(1)
  jobURL = xpathSApply(result, ".//div[@class='flex-row']//div[@class='summary']//header[@class='card-header']//h2//a", xmlGetAttr, "href")
  jobLink = suppressMessages(htmlParse(GET(jobURL)))
  jobDescription = xpathSApply(jobLink, "//div[@name='value_description']//text()", xmlValue, trim = TRUE)
  
  return(list(title = jobTitle,
              source = "monster.com",
              company = jobCompany,
              location = jobLocation,
              description = paste(jobDescription, collapse = "@@"),
              link = jobURL))
}

# function to get data for different searches
getJobPostings = function(search){
  url = "https://www.monster.com"
  p = "/jobs/search/"
  doc = htmlParse(GET(url, path = p, query = list(q = search, where = "california", stpage = "1", page = "10")))

  # get all search results
  results = getNodeSet(doc, "//section[@class='card-content ']")
  jobListings = lapply(results, getJobInfo)
  # remove "rows" that contains NA (extracted ads from website)
  checkRows = sapply(jobListings, function(x) x$title)
  
  return(jobListings[!is.na(checkRows)])
}

searches = c("data scientist", "data analyst", "statistician")

# obtain all search results
jobListings = sapply(searches, getJobPostings)
print(sapply(jobListings, length)) # DS: 252, DA: 258, S: 22
fullJobListing = c(jobListings[[1]], jobListings[[2]], jobListings[[3]])
monster = data.frame(matrix(unlist(fullJobListing), nrow=length(fullJobListing), byrow=T))
names(monster) = names(fullJobListing[[1]])

# save data frame as RData
save(monster, file = "monster.RData")
