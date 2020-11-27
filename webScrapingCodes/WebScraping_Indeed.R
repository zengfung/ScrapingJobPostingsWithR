library(XML)
library(RCurl)
library(httr)

url = "https://www.indeed.com"
p = "jobs"
search = "data scientist"
location = "california"
doc = htmlParse(GET(url, path = p, query = list( q = search, l = location)))

results = getNodeSet(doc, "//div[@class='jobsearch-SerpJobCard unifiedRow row result']")
firstResult = results[[1]]
jobTitle = xmlValue(getNodeSet(firstResult, "./h2/a"), trim = TRUE)
jobCompany = xmlValue(getNodeSet(firstResult, "./div[@class='sjcl']/div/span[@class='company']"))
jobLocation = xmlValue(getNodeSet(firstResult, "./div[@class='sjcl']/div[@class='location accessible-contrast-color-location']"))
jobURL = paste0("https://www.indeed.com", xpathSApply(firstResult, "./h2/a", xmlGetAttr, "href"))
jobLink = htmlParse(GET(jobURL))
jobDescription = xmlValue(getNodeSet(jobLink, "//div[@id='jobDescriptionText']//text()"))

getJobInfo = function(result){
  jobTitle = xmlValue(getNodeSet(result, "./h2/a"), trim = TRUE)
  jobCompany = xmlValue(getNodeSet(result, "./div[@class='sjcl']/div/span[@class='company']"))
  jobLocation = xmlValue(getNodeSet(result, "./div[@class='sjcl']/div[@class='location accessible-contrast-color-location']"))
  jobURL = paste0("https://www.indeed.com", xpathSApply(result, "./h2/a", xmlGetAttr, "href"))
  jobLink = htmlParse(GET(jobURL))
  jobDescription = xmlValue(getNodeSet(jobLink, "//div[@id='jobDescriptionText']//text()"))
  
  return(list(title = jobTitle,
              source = "indeed.com",
              company = jobCompany,
              location = jobLocation,
              description = paste(jobDescription, collapse = "@@"),
              link = jobURL))
  # jobTypeAndSalary = xmlValue(getNodeSet(result, ".//div[@class='details']//div[@class='wage']"))
  # jobSalary = ifelse(jobTypeAndSalary != "Compensation Unspecified",
  #                    regmatches(jobTypeAndSalary, gregexpr("[$].+$", jobTypeAndSalary)[[1]]),
  #                    NA)
  # jobDescription = xmlValue(getNodeSet(result, ".//div[@class='description']"))
  # jobSkills = xmlValue(getNodeSet(result, ".//div[@class='skills']//ul[@class='skill-list']//li[@class='skill-item']//a//span[@class='skill-name']"))
  # jobIntro = xmlValue(getNodeSet(jobLink, "//div[@class='job-details span9']//div[@class='section-data']"))
  # jobDetailsNode = getNodeSet(jobLink, "//div[@class='job-details span9']//div[@class='section-data section-data-title']")
  # jobDetails = lapply(jobDetailsNode, function(node) {
  #   sapply(node[names(node)=="text"], xmlValue, trim = TRUE)
  # })
  # jobDetails = sapply(jobDetails, function(details){
  #   paste(details, collapse="\n")
  # })
  # if (length(jobDetails) > 0){
  #   names(jobDetails) = xmlValue(getNodeSet(jobLink, "//div[@class='job-details span9']//h4[@class='section-title']"))
  # }
  # jobRequirements = ifelse(is.null(jobDetails["What You Need for this Position"][[1]]),
  #                          NA, jobDetails["What You Need for this Position"][[1]])
  # jobResponsibilities = ifelse(is.null(jobDetails["What You Will Be Doing"][[1]]),
  #                              NA, jobDetails["What You Will Be Doing"][[1]])
  # jobBenefits = ifelse(is.null(jobDetails["What's In It for You"][[1]]),
  #                      NA, jobDetails["What's In It for You"][[1]])
  # return (list(title = jobTitle,
  #              source = "cybercoders.com",
  #              company = "CyberCoders",
  #              location = jobLocation,
  #              employmentType = jobType,
  #              salary = jobSalary,
  #              responsibilities = jobResponsibilities,
  #              requiredSkills = jobRequirements,
  #              preferredSkills = paste(jobSkills, collapse = "\n"),
  #              benefits = jobBenefits,
  #              #intro = jobIntro,
  #              link = jobURL,
  #              description = jobDescription))
}

getTotalResults = function(string){
  totalResults = gsub("Page [0-9]+ of ([0-9,]+) jobs", "\\1", string)
  totalResults = as.integer(gsub(",", "", totalResults))
  return (totalResults)
}

# function to get data for different searches
getJobPostings = function(search){
  url = "https://www.indeed.com"
  p = "jobs"
  location = "california"
  doc = htmlParse(GET(url, path = p, query = list( q = search, l = location)))
  totalResults = getTotalResults(xmlValue(getNodeSet(doc, "//div[@id='searchCountPages']"), trim = TRUE))
  
  # get all search results
  allJobListings = list()
  page = 1
  while (length(allJobListings) < totalResults){
    # TODO: Add page number on following code
    doc = htmlParse(GET(url, path = p, query = list( q = search, l = location)))
    results = getNodeSet(doc, "//div[@class='jobsearch-SerpJobCard unifiedRow row result']")
    jobListings = lapply(results, getJobInfo)
    # remove "rows" that contains NA (extracted ads from website)
    checkRows = sapply(jobListings, function(x) x$title)
    allJobListings = c(allJobListings, jobListings[!is.na(checkRows)])
    break
  }
  
  return(allJobListings)
}

searches = c("data scientist", "data analyst", "statistician")

# obtain all search results
jobListings = lapply(searches, getJobPostings)
print(sapply(jobListings, length))
fullJobListing = c(jobListings[[1]], jobListings[[2]], jobListings[[3]])
df = data.frame(matrix(unlist(fullJobListing), nrow=length(fullJobListing), byrow=T))
names(df) = names(fullJobListing[[1]])
