library(XML)
library(httr)
library(RCurl)

###
#To be deleted after testing
###
# go to page of cybercoders.com for search results
url = "https://www.careerbuilder.com"
p = "jobs"
search = "data scientist"
doc = htmlParse(GET(url, path = p, query = list(keywords = search, page_number = "1")))

results = getNodeSet(doc, "//div[@class='data-results-content-parent relative']")
firstResult = results[[1]]
jobTitle = xmlValue(getNodeSet(firstResult, ".//div[@class='col big col-mobile-inline']//div[@class='data-results-title dark-blue-text b']"), trim = TRUE)
primaryResult = xpathSApply(firstResult, ".//div[@class='col big col-mobile-inline']//div[@class='data-details']//*", xmlValue)
jobLocation = primaryResult[2]
jobCompany = primaryResult[1]
jobEmploymentType = primaryResult[3]
jobURL = paste0(url, xpathSApply(firstResult, ".//a[@class='data-results-content block job-listing-item']", xmlGetAttr, "href"))
jobLink = htmlParse(GET(jobURL))
jobDescription = xpathSApply(jobLink, "//div[@class='col big col-mobile-full']//p", xmlValue)
jobPreferredSkills = xpathSApply(jobLink, "//div[@class='bloc']//h4[@class='dark-blue-text pb']/..//div", xmlValue)

# function to get data
getJobInfo = function(result){
  jobTitle = xmlValue(getNodeSet(result, ".//div[@class='col big col-mobile-inline']//div[@class='data-results-title dark-blue-text b']"), trim = TRUE)
  primaryResult = xpathSApply(result, ".//div[@class='col big col-mobile-inline']//div[@class='data-details']//*", xmlValue)
  jobLocation = ifelse(length(primaryResult)==3, primaryResult[2], primaryResult[1])
  jobCompany = ifelse(length(primaryResult)==3, primaryResult[1], NA)
  jobEmploymentType = ifelse(length(primaryResult)==3, primaryResult[3], primaryResult[2])
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

getTotalPages = function(string){
  m = gregexpr("^[0-9,]+", string)
  n = regmatches(string, m)[[1]]
  totalSearches = as.integer(gsub(",", "", n))
  totalPages = (totalSearches %/% 25) + 1
  return (totalPages)
}

# function to get data for different searches
getJobPostings = function(search){
  url = "https://www.careerbuilder.com"
  p = "jobs"
  doc = htmlParse(GET(url, path = p, query = list(keywords = search, page_number = "1")))
  totalPages = getTotalPages(xmlValue(getNodeSet(doc, "//h1[@class='fz1rem']")))
  
  # get all search results
  allJobListings = list()
  for (i in 1:totalPages){
    doc = htmlParse(GET(url, path = p, query = list(keywords = search, page_number = as.character(i))))
    results = getNodeSet(doc, "//div[@class='data-results-content-parent relative']")
    jobListings = lapply(results, getJobInfo)
    # remove "rows" that contains NA (extracted ads from website)
    checkRows = sapply(jobListings, function(x) x$title)
    allJobListings = c(allJobListings, jobListings[!is.na(checkRows)])
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
