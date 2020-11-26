library(XML)
library(RCurl)
library(httr)

###
#To be deleted after testing
###
# go to page of cybercoders.com for search results
url = "https://www.monster.com"
p = "/jobs/search/"
search = "data analyst"
doc = htmlParse(GET(url, path = p, query = list(q = search, where = "united states")))

results = getNodeSet(doc, "//section[@class='card-content ']")
firstResult = results[[1]]
jobTitle = xmlValue(getNodeSet(firstResult, ".//div[@class='flex-row']//div[@class='summary']//header[@class='card-header']//h2"), trim = TRUE)
jobLocation = xmlValue(getNodeSet(firstResult, ".//div[@class='flex-row']//div[@class='summary']//div[@class='location']//span[@class='name']"), trim = TRUE)
jobCompany = xmlValue(getNodeSet(firstResult, ".//div[@class='flex-row']//div[@class='summary']//div[@class='company']//span[@class='name']"), trim = TRUE)
jobURL = xpathSApply(firstResult, ".//div[@class='flex-row']//div[@class='summary']//header[@class='card-header']//h2//a", xmlGetAttr, "href")
jobLink = htmlParse(GET(jobURL))
jobDescription = xpathSApply(jobLink, "//div[@class='card-content']", xmlValue)


# function to get data
getJobInfo = function(result){
  jobTitle = xmlValue(getNodeSet(result, ".//div[@class='flex-row']//div[@class='summary']//header[@class='card-header']//h2"), trim = TRUE)
  jobURL = xpathSApply(result, ".//div[@class='flex-row']//div[@class='summary']//header[@class='card-header']//h2//a", xmlGetAttr, "href")
  jobLink = suppressMessages(htmlParse(GET(jobURL)))
  jobLocation = xmlValue(getNodeSet(result, ".//div[@class='flex-row']//div[@class='summary']//div[@class='location']//span[@class='name']"), trim = TRUE)
  jobCompany = xmlValue(getNodeSet(result, ".//div[@class='flex-row']//div[@class='summary']//div[@class='company']//span[@class='name']"), trim = TRUE)
  
  return(list(title = jobTitle,
              source = "monster.com",
              company = jobCompany,
              location = jobLocation,
              link = jobURL))
  # jobTypeAndSalary = xmlValue(getNodeSet(result, ".//div[@class='details']//div[@class='wage']"))
  # jobType = ifelse(jobTypeAndSalary != "Compensation Unspecified", 
  #                  regmatches(jobTypeAndSalary, gregexpr("^[[:alpha:]-]+", jobTypeAndSalary)[[1]]),
  #                  NA)
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

# function to get data for different searches
getJobPostings = function(search){
  url = "https://www.monster.com"
  p = "/jobs/search/"
  doc = htmlParse(GET(url, path = p, query = list(q = search, where = "united states", stpage = "1", page = "10")))
  
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
print(sapply(jobListings, length))
fullJobListing = c(jobListings[[1]], jobListings[[2]], jobListings[[3]])
df = data.frame(matrix(unlist(fullJobListing), nrow=length(fullJobListing), byrow=T))
names(df) = names(fullJobListing[[1]])


