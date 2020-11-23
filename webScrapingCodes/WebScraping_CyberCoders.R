library(XML)
library(xml2)
library(RCurl)

###
#To be deleted after testing
###
# go to page of cybercoders.com for search results
url = "https://www.cybercoders.com/search/"
search = "data scientist"
doc = htmlParse(getForm(url, searchterms = search))

# get all search results
results = getNodeSet(doc, "//div[@class='job-listing-item']")

# test for first result
firstResult = results[[1]]
jobTitle = xmlValue(getNodeSet(firstResult, ".//div[@class='job-title']//a"))
jobURL = paste0("https://www.cybercoders.com", 
                 xpathSApply(firstResult, ".//div[@class='job-title']//a", xmlGetAttr, "href"))
jobLink = htmlParse(getURL(jobURL))
location = xmlValue(getNodeSet(firstResult, ".//div[@class='details']//div[@class='location']"))
jobTypeAndSalary = xmlValue(getNodeSet(firstResult, ".//div[@class='details']//div[@class='wage']"))
jobDescription = xmlValue(getNodeSet(firstResult, ".//div[@class='description']"))
skills = xmlValue(getNodeSet(firstResult, ".//div[@class='skills']//ul[@class='skill-list']//li[@class='skill-item']//a//span[@class='skill-name']"))
jobIntro = xmlValue(getNodeSet(jobLink, "//div[@class='job-details span9']//div[@class='section-data']"))
jobDetails = xmlValue(getNodeSet(jobLink, "//div[@class='job-details span9']//div[@class='section-data section-data-title']"))
names(jobDetails) = xmlValue(getNodeSet(jobLink, "//div[@class='job-details span9']//h4[@class='section-title']"))
###
#END
###


# function to get data
getJobInfo = function(result){
  jobTitle = xmlValue(getNodeSet(result, ".//div[@class='job-title']//a"))
  jobURL = paste0("https://www.cybercoders.com", 
                   xpathSApply(result, ".//div[@class='job-title']//a", xmlGetAttr, "href"))
  jobLink = htmlParse(getURL(jobURL))
  jobLocation = xmlValue(getNodeSet(result, ".//div[@class='details']//div[@class='location']"))
  jobTypeAndSalary = xmlValue(getNodeSet(result, ".//div[@class='details']//div[@class='wage']"))
  jobType = ifelse(jobTypeAndSalary != "Compensation Unspecified", 
                   regmatches(jobTypeAndSalary, gregexpr("^[[:alpha:]-]+", jobTypeAndSalary)),
                   NA)
  jobSalary = ifelse(jobTypeAndSalary != "Compensation Unspecified",
                     regmatches(jobTypeAndSalary, gregexpr("[$].+$", jobTypeAndSalary)),
                     NA)
  jobDescription = xmlValue(getNodeSet(result, ".//div[@class='description']"))
  jobSkills = xmlValue(getNodeSet(result, ".//div[@class='skills']//ul[@class='skill-list']//li[@class='skill-item']//a//span[@class='skill-name']"))
  jobIntro = xmlValue(getNodeSet(jobLink, "//div[@class='job-details span9']//div[@class='section-data']"))
  jobDetails = xmlValue(getNodeSet(jobLink, "//div[@class='job-details span9']//div[@class='section-data section-data-title']"))
  names(jobDetails) = xmlValue(getNodeSet(jobLink, "//div[@class='job-details span9']//h4[@class='section-title']"))
  
  return (list(title = jobTitle,
               link = jobURL,
               location = jobLocation,
               employmentType = jobType,
               salary = jobSalary,
               description = jobDescription,
               preferredSkills = jobSkills,
               intro = jobIntro,
               details = jobDetails,
               source = "CyberCoders"))
}

# function to get data for different searches
getJobPostings = function(search){
  url = "https://www.cybercoders.com/search/"
  doc = htmlParse(getForm(url, searchterms = search))

  # get all search results
  results = getNodeSet(doc, "//div[@class='job-listing-item']")
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
