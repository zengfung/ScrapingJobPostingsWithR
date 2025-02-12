library(XML)
library(httr)
library(RCurl)

# function to get data
getJobInfo = function(result){
  jobTitle = xmlValue(getNodeSet(result, ".//div[@class='job-title']//a"))
  jobURL = paste0("https://www.cybercoders.com", 
                   xpathSApply(result, ".//div[@class='job-title']//a", xmlGetAttr, "href"))
  jobLink = htmlParse(getURL(jobURL))
  jobLocation = xmlValue(getNodeSet(result, ".//div[@class='details']//div[@class='location']"))
  jobTypeAndSalary = xmlValue(getNodeSet(result, ".//div[@class='details']//div[@class='wage']"))
  jobType = ifelse(jobTypeAndSalary != "Compensation Unspecified", 
                   regmatches(jobTypeAndSalary, gregexpr("^[[:alpha:]-]+", jobTypeAndSalary)[[1]]),
                   NA)
  jobSalary = ifelse(jobTypeAndSalary != "Compensation Unspecified",
                     regmatches(jobTypeAndSalary, gregexpr("[$].+$", jobTypeAndSalary)[[1]]),
                     NA)
  jobDescription = xmlValue(getNodeSet(result, ".//div[@class='description']"))
  jobSkills = xmlValue(getNodeSet(result, ".//div[@class='skills']//ul[@class='skill-list']//li[@class='skill-item']//a//span[@class='skill-name']"))
  jobIntro = xmlValue(getNodeSet(jobLink, "//div[@class='job-details span9']//div[@class='section-data']"))
  jobDetailsNode = getNodeSet(jobLink, "//div[@class='job-details span9']//div[@class='section-data section-data-title']")
  jobDetails = lapply(jobDetailsNode, function(node) {
    sapply(node[names(node)=="text"], xmlValue, trim = TRUE)
  })
  jobDetails = sapply(jobDetails, function(details){
    paste(details, collapse="@@")
  })
  if (length(jobDetails) > 0){
    names(jobDetails) = xmlValue(getNodeSet(jobLink, "//div[@class='job-details span9']//h4[@class='section-title']"))
  }
  jobRequirements = ifelse(is.null(jobDetails["What You Need for this Position"][[1]]),
                           NA, jobDetails["What You Need for this Position"][[1]])
  jobResponsibilities = ifelse(is.null(jobDetails["What You Will Be Doing"][[1]]),
                               NA, jobDetails["What You Will Be Doing"][[1]])
  jobBenefits = ifelse(is.null(jobDetails["What's In It for You"][[1]]),
                       NA, jobDetails["What's In It for You"][[1]])
  return (list(title = jobTitle,
               source = "cybercoders.com",
               company = "CyberCoders",
               location = jobLocation,
               employmentType = jobType,
               salary = jobSalary,
               responsibilities = jobResponsibilities,
               requiredSkills = jobRequirements,
               preferredSkills = paste(jobSkills, collapse = "@@"),
               benefits = jobBenefits,
               #intro = jobIntro,
               link = jobURL,
               description = jobDescription))
}

# function to get data for different searches
getJobPostings = function(search){
  url = "https://www.cybercoders.com/search/"
  doc = htmlParse(getForm(url, searchterms = search))

  # get all search results
  allJobListings = list()
  nextPageLink = NA # setting up as non-NULL value
  page = 1
  while (!is.null(nextPageLink)){
    page = page + 1
    
    # get all results in current page
    results = getNodeSet(doc, "//div[@class='job-listing-item']")
    jobListings = lapply(results, getJobInfo)
    # remove "rows" that contains NA (extracted ads from website)
    checkRows = sapply(jobListings, function(x) x$title)
    allJobListings = c(allJobListings, jobListings[!is.na(checkRows)])
    
    nextPageLink = xpathSApply(doc, "//ul[@class='pager']/li[@class='lnk-next pager-item ']/a[@rel='next']", xmlGetAttr, "href")
    doc = htmlParse(GET(paste0(url, nextPageLink)))
  }
  
  return(allJobListings)
}

searches = c("data scientist", "data analyst", "statistician")

# obtain all search results
jobListings = sapply(searches, getJobPostings)
print(sapply(jobListings, length)) # DS: 16, DA: 26, S: 0
fullJobListing = c(jobListings[[1]], jobListings[[2]], jobListings[[3]])
cyberCoders = data.frame(matrix(unlist(fullJobListing), nrow=length(fullJobListing), byrow=T))
names(cyberCoders) = names(fullJobListing[[1]])

# save data frame as RData
save(cyberCoders, file = "cyberCoders.RData")
