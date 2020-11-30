library(XML)
library(RCurl)
library(httr)

url = "https://www.indeed.com"
p = "jobs"
search = "statistician"
location = "california"
doc = htmlParse(GET(url, path = p, query = list( q = search, l = location), config(cookie = siteCookie)))

results = getNodeSet(doc, "//div[@class='jobsearch-SerpJobCard unifiedRow row result']")
firstResult = results[[3]]
jobTitle = xmlValue(getNodeSet(firstResult, "./h2/a"), trim = TRUE)
jobCompany = xmlValue(getNodeSet(firstResult, "./div[@class='sjcl']/div/span[@class='company']"), trim = TRUE)
jobLocation = xmlValue(getNodeSet(firstResult, "./div[@class='sjcl']/*[@class='location accessible-contrast-color-location']"))
jobURL = paste0("https://www.indeed.com", xpathSApply(firstResult, "./h2/a", xmlGetAttr, "href"))
jobLink = htmlParse(GET(jobURL, config(cookie = siteCookie)))
jobDescription = xmlValue(getNodeSet(jobLink, "//div[@id='jobDescriptionText']//text()"))

getJobInfo = function(result){
  jobTitle = xmlValue(getNodeSet(result, "./h2/a"), trim = TRUE)
  jobCompany = xmlValue(getNodeSet(result, "./div[@class='sjcl']/div/span[@class='company']"), trim = TRUE)
  jobLocation = xmlValue(getNodeSet(result, "./div[@class='sjcl']/*[@class='location accessible-contrast-color-location']"))
  
  # Sys.sleep before parsing new page to avoid reCAPTCHA
  Sys.sleep(runif(1, min = 7, max = 10))
  jobURL = paste0("https://www.indeed.com", xpathSApply(result, "./h2/a", xmlGetAttr, "href"))
  jobLink = htmlParse(GET(jobURL, config(cookie = siteCookie)))
  jobDescription = xmlValue(getNodeSet(jobLink, "//div[@id='jobDescriptionText']//text()"))
  
  return(list(title = jobTitle,
              source = "indeed.com",
              company = jobCompany,
              location = jobLocation,
              description = paste(jobDescription, collapse = "@@"),
              link = jobURL))
}

# function to get data for different searches
getJobPostings = function(search){
  url = "https://www.indeed.com"
  p = "jobs"
  location = "sacramento"
  doc = htmlParse(GET(url, path = p, query = list( q = search, l = location), config(cookie = siteCookie)))

  # get all search results
  allJobListings = list()
  nextPageLink = NA # setting up as non-NULL value
  page = 1
  while (!is.null(nextPageLink)){
    cat("Searching: ", search, "; Scraping page ", page, "\n")
    print(nextPageLink)
    page = page + 1
    
    # get all results in current page
    results = getNodeSet(doc, "//div[@class='jobsearch-SerpJobCard unifiedRow row result']")
    jobListings = lapply(results, getJobInfo)
    # remove "rows" that contains NA (extracted ads from website)
    checkRows = sapply(jobListings, function(x) x$title)
    allJobListings = c(allJobListings, jobListings[!is.na(checkRows)])
    
    Sys.sleep(runif(1, min = 7, max = 10))
    nextPageLink = xpathSApply(doc, "//ul[@class='pagination-list']/li/a[@aria-label='Next']", xmlGetAttr, "href")
    doc = htmlParse(GET(paste0("https://indeed.com",nextPageLink), config(cookie = siteCookie)))
  }
  
  return(allJobListings)
}

searches = c("statistician", "data analyst", "data scientist")

# obtain all search results
siteCookie = '_ga=GA1.2.418213468.1577366682; SESSION_START_TIME=1577366821595; SESSION_ID=1dt1676mrbjae801; SESSION_END_TIME=1577366943383; _gcl_au=1.1.1664612506.1597536843; pjps=1; cmppmeta=eNoBWACn/7NXV4dfaUWJsxL/J/4G2kRlVSPvfVwy8TVxG/LoDcjWrmMwvQGT/NV6uB5lfvc/Ja0vR/BNbQNLWlAGLqCMkuMlU6exvtuHNLW53KOlx6DJ7C2Mw+uVc5tpGS0B; PREF="TM=1597888151107:L=California"; mp_f2bdd96b68c5d2bc9fcc1e87f69dab44_mixpanel=%7B%22distinct_id%22%3A%20%221dt162ugcf7lg800%22%2C%22%24device_id%22%3A%20%22173f4a1765bc3-0ed740483e2d76-3323767-144000-173f4a1765c33e%22%2C%22%24initial_referrer%22%3A%20%22https%3A%2F%2Fapply.indeed.com%2Findeedapply%2Fxpc%3Fv%3D5%22%2C%22%24initial_referring_domain%22%3A%20%22apply.indeed.com%22%2C%22%24user_id%22%3A%20%221dt162ugcf7lg800%22%7D; CTK=1dt162ugcf7lg800; g_state={"i_p":1600100719085,"i_l":3}; __ssid=8db230012b0ac1e1c17638d9e7acf78; SOCK="6xLfWHwDgg5tb46jt7aITbP6-ds="; SHOE="yCB8-Q28ONSCp0SsxzhIsyzbG3g5E0EQlA__8Av3cPP__RbFu8x7UJ8EN0UzwGDdHCYMQDE2FOJcBDJg6lYnSeTwcJarO3fzxIyq-V0284X8ZuBNiv3DA2SPrAbuRcHwQpJUaexs4uRwLONA9yHW3GY="; indeed_rcc="PREF:cmppmeta:LV:CTK:UD:RQ"; jasxMarvin=1; LC="co=US&hl=en_US"; _hjid=2ff2526e-34f6-4ebe-b099-9d6e09566222; CTK=1enndq436o14o800; prforceGroups=#A3:careerguide_article_inline_cta_tst3,#A5:coverletter_sample_hp_video_tst0,#A11:resume_sample_hp_video_tst1,#A7:careerguide_onetap_tst2,#A4:careerguide_resumereview_cta_tst2,#A5:careerguide_displayads_tst1,#A5:careerguide_homepage_displayads_tst1,#A5:careerguide_careerdev_feedback_tst0,careerguide_article_inline_cta_tst3,coverletter_sample_hp_video_tst0,resume_sample_hp_video_tst1,careerguide_onetap_tst2,careerguide_resumereview_cta_tst2,careerguide_displayads_tst1,careerguide_homepage_displayads_tst1,careerguide_careerdev_feedback_tst0; INDEED_CSRF_TOKEN=bmwXdNUJqdLYjGc85QAXcLXopWT3G1dX; UD="LA=1606392026:LV=1606258920:CV=1606384111:TS=1577366943"; LV="LA=1606392026:LV=1606258920:CV=1606384111:TS=1577366943"; CSRF=DiiI2qagYB4LFcUJObceaXbTYyGeT72h; _gid=GA1.2.1593538871.1606444583; PPN=1; MICRO_CONTENT_CSRF_TOKEN=BN2vdtrV1LUMWy9MKlHMFqEht2yTy5Ih; jobAlertPopoverShown=1; PPID=eyJraWQiOiI5MmMyZGZkNS0xZGIzLTRmY2YtODZlNS00YzJjMTk1ODE5MDgiLCJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJzdWIiOiJjZWZjZjNkM2E4NGViM2RmIiwiYXVkIjoiYzFhYjhmMDRmIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImF1dGgiOiJnb29nbGUiLCJjcmVhdGVkIjoxNTk5NDk1OTQzMDAwLCJyZW1fbWUiOnRydWUsImlzcyI6Imh0dHBzOlwvXC9zZWN1cmUuaW5kZWVkLmNvbSIsImV4cCI6MTYwNjQ1Njk4NywiaWF0IjoxNjA2NDU1MTg3LCJsb2dfdHMiOjE1OTk3NTQwMjUsImVtYWlsIjoiemVuZ2Z1bmcubGlld0BnbWFpbC5jb20ifQ.Npn7_GuL2DUXjwZC7fcqUdTH5DEYERqvmzCGJHTjcUQAgsvsy9EevyCUU879kjT4xkqrBgHBRTLHX4XLia59Ow; s_seven=jD4tpwBxr10Fy6QKhVjzqBHUJ5jowuRiVxegFvbXVdo; RQ="q=statistician&l=California&ts=1606455617155&pts=1606446000595:q=data+scientist&l=California&ts=1606444592707:q=Data+Science&l=California&ts=1606097666496&pts=1604425532687:q=Curai&l=California&ts=1602180883260:q=Data&l=California&ts=1602091847548:q=Analytics+Consultant&l=California&ts=1601588052576:q=Statistical+Consultant&l=California&ts=1601499338734:q=Data+Analyst&l=California&ts=1601490064148:q=Data+Analyst+Intern&l=California&ts=1601047480169:q=Data+Science+Intern&l=California&ts=1600826719711"; jaSerpCount=6; JSESSIONID=DCBC9CE86470998DA0556F2751B43CB5'
jobListings = lapply(searches, getJobPostings)
print(sapply(jobListings, length)) # DS: 44, DA: 108, S: 6
fullJobListing = c(jobListings[[1]], jobListings[[2]], jobListings[[3]])
indeed = data.frame(matrix(unlist(fullJobListing), nrow=length(fullJobListing), byrow=T))
names(indeed) = names(fullJobListing[[1]])

# save as RData file
save(indeed, file = "indeed.RData")
# cookie
# _ga=GA1.2.418213468.1577366682; SESSION_START_TIME=1577366821595; SESSION_ID=1dt1676mrbjae801; SESSION_END_TIME=1577366943383; _gcl_au=1.1.1664612506.1597536843; pjps=1; cmppmeta=eNoBWACn/7NXV4dfaUWJsxL/J/4G2kRlVSPvfVwy8TVxG/LoDcjWrmMwvQGT/NV6uB5lfvc/Ja0vR/BNbQNLWlAGLqCMkuMlU6exvtuHNLW53KOlx6DJ7C2Mw+uVc5tpGS0B; PREF="TM=1597888151107:L=California"; mp_f2bdd96b68c5d2bc9fcc1e87f69dab44_mixpanel=%7B%22distinct_id%22%3A%20%221dt162ugcf7lg800%22%2C%22%24device_id%22%3A%20%22173f4a1765bc3-0ed740483e2d76-3323767-144000-173f4a1765c33e%22%2C%22%24initial_referrer%22%3A%20%22https%3A%2F%2Fapply.indeed.com%2Findeedapply%2Fxpc%3Fv%3D5%22%2C%22%24initial_referring_domain%22%3A%20%22apply.indeed.com%22%2C%22%24user_id%22%3A%20%221dt162ugcf7lg800%22%7D; CTK=1dt162ugcf7lg800; g_state={"i_p":1600100719085,"i_l":3}; __ssid=8db230012b0ac1e1c17638d9e7acf78; SOCK="6xLfWHwDgg5tb46jt7aITbP6-ds="; SHOE="yCB8-Q28ONSCp0SsxzhIsyzbG3g5E0EQlA__8Av3cPP__RbFu8x7UJ8EN0UzwGDdHCYMQDE2FOJcBDJg6lYnSeTwcJarO3fzxIyq-V0284X8ZuBNiv3DA2SPrAbuRcHwQpJUaexs4uRwLONA9yHW3GY="; indeed_rcc="PREF:cmppmeta:LV:CTK:UD:RQ"; jasxMarvin=1; LC="co=US&hl=en_US"; _hjid=2ff2526e-34f6-4ebe-b099-9d6e09566222; CTK=1enndq436o14o800; prforceGroups=#A3:careerguide_article_inline_cta_tst3,#A5:coverletter_sample_hp_video_tst0,#A11:resume_sample_hp_video_tst1,#A7:careerguide_onetap_tst2,#A4:careerguide_resumereview_cta_tst2,#A5:careerguide_displayads_tst1,#A5:careerguide_homepage_displayads_tst1,#A5:careerguide_careerdev_feedback_tst0,careerguide_article_inline_cta_tst3,coverletter_sample_hp_video_tst0,resume_sample_hp_video_tst1,careerguide_onetap_tst2,careerguide_resumereview_cta_tst2,careerguide_displayads_tst1,careerguide_homepage_displayads_tst1,careerguide_careerdev_feedback_tst0; INDEED_CSRF_TOKEN=bmwXdNUJqdLYjGc85QAXcLXopWT3G1dX; UD="LA=1606392026:LV=1606258920:CV=1606384111:TS=1577366943"; LV="LA=1606392026:LV=1606258920:CV=1606384111:TS=1577366943"; CSRF=DiiI2qagYB4LFcUJObceaXbTYyGeT72h; _gid=GA1.2.1593538871.1606444583; PPN=1; MICRO_CONTENT_CSRF_TOKEN=BN2vdtrV1LUMWy9MKlHMFqEht2yTy5Ih; jobAlertPopoverShown=1; RQ="q=statistician&l=California&ts=1606446000595:q=data+scientist&l=California&ts=1606444592707:q=Data+Science&l=California&ts=1606097666496&pts=1604425532687:q=Curai&l=California&ts=1602180883260:q=Data&l=California&ts=1602091847548:q=Analytics+Consultant&l=California&ts=1601588052576:q=Statistical+Consultant&l=California&ts=1601499338734:q=Data+Analyst&l=California&ts=1601490064148:q=Data+Analyst+Intern&l=California&ts=1601047480169:q=Data+Science+Intern&l=California&ts=1600826719711"; jaSerpCount=3; PPID=eyJraWQiOiI5MmMyZGZkNS0xZGIzLTRmY2YtODZlNS00YzJjMTk1ODE5MDgiLCJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJzdWIiOiJjZWZjZjNkM2E4NGViM2RmIiwiYXVkIjoiYzFhYjhmMDRmIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImF1dGgiOiJnb29nbGUiLCJjcmVhdGVkIjoxNTk5NDk1OTQzMDAwLCJyZW1fbWUiOnRydWUsImlzcyI6Imh0dHBzOlwvXC9zZWN1cmUuaW5kZWVkLmNvbSIsImV4cCI6MTYwNjQ1NDc3MCwiaWF0IjoxNjA2NDUyOTcwLCJsb2dfdHMiOjE1OTk3NTQwMjUsImVtYWlsIjoiemVuZ2Z1bmcubGlld0BnbWFpbC5jb20ifQ.D9y3nWTtkqLH7HkC7xqLY04Lb3RJ7YYmIV482TJsTYeXQx1oAQMFn6_u5OMWR9UFqY0aqXnhB_qCcOD0-xHOTQ; JSESSIONID=B9FD26A95D92DDB958BCCA3DDC8CA099
# _ga=GA1.2.418213468.1577366682; SESSION_START_TIME=1577366821595; SESSION_ID=1dt1676mrbjae801; SESSION_END_TIME=1577366943383; _gcl_au=1.1.1664612506.1597536843; pjps=1; cmppmeta=eNoBWACn/7NXV4dfaUWJsxL/J/4G2kRlVSPvfVwy8TVxG/LoDcjWrmMwvQGT/NV6uB5lfvc/Ja0vR/BNbQNLWlAGLqCMkuMlU6exvtuHNLW53KOlx6DJ7C2Mw+uVc5tpGS0B; PREF="TM=1597888151107:L=California"; mp_f2bdd96b68c5d2bc9fcc1e87f69dab44_mixpanel=%7B%22distinct_id%22%3A%20%221dt162ugcf7lg800%22%2C%22%24device_id%22%3A%20%22173f4a1765bc3-0ed740483e2d76-3323767-144000-173f4a1765c33e%22%2C%22%24initial_referrer%22%3A%20%22https%3A%2F%2Fapply.indeed.com%2Findeedapply%2Fxpc%3Fv%3D5%22%2C%22%24initial_referring_domain%22%3A%20%22apply.indeed.com%22%2C%22%24user_id%22%3A%20%221dt162ugcf7lg800%22%7D; CTK=1dt162ugcf7lg800; g_state={"i_p":1600100719085,"i_l":3}; __ssid=8db230012b0ac1e1c17638d9e7acf78; SOCK="6xLfWHwDgg5tb46jt7aITbP6-ds="; SHOE="yCB8-Q28ONSCp0SsxzhIsyzbG3g5E0EQlA__8Av3cPP__RbFu8x7UJ8EN0UzwGDdHCYMQDE2FOJcBDJg6lYnSeTwcJarO3fzxIyq-V0284X8ZuBNiv3DA2SPrAbuRcHwQpJUaexs4uRwLONA9yHW3GY="; indeed_rcc="PREF:cmppmeta:LV:CTK:UD:RQ"; jasxMarvin=1; LC="co=US&hl=en_US"; _hjid=2ff2526e-34f6-4ebe-b099-9d6e09566222; CTK=1enndq436o14o800; prforceGroups=#A3:careerguide_article_inline_cta_tst3,#A5:coverletter_sample_hp_video_tst0,#A11:resume_sample_hp_video_tst1,#A7:careerguide_onetap_tst2,#A4:careerguide_resumereview_cta_tst2,#A5:careerguide_displayads_tst1,#A5:careerguide_homepage_displayads_tst1,#A5:careerguide_careerdev_feedback_tst0,careerguide_article_inline_cta_tst3,coverletter_sample_hp_video_tst0,resume_sample_hp_video_tst1,careerguide_onetap_tst2,careerguide_resumereview_cta_tst2,careerguide_displayads_tst1,careerguide_homepage_displayads_tst1,careerguide_careerdev_feedback_tst0; INDEED_CSRF_TOKEN=bmwXdNUJqdLYjGc85QAXcLXopWT3G1dX; UD="LA=1606392026:LV=1606258920:CV=1606384111:TS=1577366943"; LV="LA=1606392026:LV=1606258920:CV=1606384111:TS=1577366943"; CSRF=DiiI2qagYB4LFcUJObceaXbTYyGeT72h; _gid=GA1.2.1593538871.1606444583; PPN=1; MICRO_CONTENT_CSRF_TOKEN=BN2vdtrV1LUMWy9MKlHMFqEht2yTy5Ih; jobAlertPopoverShown=1; PPID=eyJraWQiOiI5MmMyZGZkNS0xZGIzLTRmY2YtODZlNS00YzJjMTk1ODE5MDgiLCJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJzdWIiOiJjZWZjZjNkM2E4NGViM2RmIiwiYXVkIjoiYzFhYjhmMDRmIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImF1dGgiOiJnb29nbGUiLCJjcmVhdGVkIjoxNTk5NDk1OTQzMDAwLCJyZW1fbWUiOnRydWUsImlzcyI6Imh0dHBzOlwvXC9zZWN1cmUuaW5kZWVkLmNvbSIsImV4cCI6MTYwNjQ1Njk4NywiaWF0IjoxNjA2NDU1MTg3LCJsb2dfdHMiOjE1OTk3NTQwMjUsImVtYWlsIjoiemVuZ2Z1bmcubGlld0BnbWFpbC5jb20ifQ.Npn7_GuL2DUXjwZC7fcqUdTH5DEYERqvmzCGJHTjcUQAgsvsy9EevyCUU879kjT4xkqrBgHBRTLHX4XLia59Ow; s_seven=jD4tpwBxr10Fy6QKhVjzqBHUJ5jowuRiVxegFvbXVdo; RQ="q=statistician&l=California&ts=1606455617155&pts=1606446000595:q=data+scientist&l=California&ts=1606444592707:q=Data+Science&l=California&ts=1606097666496&pts=1604425532687:q=Curai&l=California&ts=1602180883260:q=Data&l=California&ts=1602091847548:q=Analytics+Consultant&l=California&ts=1601588052576:q=Statistical+Consultant&l=California&ts=1601499338734:q=Data+Analyst&l=California&ts=1601490064148:q=Data+Analyst+Intern&l=California&ts=1601047480169:q=Data+Science+Intern&l=California&ts=1600826719711"; jaSerpCount=6; JSESSIONID=DCBC9CE86470998DA0556F2751B43CB5
