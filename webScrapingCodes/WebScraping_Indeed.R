library(XML)
library(RCurl)

url = "https://www.indeed.com/jobs"
search = "data scientist"
location = "united states"
doc = htmlParse(getForm(url, q = search, l = location))
