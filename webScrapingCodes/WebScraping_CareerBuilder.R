library(XML)
library(xml2)
library(RCurl)

###
#To be deleted after testing
###
# go to page of cybercoders.com for search results
url = "https://www.careerbuilder.com/jobs?utf8=%E2%9C%93&"
search = "data scientist"
doc = htmlParse(getForm(url, keywords = search))
