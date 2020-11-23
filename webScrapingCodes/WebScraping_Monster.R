library(XML)
library(RCurl)

###
#To be deleted after testing
###
# go to page of cybercoders.com for search results
url = "https://www.monster.com/"
search = "data scientist"
doc = htmlParse(getForm(url, q = search))
