## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------------------------------------------------------------------------------
load("cyberCoders.RData")

# minimum education level
minEducationLevel = factor(sapply(cyberCoders$requiredSkills, function(desc){
  if (grepl("high school diploma", desc, ignore.case = TRUE) == TRUE) {
    return ("High School")
  } else if (grepl("(\\<A[.]?A\\>|\\<A[.]?S\\>|Associate.?s degree)", desc) == TRUE){
    return ("Associate's")
  } else if (grepl("(\\<B[.]?A\\>|\\<B[.]?S\\>|Bachelor[']?s?)", desc) == TRUE){
    return ("Bachelor's")
  } else if (grepl("(M[.]?S\\>|Master.?s|Advanced [Dd]egree)", desc) == TRUE) {
    return ("Master's")
  } else if (grepl("(\\<Ph[.]?[Dd]|[Dd]octoral[ [Dd]egree]?)", desc) == TRUE) {
    return ("Ph.D")
  } else {
    return ("None")
  }
}), levels = c("None", "High School", "Associate's", "Bachelor's", "Master's", "Ph.D"))

# location
city = trimws(gsub("([[:alpha:] ]+),([[:space:][:alpha:]]+)", "\\1", cyberCoders$location))
state = trimws(gsub("([[:alpha:] ]+),([[:space:][:alpha:]]+)", "\\2", cyberCoders$location))

# employment type
employmentTypeRegex = "(full[- ]?time|part[- ]time|hourly|contract|short[- ]term|month)"
getEmploymentType = function(employmentType, description){
  if (!is.na(employmentType)){
    return (employmentType)
  } else{
    if (grepl(employmentTypeRegex, description, ignore.case = TRUE) == TRUE){
      m = regexpr(employmentTypeRegex, description, ignore.case = TRUE)
      result = regmatches(description, m)
      employmentType = switch(tolower(result),
                              "full-time" = "Full-time",
                              "full time" = "Full-time",
                              "part-time" = "Part-time",
                              "part time" = "Part-time",
                              "hourly" = "Hourly",
                              "contract" = "Contract",
                              "short-term" = "Short-term",
                              "short term" = "Short-term",
                              "month" = "Contract")
      return (employmentType)
    } else{
      return (NA)
    }
  }
}
employmentType = mapply(getEmploymentType, cyberCoders$employmentType, cyberCoders$description)

# forming cleaned data frame
cyberCoders_df = data.frame(Position = cyberCoders$title, 
                            Company = cyberCoders$company,
                            City = city,
                            State = state,
                            EmploymentType = employmentType,
                            MinEducationLevel = minEducationLevel,
                            Salary = cyberCoders$salary,
                            Link = cyberCoders$link)
head(cyberCoders_df)


## ----------------------------------------------------------------------------------------------------------------------------------
stopWords = tm::stopwords("en")
# required skills
reqSkills = tolower(unlist(strsplit(cyberCoders$requiredSkills, "@@")))
reqSkills_keywords = unlist(lapply(reqSkills, function(x) {
  m = gregexpr("([[:alnum:]]+[[:punct:]][[:alnum:]]+|[[:alnum:]]+)", x)
  results = regmatches(x, m)
  return (results)
}))
reqSkills_keywords = reqSkills_keywords[!(reqSkills_keywords %in% stopWords)]
reqSkills_table = sort(table(reqSkills_keywords), decreasing = TRUE)
topReqSkills = reqSkills_table[c("python", "sql", "programming", "r", "spark")]
names(topReqSkills) = c("Python", "SQL", "Programming", "R", "Spark")
barplot(topReqSkills, main = "Top Required Skills", xlab = "Skills")

# minimum required education level
minEducationLevel_table = table(minEducationLevel)
barplot(minEducationLevel_table, main = "Minimum Education Level Requirement", xlab = "Education Level", cex.names = 0.7)

# top degree fields
topFields = reqSkills_table[c("science", "math", "marketing", "business", "bioinformatics", "computer", "statistics")]
names(topFields) = c("Data Science", "Math", "Marketing", "Business", "Bioinformatics", "Comp. Science", "Statistics")
barplot(topFields, main = "Top Degree Fields", xlab = "Fields", cex.names = 0.7)


## ----------------------------------------------------------------------------------------------------------------------------------
# employment types
barplot(table(employmentType), main = "Employment Type", xlab = "Employment Type")

# location
par(mfrow = c(1,2))
barplot(sort(table(city), decreasing = TRUE)[1:5], main = "Top 5 Cities", xlab = "City", cex.names = 0.4)
barplot(sort(table(state), decreasing = TRUE)[1:5], main = "Top 5 States", xlab = "State")

# top preferred skills
par(mfrow = c(1,1))
prefSkills = unlist(strsplit(cyberCoders$preferredSkills, "@@"))
prefSkills_table = sort(table(prefSkills), decreasing = TRUE)
topPrefSkills = prefSkills_table[1:5]
barplot(topPrefSkills, main = "Top Preferred Skills", xlab = "Skills")


## ----------------------------------------------------------------------------------------------------------------------------------
load("careerBuilder.RData")

# minimum education level
minEducationLevel = factor(sapply(careerBuilder$description, function(desc){
  if (grepl("high school diploma", desc, ignore.case = TRUE) == TRUE) {
    return ("High School")
  } else if (grepl("(\\<A[.]?A\\>|\\<A[.]?S\\>|Associate.?s degree)", desc) == TRUE){
    return ("Associate's")
  } else if (grepl("(\\<B[.]?A\\>|\\<B[.]?S\\>|Bachelor[']?s?)", desc) == TRUE){
    return ("Bachelor's")
  } else if (grepl("(M[.]?S\\>|Master.?s|Advanced [Dd]egree)", desc) == TRUE) {
    return ("Master's")
  } else if (grepl("(\\<Ph[.]?[Dd]|[Dd]octoral[ [Dd]egree]?)", desc) == TRUE) {
    return ("Ph.D")
  } else {
    return ("None")
  }
}), levels = c("None", "High School", "Associate's", "Bachelor's", "Master's", "Ph.D"))

# location
city = trimws(gsub("([[:alpha:] ]+)-([[:space:][:alpha:]]+)", "\\2", careerBuilder$location))
state = trimws(gsub("([[:alpha:] ]+)-([[:space:][:alpha:]]+)", "\\1", careerBuilder$location))

# employment type
employmentType = sapply(careerBuilder$employmentType, function(x) switch(x,
                        "Contract to Hire" = "Contract",
                        "Contractor" = "Contract",
                        "Full-Time" = "Full-time",
                        "Full-Time/Part-Time" = "Full/Part-time",
                        "Intern" = "Intern",
                        "Not Specified" = "NA",
                        "Part-Time" = "Part-time",
                        "Per Diem" = "Hourly",
                        "Seasonal/Temp" = "Short-term"))

# cleaned data frame
careerBuilder_df = data.frame(Title = careerBuilder$title,
                              Company = careerBuilder$company,
                              City = city,
                              State = state,
                              EmploymentType = employmentType,
                              MinEducationLevel = minEducationLevel,
                              Link = careerBuilder$link)
head(careerBuilder_df)


## ----------------------------------------------------------------------------------------------------------------------------------
# required skills
reqSkills = tolower(unlist(strsplit(careerBuilder$description, "@@")))
reqSkills_keywords = unlist(lapply(reqSkills, function(x) {
  m = gregexpr("([[:alnum:]]+[[:punct:]][[:alnum:]]+|[[:alnum:]]+)", x)
  results = regmatches(x, m)
  return (results)
}))
reqSkills_keywords = reqSkills_keywords[!(reqSkills_keywords %in% stopWords)]
reqSkills_table = sort(table(reqSkills_keywords), decreasing = TRUE)
topReqSkills = reqSkills_table[c("business", "analysis", "clinical", "testing", "python")]
names(topReqSkills) = c("Business", "Analysis", "Clinical", "Testing", "Python")
barplot(topReqSkills, main = "Top Required Skills", xlab = "Skills")

# minimum required education level
minEducationLevel_table = table(minEducationLevel)
barplot(minEducationLevel_table[2:6], main = "Minimum Education Level Requirement", xlab = "Education Level", cex.names = 0.7)

# top degree fields
topFields = reqSkills_table[c("business", "science", "marketing", "engineering", "healthcare")]
names(topFields) = c("Business", "Data/Comp. Sc.", "Marketing", "Engineering", "Healthcare")
barplot(topFields, main = "Top Degree Fields", xlab = "Fields", cex.names = 0.7)


## ----------------------------------------------------------------------------------------------------------------------------------
# employment types
barplot(table(employmentType), main = "Employment Type", xlab = "Employment Type", cex.names = 0.7)

# location
barplot(sort(table(city), decreasing = TRUE)[1:5], main = "Top 5 Cities", xlab = "City", cex.names = 0.8)

# top preferred skills
prefSkills = unlist(strsplit(careerBuilder$preferredSkills, "@@"))
prefSkills_table = sort(table(prefSkills), decreasing = TRUE)
topPrefSkills = prefSkills_table[1:5]
barplot(topPrefSkills, main = "Top Preferred Skills", xlab = "Skills")


## ----------------------------------------------------------------------------------------------------------------------------------
load("monster.RData")

# minimum education level
minEducationLevel = factor(sapply(monster$description, function(desc){
  if (grepl("high school diploma", desc, ignore.case = TRUE) == TRUE) {
    return ("High School")
  } else if (grepl("(\\<A[.]?A\\>|\\<A[.]?S\\>|Associate.?s degree)", desc) == TRUE){
    return ("Associate's")
  } else if (grepl("(\\<B[.]?A\\>|\\<B[.]?S\\>|Bachelor[']?s?)", desc) == TRUE){
    return ("Bachelor's")
  } else if (grepl("(M[.]?S\\>|Master.?s|Advanced [Dd]egree)", desc) == TRUE) {
    return ("Master's")
  } else if (grepl("(\\<Ph[.]?[Dd]|[Dd]octoral[ [Dd]egree]?)", desc) == TRUE) {
    return ("Ph.D")
  } else {
    return ("None")
  }
}), levels = c("None", "High School", "Associate's", "Bachelor's", "Master's", "Ph.D"))

# location
city = trimws(gsub("([[:alpha:] ]+),([[:space:][:alpha:]]+).*", "\\1", monster$location))
state = trimws(gsub("([[:alpha:] ]+),([[:space:][:alpha:]]+).*", "\\2", monster$location))

# employment type
employmentTypeRegex = "(full[- ]?time|part[- ]time|hourly|contract|short[- ]term|month)"
getEmploymentType = function(description){
  if (grepl(employmentTypeRegex, description, ignore.case = TRUE) == TRUE){
    m = regexpr(employmentTypeRegex, description, ignore.case = TRUE)
    result = regmatches(description, m)
    employmentType = switch(tolower(result),
                            "full-time" = "Full-time",
                            "full time" = "Full-time",
                            "part-time" = "Part-time",
                            "part time" = "Part-time",
                            "hourly" = "Hourly",
                            "contract" = "Contract",
                            "short-term" = "Short-term",
                            "short term" = "Short-term",
                            "month" = "Contract",
                            result)
    return (employmentType)
  } else{
    return (NA)
  }
}
employmentType = unlist(lapply(monster$description, getEmploymentType))

# forming cleaned data frame
monster_df = data.frame(Position = monster$title, 
                        Company = monster$company,
                        City = city,
                        State = state,
                        EmploymentType = employmentType,
                        MinEducationLevel = minEducationLevel,
                        Link = monster$link)
head(monster_df)


## ----------------------------------------------------------------------------------------------------------------------------------
# required skills
reqSkills = tolower(unlist(strsplit(monster$description, "@@")))
reqSkills_keywords = unlist(lapply(reqSkills, function(x) {
  m = gregexpr("([[:alnum:]]+[[:punct:]][[:alnum:]]+|[[:alnum:]]+)", x)
  results = regmatches(x, m)
  return (results)
}))
reqSkills_keywords = reqSkills_keywords[!(reqSkills_keywords %in% stopWords)]
reqSkills_table = sort(table(reqSkills_keywords), decreasing = TRUE)
topReqSkills = reqSkills_table[c("business", "analysis", "sql", "leadership", "python")]
names(topReqSkills) = c("Business", "Analysis", "SQL", "Leadership", "Python")
barplot(topReqSkills, main = "Top Required Skills", xlab = "Skills")

# minimum required education level
minEducationLevel_table = table(minEducationLevel)
barplot(minEducationLevel_table[2:6], main = "Minimum Education Level Requirement", xlab = "Education Level", cex.names = 0.7)

# top degree fields
topFields = reqSkills_table[c("business", "science", "marketing", "engineering", "statistics")]
names(topFields) = c("Business", "Data/Comp. Sc.", "Marketing", "Engineering", "Statistics")
barplot(topFields, main = "Top Degree Fields", xlab = "Fields", cex.names = 0.7)

# employment types
barplot(table(employmentType), main = "Employment Type", xlab = "Employment Type", cex.names = 0.7)

# location
barplot(sort(table(city), decreasing = TRUE)[1:5], main = "Top 5 Cities", xlab = "City", cex.names = 0.8)


## ----------------------------------------------------------------------------------------------------------------------------------
load("indeed.RData")

# minimum education level
minEducationLevel = factor(sapply(indeed$description, function(desc){
  if (grepl("high school diploma", desc, ignore.case = TRUE) == TRUE) {
    return ("High School")
  } else if (grepl("(\\<A[.]?A\\>|\\<A[.]?S\\>|Associate.?s degree)", desc) == TRUE){
    return ("Associate's")
  } else if (grepl("(\\<B[.]?A\\>|\\<B[.]?S\\>|Bachelor[']?s?)", desc) == TRUE){
    return ("Bachelor's")
  } else if (grepl("(M[.]?S\\>|Master.?s|Advanced [Dd]egree)", desc) == TRUE) {
    return ("Master's")
  } else if (grepl("(\\<Ph[.]?[Dd]|[Dd]octoral[ [Dd]egree]?)", desc) == TRUE) {
    return ("Ph.D")
  } else {
    return ("None")
  }
}), levels = c("None", "High School", "Associate's", "Bachelor's", "Master's", "Ph.D"))

# location
city = trimws(gsub("(^[a-zA-Z ]+$|([[:alpha:] ]+),( ?[[:alpha:]]+).*)", "\\2", indeed$location))
city = ifelse(city == "", NA, city) # change "" to NA
state = trimws(gsub("(^[a-zA-Z ]+$|([[:alpha:] ]+),( ?[[:alpha:]]+).*)", "\\3", indeed$location))
state = ifelse(state == "", NA, state) # change "" to NA

# employment type
employmentTypeRegex = "(full[- ]?time|part[- ]time|hourly|contract|short[- ]term|month)"
getEmploymentType = function(description){
  if (grepl(employmentTypeRegex, description, ignore.case = TRUE) == TRUE){
    m = regexpr(employmentTypeRegex, description, ignore.case = TRUE)
    result = regmatches(description, m)
    employmentType = switch(tolower(result),
                            "full-time" = "Full-time",
                            "full time" = "Full-time",
                            "part-time" = "Part-time",
                            "part time" = "Part-time",
                            "hourly" = "Hourly",
                            "contract" = "Contract",
                            "short-term" = "Short-term",
                            "short term" = "Short-term",
                            "month" = "Contract",
                            result)
    return (employmentType)
  } else{
    return (NA)
  }
}
employmentType = unlist(lapply(indeed$description, getEmploymentType))

# forming cleaned data frame
indeed_df = data.frame(Position = indeed$title, 
                        Company = indeed$company,
                        City = city,
                        State = state,
                        EmploymentType = employmentType,
                        MinEducationLevel = minEducationLevel,
                        Link = indeed$link)
head(indeed_df)


## ----------------------------------------------------------------------------------------------------------------------------------
# required skills
reqSkills = tolower(unlist(strsplit(indeed$description, "@@")))
reqSkills_keywords = unlist(lapply(reqSkills, function(x) {
  m = gregexpr("([[:alnum:]]+[[:punct:]][[:alnum:]]+|[[:alnum:]]+)", x)
  results = regmatches(x, m)
  return (results)
}))
reqSkills_keywords = reqSkills_keywords[!(reqSkills_keywords %in% stopWords)]
reqSkills_table = sort(table(reqSkills_keywords), decreasing = TRUE)
topReqSkills = reqSkills_table[c("business", "analysis", "sql", "python", "office")]
names(topReqSkills) = c("Business", "Analysis", "SQL", "Python", "MS Office")
barplot(topReqSkills, main = "Top Required Skills", xlab = "Skills")

# minimum required education level
minEducationLevel_table = table(minEducationLevel)
barplot(minEducationLevel_table, main = "Minimum Education Level Requirement", xlab = "Education Level", cex.names = 0.7)

# top degree fields
topFields = reqSkills_table[c("business", "science", "statistics", "engineering", "healthcare")]
names(topFields) = c("Business", "Data/Comp. Sc.", "Statistics", "Engineering", "Healthcare")
barplot(topFields, main = "Top Degree Fields", xlab = "Fields", cex.names = 0.7)

# employment types
barplot(table(employmentType), main = "Employment Type", xlab = "Employment Type", cex.names = 0.7)

# location
barplot(sort(table(city), decreasing = TRUE)[1:5], main = "Top 5 Cities", xlab = "City", cex.names = 0.8)

