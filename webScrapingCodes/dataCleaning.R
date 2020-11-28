educationRegex = "(\\<B[.]?A\\>|\\<B[.]?S\\>|Bachelor[']?s?|M[.]?S\\>|Master'?s|\\<Ph[.]?[Dd]|Advanced [Dd]egree|[Dd]octoral [Dd]egree).*"
employmentTypeRegex = "(full[- ]?time|part[- ]time|hourly|contract|short[- ]term|month|duration)"
# TODO: look for salaries
salaryRegex = "(pay[ rate]?|salary)"
# TODO: work on required skills and preferred skills(for monster.com)
reqSkillsRegex = "([Ee]xperience with.+)"


# cleaning CyberCoders
load("cyberCoders.RData")

# required skills
reqSkills = strsplit(cyberCoders$requiredSkills, "@@")
educationLevel = sapply(reqSkills, function(job) grep(educationRegex, job, value = TRUE))
requiredSkills = sapply(reqSkills, function(job) grep(reqSkillsRegex, job, value = TRUE))


# cleaning monster
load("monster.RData")
description = strsplit(monster$description, "@@")
educationLevel = sapply(description, function(job) grep(educationRegex, job, value = TRUE))
employmentType = sapply(description, function(job) grep(employmentTypeRegex, job, ignore.case = TRUE, value = TRUE))

# cleaning career builder
load("careerBuilder.RData")
description = strsplit(careerBuilder$description, "@@")
educationLevel = sapply(description, function(job) grep(educationRegex, job, value = TRUE))
