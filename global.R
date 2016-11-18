universityData <- read.csv("data/timesdata.csv", header=TRUE)
relevantColumns <-  universityData[, c("university_name", "country", "num_students", "female_male_ratio", "year")]
universityDataRelevant <- relevantColumns[-which(relevantColumns$year != "2016"), ]
universityDataRelevant <- universityDataRelevant[-which(universityDataRelevant$female_male_ratio == ""), ]
universityDataRelevant <- universityDataRelevant[-which(universityDataRelevant$female_male_ratio == "-"), ]
universityDataRelevant$num_students <- as.numeric(gsub(",","",universityDataRelevant$num_students))

countrySorted <- sort(unique(universityDataRelevant[,2]))

yeardataSorted <- sort(unique(universityData[,14]))
