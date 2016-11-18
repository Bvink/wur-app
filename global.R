universityData <- read.csv("data/timesdata.csv", header=TRUE)
relevantColumns <-  universityData[, c("university_name", "country", "num_students", "female_male_ratio", "year")]
universityDataRelevant <- relevantColumns[-which(universityData$year != "2016"), ]
universityDataRelevant <- universityDataRelevant[-which(universityDataRelevant$num_students == ""), ]
universityDataRelevant <- universityDataRelevant[-which(universityDataRelevant$female_male_ratio == ""), ]

universitySorted <- sort(unique(universityDataRelevant[,2]))

yeardataSorted <- sort(unique(universityData[,14]))
