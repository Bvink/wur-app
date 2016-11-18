library(shiny)
library(datasets)

# Define server logic required to summarize and view the selected
# dataset
function(input, output) {	
  
  # Return the requested data
  datasetInput <- reactive({	
  Country <- input$country
  if (Country != "All") {
    uniData <- universityDataRelevant[which(universityDataRelevant$country == "Germany"), ]
	uniData <- uniData[order(uniData$university_name),]
	relevantData <- uniData$num_students
    horizontal <- uniData$university_name
	maleStats <- getMalePercentages(uniData)
    femaleStats <- getFemalePercentages(uniData)
  } else {
    uniData <- universityDataRelevant
	aggRawNumbers <- aggregate(uniData$num_students, by=list(Category=uniData$country), FUN=sum)
	horizontal = aggRawNumbers[,1]
	relevantData = aggRawNumbers[,2]
	maleStats = getMaleAverage(uniData)
	femaleStats = getFemaleAverage(uniData)
  }
  
  horizontal <- droplevels(horizontal)
  beside <- FALSE
  
  if(input$option == "Raw Numbers") {
	  if(input$gender == "Male") {
		plotData <- relevantData*(maleStats/100)
		colours <- c("lightblue")
		yLimit <- FALSE
	  } else if (input$gender == "Female") { 
		plotData <- relevantData*(femaleStats/100)
		colours <- c("pink")
		yLimit <- FALSE
	  } else if (input$gender == "Comparison" || input$gender == "Total") { 
		plotData <- rbind(relevantData*(maleStats/100), relevantData*(femaleStats/100))
		colours <- c("lightblue", "pink")
		yLimit <- FALSE
	  }
	  yLimit <- c(0, ceiling(max(plotData)))
	  if(input$gender == "Total") {
		yLimit <- c(0, ceiling(max(relevantData)))
	  } else {
		beside <- TRUE
		yLimit <- c(0, ceiling(max(plotData)))
	  }
  } else if(input$option == "Percentage") {
	  if(input$gender == "Male") {
		plotData <- maleStats
		colours <- c("lightblue")
	  } else if (input$gender == "Female") { 
		plotData <- femaleStats
		colours <- c("pink")
	  } else if (input$gender == "Comparison"|| input$gender == "Total") { 
		plotData <- rbind(maleStats, femaleStats)
		colours <- c("lightblue", "pink")
		if(input$gender == "Comparison") {
		  beside <- TRUE
		}
	  }
	  yLimit <- c(0,100)
  }
  
  plot <- barplot(plotData, names.arg = horizontal, las=2, cex.names=0.8, ylab="Students", col=colours, beside=beside, ylim=yLimit)
  output <- plot 
  
  })
  
  getMalePercentages <- function(uniData) {
  maleStats <- as.numeric(gsub("\\:.*","",uniData$female_male_ratio[]))
  return(maleStats)
  }
  
  getMaleAverage <- function(uniData) {
    maleStats <- getMalePercentages(uniData)
    tempDataStruct <- uniData
	tempDataStruct$female_male_ratio <- maleStats
	aggMaleStats <- aggregate(tempDataStruct$female_male_ratio, by=list(Category=uniData$country), FUN=sum)
	tempDataStruct$female_male_ratio <- 1
	countMaleStats <- aggregate(tempDataStruct$female_male_ratio, by=list(Category=uniData$country), FUN=sum)
	averageMaleStats <- aggMaleStats[,2]/countMaleStats[,2]
	return(averageMaleStats)
  }
  
  getFemalePercentages <- function(uniData) {
  femaleStats <- as.numeric(gsub("^.*?\\:","",uniData$female_male_ratio[]))
  return(femaleStats)
  }
  
  getFemaleAverage <- function(uniData) {
    femaleStats <- getFemalePercentages(uniData)
    tempDataStruct <- uniData
	tempDataStruct$female_male_ratio <- femaleStats
	aggFemaleStats <- aggregate(tempDataStruct$female_male_ratio, by=list(Category=uniData$country), FUN=sum)
	tempDataStruct$female_male_ratio <- 1
	countFemaleStats <- aggregate(tempDataStruct$female_male_ratio, by=list(Category=uniData$country), FUN=sum)
	averageFemaleStats <- aggFemaleStats[,2]/countFemaleStats[,2]
	return(averageFemaleStats)
  }
 
 
  # Show the number of students per country/university.
  output$view <- renderPlot({
    options(scipen=5)
	par(mar=c(15,5,2,2), mgp=c(4,1,0))
	datasetInput()
  }, width = "auto", height = 750)
  
}
