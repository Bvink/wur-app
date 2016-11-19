library(shiny)
library(datasets)

# Define server logic required to summarize and view the selected
# dataset
function(input, output) {	
  
  # Return the requested data
  datasetInput <- reactive({	
  
  plotData <- obtainPlotData(input$country)
  plotData <- plotData[order(plotData$names),]
  
  if(input$option == "Raw Numbers") {
	  if(input$gender == "Male") {
		results <- plotData$relevantData*(plotData$maleStats/100)
		colours <- c("lightblue")
	  } else if (input$gender == "Female") { 
		results <- plotData$relevantData*(plotData$femaleStats/100)
		colours <- c("pink")
	  } else if (input$gender == "Comparison" || input$gender == "Total") { 
		results <- rbind(plotData$relevantData*(plotData$maleStats/100), plotData$relevantData*(plotData$femaleStats/100))
		colours <- c("lightblue", "pink")
	  }
	  if (input$gender == "Total") {
	    yLimit <- c(0, ceiling(max(plotData$relevantData)))
	  } else {
	    yLimit <- c(0, ceiling(max(results)))
	  }
  } else if(input$option == "Percentage") {
	  if(input$gender == "Male") {
		results <- plotData$maleStats
		colours <- c("lightblue")
	  } else if (input$gender == "Female") { 
		results <- plotData$femaleStats
		colours <- c("pink")
	  } else if (input$gender == "Comparison"|| input$gender == "Total") { 
		results <- rbind(plotData$maleStats, plotData$femaleStats)
		colours <- c("lightblue", "pink")
	  }
	  yLimit <- c(0,100)
  }
  
  plot <- barplot(results, names.arg = plotData$names, las=2, cex.names=0.8, ylab="Students", col=colours, beside=(input$gender == "Comparison"), ylim=yLimit)
  output <- plot 
  
  })
  
  obtainPlotData <- function(country) {
    if (country != "All") {
      uniData <- universityDataRelevant[which(universityDataRelevant$country == input$country), ]
      plotData <- obtainUniversityData(uniData)
    } else {
      countryData <- universityDataRelevant
	  plotData <- obtainCountryData(countryData)
    }
	return(plotData)
  }
  
  obtainUniversityData <- function(uniData) {
	names <- uniData$university_name
	relevantData <- uniData$num_students
	maleStats <- getMalePercentages(uniData)
    femaleStats <- getFemalePercentages(uniData)
	plotData <- formatData(names, relevantData, maleStats, femaleStats)
	return(plotData)
  }
  
  obtainCountryData <- function(countryData) {
    aggRawNumbers <- aggregate(countryData$num_students, by=list(Category=countryData$country), FUN=sum)
	names = aggRawNumbers[,1]
	relevantData = aggRawNumbers[,2]
	maleStats = getMaleAverage(countryData)
	femaleStats = getFemaleAverage(countryData)
	plotData <- formatData(names, relevantData, maleStats, femaleStats)
	return(plotData)
  }
  
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
  
  formatData <- function(names, relevantData, maleStats, femaleStats) {
    names <- droplevels(names)
    plotData <- data.frame(names, relevantData, maleStats, femaleStats)
	return(plotData)
  }
 
 
  # Show the number of students per country/university.
  output$view <- renderPlot({
    options(scipen=5)
	par(mar=c(15,6,2,2), mgp=c(5,1,0))
	datasetInput()
  }, width = "auto", height = 750)
  
}
