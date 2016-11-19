library(shiny)
library(datasets)

# Define server logic required to summarize and view the selected
# dataset
function(input, output) {	
  
  # Return the requested data
  datasetInput <- reactive({	
  
  plotData <- obtainPlotData(input$country)
  plotData <- sortData(plotData, input$mode, input$option, input$gender)
  
  if(input$option == "Raw Numbers") {
	  if(input$gender == "Male") {
		results <- calculateMaleRawNumbers(plotData)
		colours <- c("lightblue")
	  } else if (input$gender == "Female") { 
		results <- calculateFemaleRawNumbers(plotData)
		colours <- c("pink")
	  } else if (input$gender == "Comparison" || input$gender == "Total") { 
		results <- rbind(calculateMaleRawNumbers(plotData), calculateFemaleRawNumbers(plotData))
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
  
  #Plot a barplot with the given data
  plot <- barplot(results, names.arg = plotData$names, las=2, cex.names=0.8, ylab="Students", col=colours, beside=(input$gender == "Comparison"), ylim=yLimit)
  output <- plot 
  
  })
  
  #Obtain the dataframe with all information relevant to the plot
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
  
  #Obtain the plot data relevant to all universities of a single country
  #This returns a single dataframe with all the relevant information
  obtainUniversityData <- function(uniData) {
	names <- uniData$university_name
	relevantData <- uniData$num_students
	maleStats <- getMalePercentages(uniData)
    femaleStats <- getFemalePercentages(uniData)
	plotData <- formatData(names, relevantData, maleStats, femaleStats)
	return(plotData)
  }
  
  #Obtain the plot data relevant to "all" countries
  #This returns a single dataframe with all the relevant information
  obtainCountryData <- function(countryData) {
    aggRawNumbers <- aggregate(countryData$num_students, by=list(Category=countryData$country), FUN=sum)
	names = aggRawNumbers[,1]
	relevantData = aggRawNumbers[,2]
	maleStats = getMaleTotal(countryData)
	femaleStats = getFemaleTotal(countryData)
	plotData <- formatData(names, relevantData, maleStats, femaleStats)
	return(plotData)
  }
  #Get the correct percentage of males from the dataset
  getMalePercentages <- function(uniData) {
    maleStats <- as.numeric(gsub("\\:.*","",uniData$female_male_ratio[]))
  return(maleStats)
  }
  
  #TODO: THIS IS ENTIRELY WRONG.
  getMaleTotal <- function(uniData) {
    maleStats <- getMalePercentages(uniData)
    tempDataStruct <- uniData
	tempDataStruct$female_male_ratio <- maleStats
	aggMaleStats <- aggregate(tempDataStruct$female_male_ratio, by=list(Category=uniData$country), FUN=sum)
	tempDataStruct$female_male_ratio <- 1
	countMaleStats <- aggregate(tempDataStruct$female_male_ratio, by=list(Category=uniData$country), FUN=sum)
	averageMaleStats <- aggMaleStats[,2]/countMaleStats[,2]
	return(averageMaleStats)
  }
  
  # Calculate the amount of males per university
  calculateMaleRawNumbers <- function(plotData) {
    return(plotData$relevantData*(plotData$maleStats/100))
  }
  
  
  #Get the correct percentage of females from the dataset
  getFemalePercentages <- function(uniData) {
    femaleStats <- as.numeric(gsub("^.*?\\:","",uniData$female_male_ratio[]))
  return(femaleStats)
  }
  
  #TODO: THIS IS ENTIRELY WRONG.
  getFemaleTotal <- function(uniData) {
    femaleStats <- getFemalePercentages(uniData)
    tempDataStruct <- uniData
	tempDataStruct$female_male_ratio <- femaleStats
	aggFemaleStats <- aggregate(tempDataStruct$female_male_ratio, by=list(Category=uniData$country), FUN=sum)
	tempDataStruct$female_male_ratio <- 1
	countFemaleStats <- aggregate(tempDataStruct$female_male_ratio, by=list(Category=uniData$country), FUN=sum)
	averageFemaleStats <- aggFemaleStats[,2]/countFemaleStats[,2]
	return(averageFemaleStats)
  }
  
  # Calculate the amount of females per university
  calculateFemaleRawNumbers <- function(plotData) {
    return(plotData$relevantData*(plotData$femaleStats/100))
  }
  
  # Format all of the obtained stats back into a single dataframe.
  formatData <- function(names, relevantData, maleStats, femaleStats) {
    names <- droplevels(names)
    plotData <- data.frame(names, relevantData, maleStats, femaleStats)
	return(plotData)
  }
  
  #Sort the data dependant on the options given.
  sortData <- function(plotData, mode, option, gender) {
    if(mode == "Alphabetical") {
	  plotData <- plotData[order(plotData$names),]
	} else if (mode == "Numeral") {
	  if(option == "Raw Numbers") {
	    if(gender == "Male") {
		  plotData <- plotData[order(calculateMaleRawNumbers(plotData)),]
		} else if(gender == "Female") {
		  plotData <- plotData[order(calculateFemaleRawNumbers(plotData)),]
		} else if(gender == "Comparison" || gender == "Total") {
		  plotData <- plotData[order(plotData$relevantData),]
		}
	  } else if(option == "Percentage") {
	  if(gender == "Male") {
		  plotData <- plotData[order(plotData$maleStats),]
		} else if(gender == "Female") {
		  plotData <- plotData[order(plotData$femaleStats),]
		} else if(gender == "Comparison" || gender == "Total") {
		  plotData <- plotData[order(plotData$maleStats),]
		}
	  }
	}
	return(plotData)
  }
 
 
  # Render the plot
  output$view <- renderPlot({
    options(scipen=5)
	par(mar=c(18,6,2,2), mgp=c(5,1,0))
	datasetInput()
  }, width = "auto", height = 750)
  
}
