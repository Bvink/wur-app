library(shiny)
library(datasets)

# Define server logic required to summarize and view the selected
# dataset
function(input, output) {	
  
  # Return the requested data
  datasetInput <- reactive({	
  
  uniData <- universityDataRelevant[which(universityDataRelevant$country == input$country), ]
  uniData$num_students <- as.numeric(gsub(",","",uniData$num_students))
  
  uniData <- uniData[order(uniData$university_name),]
  
  students <- uniData$num_students
  university <- uniData$university_name
  university <- droplevels(university)
  
  maleStats = as.numeric(substr(uniData$female_male_ratio[],1,2))
  femaleStats = as.numeric(substr(uniData$female_male_ratio[],6,7))
  
  beside <- FALSE
  
  if(input$option == "Raw Numbers") {
	  if(input$gender == "Male") {
		plotData <- students*(maleStats/100)
		colours <- c("lightblue")
		yLimit <- FALSE
	  } else if (input$gender == "Female") { 
		plotData <- students*(femaleStats/100)
		colours <- c("pink")
		yLimit <- FALSE
	  } else if (input$gender == "Comparison" || input$gender == "Total") { 
		plotData <- rbind(students*(maleStats/100), students*(femaleStats/100))
		colours <- c("lightblue", "pink")
		yLimit <- FALSE
	  }
	  yLimit <- c(0, ceiling(max(plotData)))
	  if(input$gender == "Total") {
		yLimit <- c(0, ceiling(max(students)))
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
  plot <- barplot(plotData, names.arg = university, las=2, cex.names=0.8, ylab="Students", col=colours, beside=beside, ylim=yLimit)
  output <- plot 
  
  })
 
 
  # Show the number of students per country/university.
  output$view <- renderPlot({
    options(scipen=5)
	par(mar=c(15,5,2,2), mgp=c(4,1,0))
	datasetInput()
  }, width = "auto", height = 750)
  
}
