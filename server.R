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
  
  output <- barplot(students, names.arg = university, las=2, cex.names=0.8, ylab="Students")
  
  })
 
 
  # Show the number of students per country/university.
  output$view <- renderPlot({
    options(scipen=5)
	par(mar=c(15,5,2,2), mgp=c(4,1,0))
	datasetInput()
  }, width = "auto", height = 750)
  
  
}
