library(shiny)

# Define UI for dataset viewer application

fluidPage(
  # Application title
  titlePanel("[PLACEHOLDER TITLE]"),
  
  # Sidebar with controls to select a country and specify your age & gender
  sidebarLayout(
    sidebarPanel(width=2,
	
	#TODO: Find out how to get these from the dataset
      selectInput("country", "Select your country:",
				  choices = c(as.character(universitySorted))),
      
	  selectInput("gender", "Select which gender:", 
                  choices = c("Male", "Female", "Comparison", "Total")),
		
	  selectInput("option", "Select which type:", 
                  choices = c("Raw Numbers", "Percentage")),
				  
	  p(a("Source: The Times Higher Education World University Ranking", href = "https://www.timeshighereducation.com/world-university-rankings", target = "_blank"))
    ),
    
    # Show a summary of the dataset and an HTML table with the 
	# requested number of observations
    mainPanel(width=10,
      plotOutput("view")
    )
  )
)
