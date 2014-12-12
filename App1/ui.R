shinyUI(fluidPage(
  
  titlePanel(title =" Exploratory Financial Data Analysis : Data Visualizations "),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Get Global or US Market Share of various Industry groups "), 
      
      selectInput("var", 
                  label = "Choose a Parameter ",
                  choices = c("Global", "US"
                  ),
                  selected = "Global") 
      ),
    mainPanel(
      #title (" Based On the different Relationships , a distribution of Connections among Artists. "),

      plotOutput("plot1")
      
    )
    
  )
  
  
)
)