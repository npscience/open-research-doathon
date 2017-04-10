require(shiny)
require(ggplot2)
#Read in Data

data <- read.csv("101innovations-npscience-lifesci-analysistools-counts.csv")

data = data[-1,]


#Filter top 10

server <- function(input, output) {
  
  
  filteredData = reactive({
    
    d= data[data$Grand.Total > input$threshold,] #Filter out rows below user specified threshold
    
    t = d[,1] #First Column containing tool names
    c = d[,as.numeric(input$role)+1] #Column containing usage count specified by user
    
    data.frame(Tool = t, Count = c) #Return as data frame
    
  } )
  
  
  output$plot1 <- renderPlot({
    
    p <- ggplot(data=filteredData(), aes(x=reorder(Tool,Count), y=Count)) +
      geom_bar(stat = 'identity') +
      xlab('')
    
    p + coord_flip()
    
  })
  
}


ui <- fluidPage(
  
  titlePanel("Tools used by Life Scientists"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("threshold",label='Minimum Uses',min=0,max=600,value=50),
      selectInput('role',label='Select People Role:',choices=c('All'=1,
                                                               'Bachelors/Masters Student'=2,
                                                               'Industry/Government'=3,
                                                               'Librarian'=4,
                                                               'Other'=5,
                                                               'PhD Student'=6,
                                                               'Post-Doc'=7,
                                                               'Professor'=8,
                                                               'Publisher'=9),selected = 1)
    ),
    
    mainPanel(
      
      plotOutput('plot1')
      
    )
  )
  
)


shinyApp(ui = ui, server = server)