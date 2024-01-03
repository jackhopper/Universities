#Load packages
library(shiny)
library(tidyverse)
library(factoextra)

###Read in data
modeled_scaled <- read_csv('https://raw.githubusercontent.com/jackhopper/Universities/main/modeled_data_df.csv')
schools <- read_csv('https://raw.githubusercontent.com/jackhopper/Universities/main/school_data_df.csv')


#Define UI for application
ui <- shinyUI(fluidPage(
  
  # Define side panel with slider input
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_clusters", "Number of clusters:", min = 1, max = 10, value = 3)
    ),
    
    # Main panel to display clustering results
    mainPanel(
      plotOutput("clusterPlot")
    )
  )
))


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  # Load your data
  data <- modeled_scaled
  
  # Perform k-means clustering based on user input
  clusters <- reactive({
    kmeans(data, input$num_clusters)
  })
  
  # Apply cluster assignments to raw data
  data_with_clusters <- reactive({
    schools %>% 
      mutate(cluster = clusters()$cluster) 
  })
  
  # Generate the raw fviz cluster plot
  output$clusterPlot <- renderPlot({
    fviz_cluster(clusters(),
                 data = data,
                 repel = TRUE,
                 geom = "point",
                 ggtheme = theme_minimal(),
                 main = "Clustering Results")
    #plot(data[, 1:2], col = clusters()$cluster, pch = 20, cex = 3)
  })
})

# Run the application 
shinyApp(ui = ui, server = server)