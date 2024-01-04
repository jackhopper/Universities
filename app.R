#Load packages
library(shiny)
library(tidyverse)
library(factoextra)
library(plotly)

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
      tabsetPanel(
        tabPanel("PCA Analysis", plotlyOutput("firstPlot")),
        tabPanel("Scatterplot with Clusters", plotlyOutput("secondPlot"))
      )
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
  output$firstPlot <- renderPlotly({
    fviz_cluster(clusters(),
                 data = data,
                 repel = TRUE,
                 geom = "point",
                 ggtheme = theme_minimal(),
                 main = "Clustering Results")
  })
  
  output$secondPlot <- renderPlotly({
    ggplotly(plot_ly(data = data_with_clusters(), 
                     x = ~act_score, y = ~avg_tuition, type = "scatter", mode = "markers",
                     color = ~cluster,
                     size = ~ug_enroll,
                     hoverinfo = "text", text = ~paste("School: ", school, "<br> Undergrad Enrollment: ", ug_enroll)) %>%
               plotly::layout(title = "Selectivity Metrics for Schools Based on Cluster",
                              xaxis = list(title = "ACT Score (Composite, 75th Percentile)"),
                              yaxis = list(title = "Average Tuition Rate")))
  })
  
})  

# Run the application 
shinyApp(ui = ui, server = server)