library(shiny)
library(shinydashboard)
library(reader)
library(arules)


ui <- dashboardPage(
  dashboardHeader(title = "Grocery Data Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graphs", tabName = "graphs", icon = icon("bar-chart")),
      menuItem("K-means Clustering", tabName = "kmeans_clustering", icon = icon("chart-line")),
      menuItem("Apriori Rule", tabName = "apriori_rule", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "graphs",
              fluidRow(
                box(
                  title = "Upload Data",
                  fileInput("graph_file", "Choose CSV File",
                            accept = c(".csv"))
                ),
                box(
                  title = "Payment type: Pie Chart",
                  plotOutput("pieChart", height = 300)
                ),
                box(
                  title = "Age vs Total spending: Scatter Plot",
                  plotOutput("scatterPlot", height = 300)
                ),
                box(
                  title = "City Frequency: Bar Plot",
                  plotOutput("barPlot", height = 300)
                ),
                box(
                  title = "Total Spending Distribution: Histogram",
                  plotOutput("histogram", height = 300)
                )
              )
      ),
      tabItem(tabName = "kmeans_clustering",
              fluidRow(
                box(
                  title = "K-means Clustering",
                  fileInput("file1", "Choose CSV File",
                            accept = c(".csv")),
                  numericInput("clusters", "Number of clusters (between 2 and 4):", value = 2, min = 2, max = 4),
                  actionButton("clusterButton", "Perform Clustering"),
                  tableOutput("clusterTable")
                )
              )
      ),
      tabItem(tabName = "apriori_rule",
              fluidRow(
                box(
                  title = "Apriori Rule",
                  fileInput("file", "Choose CSV File",
                            accept = c(".csv")),
                  sliderInput("support", "Support (between 0.001 and 1):", value = 0.1, min = 0.001, max = 1,step = 0.001),
                  sliderInput("confidence", "Confidence (between 0.001 and 1):", value = 0.1, min = 0.001, max = 1,step = 0.001),
                  actionButton("runApriori", "Run Apriori Rule"),
                  tableOutput("aprioriResults")
                )
              )
      )
    )
  )
)


server <- function(input, output) {
  
  
  graph_data <- reactive({
    req(input$graph_file) 
    df <- read.csv(input$graph_file$datapath)
    df <- unique(df)
    df <- na.omit(df)
    # Calculate pie chart data
    px <- table(df$paymentType)
    pie_data <- list(px = px)
    
    # Calculate scatter plot data
    y <- c(22, 25, 30, 50, 55, 29, 39, 37, 60, 55, 22, 23, 37, 36, 35)
    i <- 1
    j <- 1
    total_spending <- 1:15
    total <- 0
    while (j <= length(total_spending)) {
      total <- 0
      while (i <= length(df$rnd)) {
        if (j == df$rnd[i]) {
          total <- total + df$total[i]
        }
        i <- i + 1
      }
      total_spending[j] <- total
      i <- 1
      j <- j + 1
    }
    scatter_data <- list(x = total_spending, y = y)
    
    # Calculate bar plot data
    city_names <- names(table(df$city))
    city_frequency <- as.vector(table(df$city))
    barplot_data <- list(height = city_frequency, name = city_names)
    
    # Calculate histogram data
    histogram_data <- df$total
    
    return(list(pie_data = pie_data, scatter_data = scatter_data, barplot_data = barplot_data, histogram_data = histogram_data))
  })
  
  # Render pie chart
  output$pieChart <- renderPlot({
    px <- graph_data()$pie_data$px
    total <- paste(round(100 * px / sum(px),2), '%')
    pie(px, labels = total, main = 'Total cash account and credit', col = c('lightblue', 'white'))
    legend('bottomleft', legend = c('cash', 'credit'), fill = c('lightblue', 'white'))
  })
  
  # Render scatter plot
  output$scatterPlot <- renderPlot({
    scatter_data <- graph_data()$scatter_data
    plot(scatter_data$y, scatter_data$x, xlab = 'age', ylab = 'total spending', main = 'Total Spending VS Age', col = 'blue')
  })
  
  # Render bar plot
  output$barPlot <- renderPlot({
    barplot_data <- graph_data()$barplot_data
    barplot(height = barplot_data$height, names.arg = barplot_data$name, col = 'red', main = 'City Frequency', xlab = 'city', ylab = 'frequency',las=2)
  })
  
  # Render histogram
  output$histogram <- renderPlot({
    histogram_data <- graph_data()$histogram_data
    hist(histogram_data, col = "pink", border = "white", main = "Total Spending Distribution", xlab = "Total spending", ylab = "frequency")
  })
  
  # Function to perform K-means clustering
  kmeans_clustering <- function(data, k) {
    # Perform K-means clustering on the data
    clustering_fit <- kmeans(data[, c("age", "total")], centers = k)
    
    # Create a data frame with cluster assignments
    cluster_assignments <- data.frame(Customer = data$customer,
                                      Age = data$age,
                                      Total = data$total,
                                      Cluster = clustering_fit$cluster)
    
    return(cluster_assignments)
  }
  
  # Reactive function to read dataset and perform clustering on button click
  clustered_data <- eventReactive(input$clusterButton, {
    req(input$file1)
    
    # Read the uploaded CSV file
    df <- read.csv(input$file1$datapath)
    df <- unique(df)
    df <- na.omit(df)
    i <- 1
    j <- 1
    total_spending <- 1:15
    total <- 0
    while (j <= length(total_spending)) {
      total <- 0
      while (i <= length(df$rnd)) {
        if (j == df$rnd[i]) {
          total <- total + df$total[i]
        }
        i <- i + 1
      }
      total_spending[j] <- total
      i <- 1
      j <- j + 1
    }
    clusters <- input$clusters
    CLtable <- data.frame(customer = c('Farida', 'Mohammed', 'Ahmed', 'Adel', 'Shimaa', 'Walaa', 'Huda', 'Rania', 'Maged', 'Samy', 'Hanan', 'Eman', 'Sayed', 'Magdy', 'Sameh'),
                          age = c(22, 25, 30, 50, 55, 29, 39, 37, 60, 55, 22, 23, 37, 36, 35),
                          total = total_spending)
    cluster_assignments <- kmeans_clustering(CLtable, clusters)
    
    return(cluster_assignments)
  })
  
  # Display clustered data in table
  output$clusterTable <- renderTable({
    clustered_data()
  })
  
  # Reactive function to read dataset and perform Apriori rule on button click
  apriori_data <- eventReactive(input$runApriori, {
    req(input$file)
    df <- read.csv(input$file$datapath)
    df <- unique(df)
    df <- na.omit(df)
    write.table(df$items, "items.txt", row.names = FALSE, col.name = FALSE, quote=FALSE )
    transactions <- read.transactions("items.txt", format = "basket", sep = ",")
    rules <- apriori(transactions, parameter = list(support = input$support, confidence = input$confidence))
    
    # Convert rules to a data frame
    rules_df <- as(rules, "data.frame")
    
    return(rules_df)
  })
  
  # Display Apriori rule results in table
  output$aprioriResults <- renderTable({
    apriori_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)