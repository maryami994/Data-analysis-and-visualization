# Install and load necessary packages

#install.packages("readxl")
library(readxl)
#install.packages("shiny")
library(shiny)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages('factoextra')
library(factoextra)
#install.packages("memoise")
library(memoise)
#install.packages("arulesViz")
library(arulesViz)
#install.packages("arules")
library(arules)
#install.packages("shinythemes")
library(shinythemes)



# Define UI (the appearance of the page)
ui <- fluidPage(
  theme=shinythemes::shinytheme("flatly"),
  titlePanel("Datasets Analysis and Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File"),
      actionButton("clean", "Clean Data"),
      downloadButton("download", "Download Cleaned Data"),
      width="auto"
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cleaned Data", tableOutput("cleaned_table"),
                 ),
        
        tabPanel("Data Visualization",
                   
                   fluidRow(
                     box(title = "Distribution of Total Spendings", status = "primary",
                         plotOutput("boxplot")),
                     box(title = "Cash vs Credit Totals", status = "success",
                         plotOutput("piechart")),
                     box(title = "Comparison of Age and Total Spendings", status = "info",
                         plotOutput("agebarplot")),
                     box(title = "Comparison of City and Total Spendings", status = "danger",
                         plotOutput("citybarplot"))
                   )
        ),
        
        tabPanel("Clustering",
                  sidebarPanel(
                    width = "auto",
                    sliderInput("clusters",
                              "Number of clusters:",
                              min = 2,
                              max = 4,
                              step = 1,
                              value = 2),
                    actionButton("clustersCalc", "Ganerate Clusters Grouping")),
                    mainPanel(
                      fluidRow(
                        column(width = 8, plotOutput("plot")),
                        column(width = 4, tableOutput("table"))
                      )
                  ),
              ),
          tabPanel("Association",
                   sidebarPanel(
                     
                     sliderInput(
                       "minSupport",
                       "Minimum Support:",
                       min = 0.001,
                       max = 1,
                       value = 0.001,
                       step = 0.001
                     ),
                     sliderInput(
                       "minConfidence",
                       "Minimum Confidence:",
                       min = 0.001,
                       max = 1,
                       value = 0.001,
                       step = 0.001
                     ),
                     actionButton("generateButton", "Generate Rules")
                   ),
                   mainPanel(
                      fluidRow(
                    column(width = 6,  # Adjusted column width
                      h4("Number of Rules Generated:"),
                      verbatimTextOutput("numRules"),
                      h4("Generated Rules:"),
                      verbatimTextOutput("rules")
                    ),
                    column(width = 6,  # Adjusted column width
                      plotOutput("frequencyPlot")
                    )
                   )
                 ),
         )
       )
     )
  )
)


# Define server logic (the resultant action of each component and output generating)
server <- function(input, output) {
  
  # Reactive function to read and clean the data
  cleaned_data <- eventReactive(input$clean, {
    req(input$file)
    data <- read.csv(input$file$datapath)
    # Remove duplicate rows
    data <- unique(data)
    # Remove rows with NA values
    data <- na.omit(data)
    # Print the structure of the data frame
    str(data) 
    data
  })
  
  
  #tab panel 1:data cleaning
  # Output cleaned data as a table
  output$cleaned_table <- renderTable({
    req(is.data.frame(cleaned_data()))
    cleaned_data()
  })
  
  # Download handler for cleaned data CSV
  output$download <- downloadHandler(
    filename = function() {
      "cleaned_data.csv"
    },
    content = function(file) {
      write.csv(cleaned_data(), file, row.names = FALSE)
    }
  )
  
  
  #tab panel 2: data visualizing
  # 1)box plot
  output$boxplot <- renderPlot({
    data <- cleaned_data()
    boxplot(data$total,
            main = "Distribution of Total Spendings",
            xlab = "Total Spendings",
            col = c("#CC9999"))
  })
  
  # 2)pie chart
  output$piechart <- renderPlot({
    data <- cleaned_data()
    pie(x = table(data$paymentType),
        main = "Cash vs Credit Totals",
        col = c("#FFCCCC", "#CC99CC"))
  })
  
  # 3)bar plot of the total spendig of each age
  output$agebarplot <- renderPlot({
    data <- cleaned_data()
    total_spending_age <- aggregate(total ~ age, data = data, FUN = sum)
    ggplot(total_spending_age, aes(x = age, y = total)) +
      geom_bar(stat = "identity", fill = "#CC6699") +
      labs(x = "Age", y = "Total Spending", title = "Comparison of Age and Total Spendings")
  })
  
  # 4)bar plot of the total spending of people from each city
  output$citybarplot <- renderPlot({
    data <- cleaned_data()
    total_spending_city <- aggregate(total ~ city, data = data, FUN = sum)
    ggplot(total_spending_city, aes(x = reorder(city, -total), y = total)) +
      geom_bar(stat = "identity", fill = "#996699") +
      labs(x = "City", y = "Total Spending", title = "Comparison of City and Total Spendings") +
      scale_y_continuous(labels = scales::comma)
  })
  
  
  #tab panel 3: k means clustering
  #1)getting the specific columns of data needed for the process
  data.clust <- eventReactive(input$clustersCalc,{
    req(cleaned_data())
    data.f<-cleaned_data()[, c("customer", "age", "total")] %>%
          group_by(customer, age) %>% summarise(total_spending = sum(total),.groups = "drop") %>% collect()
    return(data.f)
  })
  #2)getting the number of clusters according to the input of the slider input
  cluster <- eventReactive(input$clustersCalc,input$clusters)
  
  #3)calculate k-means clustering outside of renderTable
  k_means <- reactive({
    req(data.clust(), cluster())
    kmeans(data.clust()[, c("age", "total_spending")], centers = as.integer(cluster()))
  })
  
  #4)create the table of each customer info and cluster number
  output$table <- renderTable({
    req(k_means(), input$clustersCalc)
    if (is.null(k_means())) {
      return("K-means clustering has not been calculated yet.")
    } else {
      final_data <- data.clust() %>% mutate(cluster_number = k_means()$cluster[match(age, data.clust()$age)])
      final_data
    }
  })
  
  #5)visualizing the clustering result
  output$plot <- renderPlot({
    req(k_means(), input$clustersCalc)
    if (is.null(k_means())) {
      return("K-means clustering has not been calculated yet.")
    } else {
      kmeans_scale <- k_means()
      fviz_cluster(kmeans_scale, data = data.clust()[, c("age", "total_spending")], data.clustcolor_var = 'cluster_number',
                   ellipse.color = "grey", add.cluster = TRUE) +
        labs(title = 'Clusters of Customers Based on Age and Total Spending',
             x = 'Age scaled values', y = 'Total Spending scaled values') +
        theme_minimal() +
        geom_text(aes(label = data.clust()$customer), check_overlap = TRUE)
    }
  })
  
  #tab panel 4: association rules
  #1) Reactive function to generate association rules based on user input
  rules <- eventReactive(input$generateButton, {
    req(cleaned_data(), input$minSupport, input$minConfidence)
    data <- cleaned_data()
    minSupport <- input$minSupport
    minConfidence <- input$minConfidence
    transactions_list <- lapply(data$items, function(x)unlist(strsplit(x, ",")))
    options("max.print" = 8000000)
    items_data <- as(transactions_list, "transactions")
    rules <- apriori(
      items_data,
      parameter = list(
        support = minSupport,
        confidence = minConfidence,
        minlen = 2
      )
    )
    return(rules)
  })
  
  # 2)text output of the number of rules generated
  output$numRules = renderText({
    req(rules())
    paste("Number of rules generated:", length(rules()))
  })
  
  # 3)output of the generated rules details
  output$rules = renderPrint({
    req(rules())
    if (!is.null(rules())) {
      inspect(rules())
    } else {
      print("No rules generated yet")
    }
    width="auto"
  })
 
  #  4) visualizing the output in a frequency plot
  output$frequencyPlot = renderPlot({
    req(cleaned_data(), input$generateButton)  
    data <- cleaned_data() 
    if (!is.null(rules())) {
      transactions_list <- lapply(data$items, function(x) unlist(strsplit(x, ",")))
      items_data <- as(transactions_list, "transactions")
      itemFrequencyPlot(
        items_data,
        main = "Frequency of items",
        xlab = "Items",
        ylab = "Frequency",
        col = "#CC9999",
        topN = 10,
        type= 'absolute'
      )
    }
  })
}


# Running the constructed application
shinyApp(ui = ui, server = server)
