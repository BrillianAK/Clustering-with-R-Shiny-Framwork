## app.R ##
library(shiny) # ShinyApp
library(readxl) # Read Excel
library(shinydashboard) # ShinyDashboard
library(preprocessCore)
library(reshape2)
library(tidyverse)
library(tidyr)
library(ggplot2)  
library(GGally)
library(cluster)    # clustering algorithms
library(NbClust)
library(factoextra)
library(stringr)
library(bestNormalize)
library(data.table)
# BiocManager::install(pkgs = "preprocessCore")
library(BiocManager)

# Read Excel
readExcel <- function(inFile) {
  if(is.null(inFile))
    return(NULL)
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
}

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "K-Means Clustering"),
                    ## Sidebar content
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Table", tabName = "Table", icon = icon("table")),
                        menuItem("Describe", tabName = "Describe", icon = icon("flask")),
                        menuItem("Normalize", tabName = "Normalize", icon = icon("wrench")),
                        menuItem("Optimum Cluster", tabName = "Optimum", icon = icon("dashboard")),
                        menuItem("Visualize", tabName = "Visualize", icon = icon("line-chart"))
                      )
                    ),
                    ## Body content
                    dashboardBody(
                      tags$script(HTML("$('body').addClass('fixed');")), # fixed top
                      tabItems(
                        # First tab content
                        tabItem(tabName = "Table",
                                fluidRow(
                                  box(
                                    title = "import Data", status = "danger", solidHeader = TRUE,
                                    collapsible = TRUE, width = 12,
                                    fileInput('file1', 'Choose xlsx file',
                                              accept = c(".xlsx"))
                                  ),
                                  box(
                                    title = "Output", status = "danger", solidHeader = TRUE,
                                    collapsible = TRUE, width = 12,
                                    dataTableOutput('contents')
                                  )
                                )
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "Describe",
                                fluidRow(
                                  box(
                                    title = "Distribution", status = "danger", solidHeader = TRUE,
                                    collapsible = TRUE, width = 12,
                                    plotOutput("distribusi")
                                  ),
                                  box(
                                    title = "Box Plot", status = "danger", solidHeader = TRUE,
                                    collapsible = TRUE, width = 12,
                                    plotOutput("boxplot")
                                  )
                                )
                        ),
                        # third tab content
                        tabItem(tabName = "Normalize",
                                fluidRow(
                                  box(
                                    title = "Table After Normalize", status = "danger", solidHeader = TRUE,
                                    collapsible = TRUE, width = 12,
                                    dataTableOutput("tableNormalize")
                                  ),
                                  box(
                                    title = "Distribution", status = "danger", solidHeader = TRUE,
                                    collapsible = TRUE, width = 12,
                                    plotOutput("distribusiAfter")
                                  ),
                                  box(
                                    title = "Box Plot", status = "danger", solidHeader = TRUE,
                                    collapsible = TRUE, width = 12,
                                    plotOutput("boxplotAfter")
                                  )
                                )
                        ),
                        # fourth tab content
                        tabItem(tabName = "Optimum",
                                fluidRow(
                                  box(
                                    title = "Method", status = "danger", solidHeader = TRUE,
                                    collapsible = TRUE, width = 12,
                                    selectInput("Method", "Choose a Method:",
                                                list("silhouette", "wss", "gap_stat"), selected = NULL)
                                  ),
                                  box(
                                    title = "Cluster", status = "danger", solidHeader = TRUE,
                                    collapsible = TRUE, width = 12,
                                    plotOutput("methode")
                                  ),
                                  box(
                                    title = "Table Number of Cluster", status = "danger", solidHeader = TRUE,
                                    collapsible = TRUE, width = 12, align="center",
                                    column(width = 2,offset = 1,
                                      tableOutput("listCluster1")
                                    ), 
                                    column(width = 2, offset = 2,
                                           tableOutput("listCluster2")
                                    ), 
                                    column(width = 2, offset = 2,
                                           tableOutput("listCluster3")
                                    )
                                  )
                                )
                        ),
                        # fifth tab content
                        tabItem(tabName = "Visualize",
                                fluidRow(
                                  box(
                                    title = "Cluster", status = "danger", solidHeader = TRUE,
                                    collapsible = TRUE, width = 12,
                                    sliderInput("Number", label = ("Number of Cluster"), min = 1, 
                                                max = 20, value = 2)
                                  ),
                                  box(
                                    title = "Cluster", status = "danger", solidHeader = TRUE,
                                    collapsible = TRUE, width = 12,
                                    plotOutput("Visual")
                                  )
                                )
                        )
                      )
                    )
)

server <- function(input, output) {
  output$contents <- renderDataTable({
    readExcel(input$file1)
  })
  
  output$distribusi <- renderPlot({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df <- column_to_rownames(df, var = "Kota") # cari otomatis
    df <- as.data.frame(df)
    
    df_long <- df %>%                          # Apply pivot_longer function
      pivot_longer(colnames(df)) %>% 
      as.data.frame()
    
    df_density <- ggplot(df_long, aes(x = value)) +    # Draw each column as density
      geom_density() + 
      facet_wrap(~name, labeller = labeller(name = label_wrap_gen(width = 20)))
    df_density
  })
  
  output$boxplot <- renderPlot({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df <- column_to_rownames(df, var = "Kota")
    df <- as.data.frame(df)
    
    df_long <- df %>%                          # Apply pivot_longer function
      pivot_longer(colnames(df)) %>% 
      as.data.frame()
    
    df_boxplot <- ggplot(df_long, aes(x = value)) +    # Draw each column as density
      geom_boxplot() + 
      facet_wrap(~name, labeller = labeller(name = label_wrap_gen(width = 20)))
    df_boxplot
  })
  
  output$tableNormalize <- renderDataTable({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df <- column_to_rownames(df, var = "Kota")
    df <- as.data.frame(df)
    
    df_long <- df %>%                          # Apply pivot_longer function
      pivot_longer(colnames(df)) %>% 
      as.data.frame()
    
    # Nomalize
    df_matrix <- data.matrix(df, rownames.force=nrow(df))
    normalized = normalize.quantiles.robust(df_matrix,
                                            copy=TRUE,
                                            weights=NULL,
                                            remove.extreme="both",
                                            n.remove=3,
                                            use.median=FALSE,
                                            use.log2=TRUE,
                                            keep.names=FALSE)
    
    normalized = as.data.frame(normalized)
    normalized = as.data.frame(scale(normalized))
    
    colnames(normalized)= colnames(df)
    rownames(normalized) = rownames(df)
    normalized
  })
  
  output$distribusiAfter <- renderPlot({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df <- column_to_rownames(df, var = "Kota")
    df <- as.data.frame(df)
    
    df_long <- df %>%                          # Apply pivot_longer function
      pivot_longer(colnames(df)) %>% 
      as.data.frame()
    
    df_matrix <- data.matrix(df, rownames.force=nrow(df))
    normalized = normalize.quantiles.robust(df_matrix,
                                            copy=TRUE,
                                            weights=NULL,
                                            remove.extreme="both",
                                            n.remove=3,
                                            use.median=FALSE,
                                            use.log2=TRUE,
                                            keep.names=FALSE)
    
    normalized = as.data.frame(normalized)
    normalized = as.data.frame(scale(normalized))
    
    colnames(normalized)= colnames(df)
    rownames(normalized) = rownames(df)
    
    normalized_long <- normalized %>% 
      pivot_longer(colnames(normalized)) %>% 
      as.data.frame()
    
    normalized_density <- ggplot(normalized_long, aes(x = value)) + geom_density() + facet_wrap(~name, labeller = labeller(name = label_wrap_gen(width = 20)))
    normalized_density
  })
  
  output$boxplotAfter <- renderPlot({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df <- column_to_rownames(df, var = "Kota")
    df <- as.data.frame(df)
    
    df_long <- df %>%                          # Apply pivot_longer function
      pivot_longer(colnames(df)) %>% 
      as.data.frame()
    
    df_matrix <- data.matrix(df, rownames.force=nrow(df))
    normalized = normalize.quantiles.robust(df_matrix,
                                            copy=TRUE,
                                            weights=NULL,
                                            remove.extreme="both",
                                            n.remove=3,
                                            use.median=FALSE,
                                            use.log2=TRUE,
                                            keep.names=FALSE)
    
    normalized = as.data.frame(normalized)
    normalized = as.data.frame(scale(normalized))
    
    colnames(normalized)= colnames(df)
    rownames(normalized) = rownames(df)
    
    normalized_long <- normalized %>% 
      pivot_longer(colnames(normalized)) %>% 
      as.data.frame()
    
    normalized_density <- ggplot(normalized_long, aes(x = value)) + geom_boxplot() + facet_wrap(~name, labeller = labeller(name = label_wrap_gen(width = 20)))
    normalized_density
  })
  
  output$methode <- renderPlot({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df <- column_to_rownames(df, var = "Kota")
    df <- as.data.frame(df)
    
    df_matrix <- data.matrix(df, rownames.force=nrow(df))
    normalized = normalize.quantiles.robust(df_matrix,
                                            copy=TRUE,
                                            weights=NULL,
                                            remove.extreme="both",
                                            n.remove=3,
                                            use.median=FALSE,
                                            use.log2=TRUE,
                                            keep.names=FALSE)
    
    normalized = as.data.frame(normalized)
    normalized = as.data.frame(scale(normalized))
    
    colnames(normalized)= colnames(df)
    rownames(normalized) = rownames(df)
    
    fviz_nbclust(df, kmeans, method = input$Method, k.max = 20)
  })
  
  output$listCluster1 <- renderTable({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df <- column_to_rownames(df, var = "Kota")
    df <- as.data.frame(df)
    
    df_long <- df %>%                          # Apply pivot_longer function
      pivot_longer(colnames(df)) %>% 
      as.data.frame()
    
    # Nomalize
    df_matrix <- data.matrix(df, rownames.force=nrow(df))
    normalized = normalize.quantiles.robust(df_matrix,
                                            copy=TRUE,
                                            weights=NULL,
                                            remove.extreme="both",
                                            n.remove=3,
                                            use.median=FALSE,
                                            use.log2=TRUE,
                                            keep.names=FALSE)
    
    normalized = as.data.frame(normalized)
    normalized = as.data.frame(scale(normalized))
    
    colnames(normalized)= colnames(df)
    rownames(normalized) = rownames(df)
    
    res<-NbClust(normalized, distance = "euclidean", min.nc=2, max.nc=20, 
                 method = "kmeans", index = "alllong")
    
    t_res = transpose(as.data.frame(res$Best.nc))
    
    colnames(t_res) <- rownames(as.data.frame(res$Best.nc))
    rownames(t_res) <- colnames(as.data.frame(res$Best.nc))
    
    t_res <- t_res[order(t_res$Number_clusters),]
    head(t_res,10)
  },rownames = TRUE)
  
  output$listCluster2 <- renderTable({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df <- column_to_rownames(df, var = "Kota")
    df <- as.data.frame(df)
    
    df_long <- df %>%                          # Apply pivot_longer function
      pivot_longer(colnames(df)) %>% 
      as.data.frame()
    
    # Nomalize
    df_matrix <- data.matrix(df, rownames.force=nrow(df))
    normalized = normalize.quantiles.robust(df_matrix,
                                            copy=TRUE,
                                            weights=NULL,
                                            remove.extreme="both",
                                            n.remove=3,
                                            use.median=FALSE,
                                            use.log2=TRUE,
                                            keep.names=FALSE)
    
    normalized = as.data.frame(normalized)
    normalized = as.data.frame(scale(normalized))
    
    colnames(normalized)= colnames(df)
    rownames(normalized) = rownames(df)
    
    res<-NbClust(normalized, distance = "euclidean", min.nc=2, max.nc=20, 
                 method = "kmeans", index = "alllong")
    
    t_res = transpose(as.data.frame(res$Best.nc))
    
    colnames(t_res) <- rownames(as.data.frame(res$Best.nc))
    rownames(t_res) <- colnames(as.data.frame(res$Best.nc))
    
    t_res <- t_res[order(t_res$Number_clusters),]
    t_res %>% slice(11:20)
  },rownames = TRUE)
  
  output$listCluster3 <- renderTable({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df <- column_to_rownames(df, var = "Kota")
    df <- as.data.frame(df)
    
    df_long <- df %>%                          # Apply pivot_longer function
      pivot_longer(colnames(df)) %>% 
      as.data.frame()
    
    # Nomalize
    df_matrix <- data.matrix(df, rownames.force=nrow(df))
    normalized = normalize.quantiles.robust(df_matrix,
                                            copy=TRUE,
                                            weights=NULL,
                                            remove.extreme="both",
                                            n.remove=3,
                                            use.median=FALSE,
                                            use.log2=TRUE,
                                            keep.names=FALSE)
    
    normalized = as.data.frame(normalized)
    normalized = as.data.frame(scale(normalized))
    
    colnames(normalized)= colnames(df)
    rownames(normalized) = rownames(df)
    
    res<-NbClust(normalized, distance = "euclidean", min.nc=2, max.nc=20, 
                 method = "kmeans", index = "alllong")
    
    t_res = transpose(as.data.frame(res$Best.nc))
    
    colnames(t_res) <- rownames(as.data.frame(res$Best.nc))
    rownames(t_res) <- colnames(as.data.frame(res$Best.nc))
    
    t_res <- t_res[order(t_res$Number_clusters),]
    tail(t_res,10)
  },rownames = TRUE)
  
  output$Visual <- renderPlot({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df <- column_to_rownames(df, var = "Kota")
    df <- as.data.frame(df)
    
    df_matrix <- data.matrix(df, rownames.force=nrow(df))
    normalized = normalize.quantiles.robust(df_matrix,
                                            copy=TRUE,
                                            weights=NULL,
                                            remove.extreme="both",
                                            n.remove=3,
                                            use.median=FALSE,
                                            use.log2=TRUE,
                                            keep.names=FALSE)
    
    normalized = as.data.frame(normalized)
    normalized = as.data.frame(scale(normalized))
    
    colnames(normalized)= colnames(df)
    rownames(normalized) = rownames(df)
    
    normalized_long <- normalized %>% 
      pivot_longer(colnames(normalized)) %>% 
      as.data.frame()
    
    k2 <- kmeans(normalized, centers=input$Number, nstart = 100, iter.max=1000, algorithm ='Lloyd')
    
    fviz_cluster(k2, data = normalized, repel=TRUE)
    # fviz_cos2(prcomp(normalized,  scale = TRUE), choice="var", axes = 2, sort.val ="asc")
  })
}

shinyApp(ui, server)
