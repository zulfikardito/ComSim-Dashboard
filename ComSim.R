# Install required packages
options(repos = list(CRAN="http://cran.rstudio.com/"))
install.packages(c("shiny", "DT", "leaflet", "ggplot2", "dplyr", "tidyverse", "shinyWidgets"), repos = "http://cran.us.r-project.org")

# Load necessary packages
library(shiny)
library(DT)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(shinyWidgets)

# Helper Function

# Function for calculating Shannon Diversity Index for a single row
calculate_shannon_row <- function(values) {
  # Remove zero values, as they don't contribute to Shannon Index
  values <- values[values > 0]
  
  # If no non-zero values exist, return 0
  if (length(values) == 0) {
    return(0)
  }
  
  total <- sum(values)
  proportions <- values / total
  shannon_index <- -sum(proportions * log(proportions))
  
  return(shannon_index)
}

# Function to normalise scores (negative scores)
normalise_negative <- function(score, a = -1, b = 0) {
  x_min <- min(score, na.rm = TRUE)
  x_max <- max(score, na.rm = TRUE)
  
  if (x_min == x_max) {
    return(rep(score, length(score)))  # Return 'a' for all values if they're the same
  }
  
  Normalised <- a + ((score - x_min) * (b - a)) / (x_max - x_min)
  return(Normalised)
}

# Function to normalise positive scores
normalise_positive <- function(score, min_score = 0, max_score = 1) {
  x_min <- min(score, na.rm = TRUE)
  x_max <- max(score, na.rm = TRUE)
  
  if (x_min == x_max) {
    return(rep(score, length(score)))  # Return 'min_score' if all values are equal
  }
  
  Normalised <- min_score + ((score - x_min) * (max_score - min_score)) / (x_max - x_min)
  return(Normalised)
}


# User Interface for the "Shiny" Dashboard
ui <- fluidPage(
  titlePanel("ComSim: Neighbourhood Suitability Assessment Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload CSV file", accept = ".csv"),
      actionButton("check_suitability", "Check Suitability"),
      downloadButton("download_data", "Download Results"),
      selectInput("category_view", "Select Suitability Category to View", 
                  choices = c("All", "Very Suitable", "Suitable", "Less Suitable", "Not Suitable", "Left Behind")),
      helpText("Download an example CSV file:"),
      downloadButton("download_example", "Download Example CSV")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("neighbourhoodMap")),
        tabPanel("Table", DT::dataTableOutput("resultTable")),
        tabPanel("Suitability Graph", plotOutput("suitabilityGraph")),
        tabPanel("Data Graph", plotOutput("dimensionScoreGraph"))
      )
    )
  )
)

# Server logic for the "Shiny" Dashboard
server <- function(input, output, session) {
  
  
  # Reactive to read data from uploaded CSV
  neighbourhood_data <- reactive({
    req(input$datafile)
    data <- read.csv(input$datafile$datapath)

    # Ensure the file has the necessary columns
    required_columns <- c("Neighbourhood", "Longitude", "Latitude", "Social_Activities", "Helping_Others", "Social_Bonding",
                          "Community_Figures", "Inhabitants_Mindset", "Active_Citizens", 
                          "Age_0_14", "Age_15_24", "Age_25_44", "Age_45_59",
                          "Age_60_74", "Age_75_plus",
                          "No_Education", "Non_Higher_Education", "Higher_Education", 
                          "Unemployed", "Civic_Servant", "Company_Employee", 
                          "Professional", "Businessman", "Labourer", "Informal_Worker", 
                          "Student", "Others", 
                          "Proximity", "Settlement_Dominance", 
                          "Engagement_Capacity", "Leadership_Performance", 
                          "Commitment", "Regeneration_Procedures", "Innovation_Capacity", "Government_Perspective", "Government_Policy",
                          "Inhabitant_Conflicts", "Meeting_Restrictions", "Digital_Tech_Adoption")
    
    # Check if all required columns are present
    missing_columns <- setdiff(required_columns, colnames(data))
    
    if (length(missing_columns) > 0) {
      stop(paste("The CSV file is missing the following columns:", paste(missing_columns, collapse = ", ")))
    }
    
    return(data)
    
  })
  
  # Example CSV file download
  output$download_example <- downloadHandler(
    filename = function() {
      paste("example_neighbourhood_data.csv")
      },
    content = function(file) {
      example_neighbourhood_data <- data.frame(
        Neighbourhood = c("Neighbourhood A", "Neighbourhood B"),
        Longitude = c(-122.4194, -118.2437),
        Latitude = c(37.7749, 34.0522),
        Social_Activities = c(2, 5),
        Helping_Others = c(TRUE, FALSE),
        Social_Bonding = c(TRUE, TRUE),
        Community_Figures = c(TRUE, FALSE),
        Inhabitants_Mindset = c(TRUE, FALSE),
        Active_Citizens = c(TRUE, FALSE),
        Age_0_14 = c(300, 250),
        Age_15_24 = c(200, 220),
        Age_25_44 = c(500, 480),
        Age_45_59 = c(350, 370),
        Age_60_74 = c(150, 160),
        Age_75_plus = c(100, 120),
        No_Education = c(50, 40),
        Non_Higher_Education = c(700, 720),
        Higher_Education = c(500, 460),
        Unemployed = c(300, 250),
        Civic_Servant = c(50, 60),
        Company_Employee = c(400, 450),
        Professional = c(100, 90),
        Businessman = c(80, 70),
        Labourer = c(60, 55),
        Informal_Worker = c(90, 85),
        Student = c(50, 40),
        Others = c(20, 25),
        Proximity = c(500, 900),
        Settlement_Dominance = c(60, 40),
        Engagement_Capacity = c(TRUE, FALSE),
        Leadership_Performance = c(TRUE, FALSE),
        Commitment = c(TRUE, FALSE),
        Regeneration_Procedures = c(TRUE, FALSE),
        Innovation_Capacity = c(TRUE, FALSE),
        Government_Perspective = c(TRUE, FALSE),
        Government_Policy = c(TRUE, FALSE),
        Inhabitant_Conflicts = c(TRUE, FALSE),
        Meeting_Restrictions = c(FALSE, TRUE),
        Digital_Tech_Adoption = c(TRUE, FALSE)
      )
      write.csv(example_neighbourhood_data, file)
    }
  )
  

  # Reactive: calculate the suitability when button is clicked based on input data
  calcResults <- reactive({
    req(neighbourhood_data())
    
    df <- neighbourhood_data()

    print(colnames(df))
    
    # Calculate indicator scores based on each dimension

    # Social Resources Dimension Scoring
    df <- df %>%
      mutate(
        # Social resources
        Social_Activities_Score = ifelse(Social_Activities <= 3, 0.25, -0.25),
        Helping_Score = ifelse(Helping_Others == TRUE, 0.7, -0.7),
        Bonding_Score = ifelse(Social_Bonding == TRUE, 0.2, -0.2),
        Community_Figures_Score = ifelse(Community_Figures == TRUE, 0.25, -0.25),
        Mindset_Score = ifelse(Inhabitants_Mindset == TRUE, 0.25, -0.25),
        Active_Citizens_Score = ifelse(Active_Citizens == TRUE, 0.5, -0.5)
      )

        # Heterogeneity score (Shannon diversity) per row
    df <- df %>%
      rowwise() %>%
      mutate(
        sociodemographic_heterogeneity = calculate_shannon_row(c(Age_0_14, Age_15_24, 
                                                                 Age_25_44, Age_45_59, 
                                                                 Age_60_74, Age_75_plus,
                                                                 No_Education, 
                                                                 Non_Higher_Education, 
                                                                 Higher_Education,
                                                                 Unemployed, Civic_Servant, 
                                                                 Company_Employee, 
                                                                 Professional,
                                                                 Businessman, Labourer, 
                                                                 Informal_Worker, Student, 
                                                                 Others)) / log(324),
        
        Heterogeneity_Score = ifelse(sociodemographic_heterogeneity >= 0.40, 0.125, -0.125),
        
        Social_Resources = Social_Activities_Score + Helping_Score + Bonding_Score + 
          Community_Figures_Score + Mindset_Score + Active_Citizens_Score + 
          Heterogeneity_Score
      ) %>%
      ungroup()  # Ensure that row-wise grouping is undone after calculation
    
    # Spatial Characteristics Dimension Scoring
    df <- df %>%
      mutate(
        Proximity_Score = ifelse(Proximity <= 800, 0.25, -0.25),
        Settlement_Domination_Score = ifelse(Settlement_Dominance > 50, 0.125, -0.125)
      )
    
    # Leadership Capacity Dimension Scoring
    df <- df %>%
      mutate(
        Engagement_Score = ifelse(Engagement_Capacity == TRUE, 0.25, -0.25),
        Performance_Score = ifelse(Leadership_Performance == TRUE, 0.5, -0.5),
        Commitment_Score = ifelse(Commitment == TRUE, 0.5, -0.5),
        Regeneration_Score = ifelse(Regeneration_Procedures == TRUE, 0.25, -0.25),
        Innovation_Score = ifelse(Innovation_Capacity == TRUE, 0.125, -0.125)
      )
    
    # Government Position Dimension Scoring
    df <- df %>%
      mutate(
        Government_Perspective_Score = ifelse(Government_Perspective == TRUE, 0.125, -0.125),
        Government_Policy_Score = ifelse(Government_Policy == TRUE, 0.125, -0.125)
      )
    
    # Disruptors and Bouncebacks
    df <- df %>%
      mutate(
        Conflict_Score = ifelse(Inhabitant_Conflicts == TRUE, -0.125, 0.125),
        Meeting_Restrictions_Score = ifelse(Meeting_Restrictions == TRUE, -0.25, 0.25),
        Tech_Adoption_Score = ifelse(Digital_Tech_Adoption == TRUE, 0.125, -0.125)
      )
 
    # Calculate final dimension scores
    df <- df %>%
      mutate(
        Social_Dimension = rowSums(select(., Social_Activities_Score, Helping_Score, Bonding_Score, Community_Figures_Score, Mindset_Score, Active_Citizens_Score, Heterogeneity_Score), na.rm = TRUE),
        Spatial_Dimension = rowSums(select(., Proximity_Score, Settlement_Domination_Score), na.rm = TRUE),
        Leadership_Dimension = rowSums(select(., Engagement_Score, Performance_Score, Commitment_Score, Regeneration_Score, Innovation_Score), na.rm = TRUE),
        Government_Dimension = rowSums(select(., Government_Perspective_Score, Government_Policy_Score), na.rm = TRUE),
        Disruptor_Dimension = rowSums(select(., Conflict_Score, Meeting_Restrictions_Score), na.rm = TRUE),
        Bounceback_Dimension = rowSums(select(., Tech_Adoption_Score), na.rm = TRUE)
      )
      
    
    # Check values before Normalisation
    print(head(df$Social_Dimension))  # Check values before Normalisation
    
    # Normalise score of each indicator
    df <- df %>%
      mutate(
        Social_Activities_Score_Normalised = ifelse(Social_Activities_Score < 0, normalise_negative(Social_Activities_Score), normalise_positive(Social_Activities_Score)),
        Helping_Score_Normalised = ifelse(Helping_Score < 0, normalise_negative(Helping_Score), normalise_positive(Helping_Score)),
        Bonding_Score_Normalised = ifelse(Bonding_Score < 0, normalise_negative(Bonding_Score), normalise_positive(Bonding_Score)),
        Community_Figures_Score_Normalised = ifelse(Community_Figures_Score < 0, normalise_negative(Community_Figures_Score), normalise_positive(Community_Figures_Score)),
        Mindset_Score_Normalised = ifelse(Mindset_Score < 0, normalise_negative(Mindset_Score), normalise_positive(Mindset_Score)),
        Active_Citizens_Score_Normalised = ifelse(Active_Citizens_Score < 0, normalise_negative(Active_Citizens_Score), normalise_positive(Active_Citizens_Score)),
        Heterogeneity_Score_Normalised = ifelse(Heterogeneity_Score < 0, normalise_negative(Heterogeneity_Score), normalise_positive(Heterogeneity_Score)),
        Proximity_Score_Normalised = ifelse(Proximity_Score < 0, normalise_negative(Proximity_Score), normalise_positive(Proximity_Score)),
        Settlement_Domination_Score_Normalised = ifelse(Settlement_Domination_Score < 0, normalise_negative(Settlement_Domination_Score), normalise_positive(Settlement_Domination_Score)),
        Engagement_Score_Normalised = ifelse(Engagement_Score < 0, normalise_negative(Engagement_Score), normalise_positive(Engagement_Score)),
        Performance_Score_Normalised = ifelse(Performance_Score < 0, normalise_negative(Performance_Score), normalise_positive(Performance_Score)),
        Commitment_Score_Normalised = ifelse(Commitment_Score < 0, normalise_negative(Commitment_Score), normalise_positive(Commitment_Score)),
        Regeneration_Score_Normalised = ifelse(Regeneration_Score < 0, normalise_negative(Regeneration_Score), normalise_positive(Regeneration_Score)),
        Innovation_Score_Normalised = ifelse(Innovation_Score < 0, normalise_negative(Innovation_Score), normalise_positive(Innovation_Score)),
        Government_Perspective_Score_Normalised = ifelse(Government_Perspective_Score < 0, normalise_negative(Government_Perspective_Score), normalise_positive(Government_Perspective_Score)),
        Government_Policy_Score_Normalised = ifelse(Government_Policy_Score < 0, normalise_negative(Government_Policy_Score), normalise_positive(Government_Policy_Score)),
        Conflict_Score_Normalised =  ifelse(Conflict_Score < 0, normalise_negative(Conflict_Score), normalise_positive(Conflict_Score)),
        Meeting_Restrictions_Score_Normalised =  ifelse(Meeting_Restrictions_Score < 0, normalise_negative(Meeting_Restrictions_Score), normalise_positive(Meeting_Restrictions_Score)),
        Tech_Adoption_Score_Normalised = ifelse(Tech_Adoption_Score < 0, normalise_negative(Tech_Adoption_Score), normalise_positive(Tech_Adoption_Score))
      )
    
    
    # Calculate final normalised dimension scores
    df <- df %>%
      mutate(
        Social_Dimension_Normalised = rowMeans(select(., Social_Activities_Score_Normalised, Helping_Score_Normalised, Bonding_Score_Normalised, Community_Figures_Score_Normalised, Mindset_Score_Normalised, Active_Citizens_Score_Normalised, Heterogeneity_Score_Normalised), na.rm = TRUE),
        Spatial_Dimension_Normalised = rowMeans(select(., Proximity_Score_Normalised, Settlement_Domination_Score_Normalised), na.rm = TRUE),
        Leadership_Dimension_Normalised = rowMeans(select(., Engagement_Score_Normalised, Performance_Score_Normalised, Commitment_Score_Normalised, Regeneration_Score_Normalised, Innovation_Score_Normalised), na.rm = TRUE),
        Government_Dimension_Normalised = rowMeans(select(., Government_Perspective_Score_Normalised, Government_Policy_Score_Normalised), na.rm = TRUE),
        Disruptor_Dimension_Normalised = rowMeans(select(., Conflict_Score_Normalised, Meeting_Restrictions_Score_Normalised), na.rm = TRUE),
        Bounceback_Dimension_Normalised = rowMeans(select(., Tech_Adoption_Score_Normalised), na.rm = TRUE)
      )
    
        
    # Check Normalised values
    print(head(df$Social_Dimension_Normalised))  # Check Normalised values
    
    # Calculate suitability score as the average of all Normalised dimension scores
    df <- df %>%
      mutate(
        Suitability_Score = rowMeans(
          cbind(Social_Dimension_Normalised, 
            Spatial_Dimension_Normalised, 
            Leadership_Dimension_Normalised, 
            Government_Dimension_Normalised,
            Disruptor_Dimension_Normalised,
            Bounceback_Dimension_Normalised),
          na.rm = TRUE)  # Exclude NAs
        ) %>%
        
        # Category based on suitability score
        mutate(category = case_when(
          Suitability_Score >= 0.8 ~ "Very Suitable",
          Suitability_Score >= 0.5 ~ "Suitable",
          Suitability_Score >= 0.0000000009 ~ "Less Suitable",
          Suitability_Score == 0 ~ "Not Suitable",
          Suitability_Score < 0 ~ "Left Behind"
        ))
    
    return(df) # Return the updated dataframe
    
  })
  
  # Reactive: Filter data based on selected category
  filteredData <- reactive({
    req(calcResults())
    
    df <- calcResults()
    
    # Filter based on the selected category
    if (input$category_view != "All") {
      df <- df %>% filter(category == input$category_view)
    }
    
    return(df)
  })
  
    # Output: Display on map
    output$neighbourhoodMap <- renderLeaflet({
      df <- filteredData()
      leaflet(df) %>%
        addProviderTiles("OpenStreetMap") %>%
        addCircleMarkers(~Longitude, ~Latitude,
          color = ~case_when(
            category == "Very Suitable" ~ "darkgreen",
            category == "Suitable" ~ "lightgreen",
            category == "Less Suitable" ~ "orange",
            category == "Not Suitable" ~ "red",
            category == "Left Behind" ~ "darkred"
          ),
          radius = 8,
          fillOpacity = 0.7,
          popup = ~paste0("Neighbourhood:", Neighbourhood, "<br>",
                          "Suitability:", category, "<br>",
                          "Suitability Score:", round(Suitability_Score, 2), "<br>",
                          "Social Resources:", Social_Dimension_Normalised, "<br>",
                          "Spatial Characteristics:", Spatial_Dimension_Normalised, "<br>",
                          "Leadership Capacity:", Leadership_Dimension_Normalised, "<br>",
                          "Goverment Position:", Government_Dimension_Normalised, "<br>",
                          "Disruptors:", Disruptor_Dimension_Normalised, "<br>",
                          "Bounceback:", Bounceback_Dimension_Normalised, "<br>"
                          )) %>%
          addLegend("bottomright", 
                    colors = c("darkgreen",  "lightgreen", "orange", "red", "darkred"),
                    labels = c("very suitable", "suitable", "less suitable", "not suitable", "left behind"),
                    title = "Suitability Category",
                    opacity = 50)
  })
    
    # Output: Show data table
    output$resultTable <- renderDataTable({
      datatable(filteredData())
    })
    
    # Named vector for suitability category colors
    suitability_colors <- c(
      "Left Behind" = "darkred",
      "Not Suitable" = "red",
      "Less Suitable" = "orange",
      "Suitable" = "lightgreen",
      "Very Suitable" = "darkgreen"
    )
    
    # Desired order of suitability categories
    suitability_levels <- c("Very Suitable", "Suitable", "Less Suitable", "Not Suitable", "Left Behind")
    
    # Output: Plot filtered suitability distribution
    output$suitabilityGraph <- renderPlot({
      # Convert category to factor with specified levels
      filteredData <- filteredData() %>%
        mutate(category = factor(category, levels = suitability_levels))
      
      ggplot(data = filteredData, aes(x = category, fill = category)) +
        geom_bar() +
        scale_fill_manual(values = suitability_colors) +  # Use the named color vector
        labs(title = "Neighbourhood Suitability Distribution", x = "Suitability Category", y = "Count") +
        theme_minimal()
    })
    
    # Output: Plot filtered suitability distribution (same as before)
    output$suitabilityGraph <- renderPlot({
      filteredData <- filteredData() %>%
        mutate(category = factor(category, levels = suitability_levels))
      
      ggplot(data = filteredData, aes(x = category, fill = category)) +
        geom_bar() +
        scale_fill_manual(values = suitability_colors) +
        labs(title = "Neighbourhood Suitability Distribution", x = "Suitability Category", y = "Count") +
        theme_minimal()
    })
    
    # Output: Plot the distribution of scores for each dimension
    output$dimensionScoreGraph <- renderPlot({
      # Assuming the Normalised scores are in columns: social_score, spatial_score, leadership_score, gov_position_score
      longData <- filteredData() %>%
        select(Social_Dimension_Normalised, Spatial_Dimension_Normalised, Leadership_Dimension_Normalised, Government_Dimension_Normalised, Disruptor_Dimension_Normalised, Bounceback_Dimension_Normalised) %>%
        gather(key = "dimension", value = "score")  # Reshape data to long format for ggplot

      ggplot(data = longData, aes(x = score, fill = dimension)) +
        geom_histogram(binwidth = 0.1, position = "dodge", alpha = 0.7) +  # You can adjust binwidth
        scale_fill_manual(values = c("blue", "green", "purple", "orange","cyan","pink")) +  # Customize colors
        labs(title = "Score Distribution by Dimension", x = "Normalised Score", y = "Neighbourhood Count") +
        theme_minimal()
    })
    

    
    # Output: Download the data as CSV
    output$download_data <- downloadHandler(
      filename = function() {
        paste("neighbourhood_suitability_data", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filteredData(), file)
      }
    )
}
    
# Function to Run the "Shiny" Dashboard
shinyApp(ui, server)

