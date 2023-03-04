
library(plotly)

EIA_dayzer_merge <- read.csv("~/Desktop/Winter 2023/GSB 521/Week 2/congressional-analysis-main/Engie-Modeling-SPP-Group/EIA_dayzer_merge.csv")

Holcomb <- EIA_dayzer_merge[EIA_dayzer_merge$plant_id ==108, ]

x <- EIA_dayzer_merge$month 

fig <- fig %>% layout(title = "Actual Generation MWH vs. DZR Predicted MWH",
                      xaxis = list(title = "Months"),
                      yaxis = list (title = "Generation in MWH"))

fig




# Load the necessary libraries
library(plotly)
library(shiny)

# Create a sample data frame
df <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"),
  Name1 = c(10, 15, 20, 25, 30, 35),
  Name2 = c(5, 10, 15, 20, 25, 30),
  Name3 = c(2, 4, 6, 8, 10, 12)
)

EIA_dayzer_merge <- read.csv("~/Desktop/Winter 2023/GSB 521/Week 2/congressional-analysis-main/Engie-Modeling-SPP-Group/EIA_dayzer_merge.csv")

Holcomb <- EIA_dayzer_merge[EIA_dayzer_merge$plant_id ==108, ]


########################################################################################################################################
#Line Graph
###########

# Define the UI for the shiny app
ui <- fluidPage(
  
  # Add a checkbox group to select the names to display
  checkboxGroupInput("names", "Select Names:", 
                     choices = colnames(df)[-1], selected = colnames(df)[-1]),
  
  # Add the plotly graph
  plotlyOutput("plot")
)

# Define the server for the shiny app
server <- function(input, output) {
  
  # Generate the plotly graph
  output$plot <- renderPlotly({
    
    # Filter the data based on the selected names
    data <- df[, c("Month", input$names)]
    
    # Convert the data to long format
    data_long <- tidyr::gather(data, key = "Name", value = "MWH", -Month)
    
    # Create the plotly graph
    plot <- plot_ly(data_long, x = ~Month, y = ~MWH, color = ~Name, type = "scatter", mode = "lines")
    
    # Add a title and axis labels
    plot <- plot %>% layout(title = "Monthly MWH by Name",
                            xaxis = list(title = "Month"),
                            yaxis = list(title = "MWH"))
    
    # Return the plotly graph
    return(plot)
  })
}

# Run the shiny app
shinyApp(ui, server)

########################################################################################################################################
#Bar Graph
##########
# Create a sample data frame
df <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"),
  Name1 = c(10, 15, 20, 25, 30, 35),
  Name2 = c(5, 10, 15, 20, 25, 30),
  Name3 = c(2, 4, 6, 8, 10, 12)
)

# Define the UI for the shiny app
ui <- fluidPage(
  
  # Add a checkbox group to select the names to display
  checkboxGroupInput("names", "Select Names:", 
                     choices = colnames(df)[-1], selected = colnames(df)[-1]),
  
  # Add the plotly graph
  plotlyOutput("plot")
)

# Define the server for the shiny app
server <- function(input, output) {
  
  # Generate the plotly graph
  output$plot <- renderPlotly({
    
    # Filter the data based on the selected names
    data <- df[, c("Month", input$names)]
    
    # Convert the data to long format
    data_long <- tidyr::gather(data, key = "Name", value = "MWH", -Month)
    
    # Create the plotly graph
    plot <- plot_ly(data_long, x = ~Month, y = ~MWH, color = ~Name, type = "bar")
    
    # Add a title and axis labels
    plot <- plot %>% layout(title = "Monthly MWH by Name",
                            xaxis = list(title = "Month"),
                            yaxis = list(title = "MWH"))
    
    # Return the plotly graph
    return(plot)
  })
}

# Run the shiny app
shinyApp(ui, server)











