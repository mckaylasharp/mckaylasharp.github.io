
library(shiny)
library(ggplot2)
df <- read.csv("C:/Users/15209/Desktop/UTD/Fall 2023/Data Visualization/2021.csv")

ui <- fluidPage(
  titlePanel("Hackaton 2"),
  sidebarLayout(
    sidebarPanel(
      selectInput("app_version", "Select a Graph", 
                  choices = c("Bubble Chart", "Bar Chart", "Scatter Chart")),
      conditionalPanel(
        condition = "input.app_version == 'Scatter Chart'",
        selectInput("xvar", "X-axis variable", choices = names(USArrests)),
        selectInput("yvar", "Y-axis variable", choices = names(USArrests))
      ),
      conditionalPanel(
        condition = "input.app_version == 'Bar Chart'",
        sliderInput("numRanges", "Number of Groups:", min = 1, max = 20, value = 8)
      ),
      conditionalPanel(
        condition = "input.app_version == 'Bubble Chart'",
        selectInput("xvar3", "X-axis variable", choices = setdiff(names(USArrests), "UrbanPop")),
        selectInput("yvar3", "Y-axis variable", choices = setdiff(names(USArrests), "UrbanPop"))
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.app_version == 'Scatter Chart'",
        plotOutput("scatterplot1")
      ),
      conditionalPanel(
        condition = "input.app_version == 'Bar Chart'",
        plotOutput("barPlot")
      ),
      conditionalPanel(
        condition = "input.app_version == 'Bubble Chart'",
        plotOutput("bubblechart")
      )
    )
  )
)

# Define the server function
server <- function(input, output) {
  # Scatter
  output$scatterplot1 <- renderPlot({
    if (input$app_version == "Scatter Chart") {
      ggplot(USArrests, aes_string(x = input$xvar, y = input$yvar)) +
        geom_point()
    }
  })
  
  # Bars
  plotData <- reactive({
    num_ranges <- input$numRanges
    
    # Calculate the minimum and maximum Trade.Value
    min_trade_value <- min(df$Trade.Value)
    max_trade_value <- max(df$Trade.Value)
    
    # Create custom ranges with the same distance from each other
    range_width <- (max_trade_value - min_trade_value) / num_ranges
    custom_ranges <- seq(min_trade_value, max_trade_value, by = range_width)
    
    # Use cut to categorize the Trade.Value into the custom ranges
    df$Trade.Value_Range <- cut(df$Trade.Value, breaks = custom_ranges, include.lowest = TRUE)
    
    # Count the number of countries in each range and convert to a data frame
    counts <- as.data.frame(table(df$Trade.Value_Range))
    
    return(counts)
  })
  
  # Bubble
  output$bubblechart <- renderPlot({
    if (input$app_version == "Bubble Chart") {
      x <- input$xvar3
      y <- input$yvar3
      
      ggplot(USArrests, aes_string(x = x, y = y, size = "UrbanPop")) +
        geom_point(color = "red") +
        labs(x = x, y = y) +
        scale_size_continuous(name = "Urban Population")
    }
  })
  
  # Render the bar plot using ggplot (easier imho)
  output$barPlot <- renderPlot({
    if (input$app_version == "Bar Chart") {
      ggplot(data = plotData(), aes(x = Var1, y = Freq)) +
        geom_bar(stat = "identity", fill = "purple", color = "black") +
        labs(title = "Number of Countries - Trade Value with Italy",
             x = "Trade value Groups",
             y = "Number of Countries") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        geom_text(aes(label = Freq), vjust = -0.5)
    }
  })
}

# Create the Shiny app
shinyApp(ui = ui, server = server)
