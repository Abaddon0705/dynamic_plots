library(shiny)
library(plotly)


server <- function(input, output) {
  # Read data
  monthly <- read.csv(file="monthly.csv", sep=",", header = TRUE)
  annual <- read.csv(file="annual.csv", sep=",", header = TRUE)
  
  monthly$Month <- factor(monthly$Month, levels = c("Jan", "Feb", "Mar", 
                                                    "Apr", "May", "Jun", 
                                                    "Jul", "Aug", "Sep", 
                                                    "Oct", "Nov", "Dec"))

  output$plot <- renderPlotly({
    plot_ly(annual, x=~Year, y=~J.D,type = "scatter", mode="lines")
    })
  
  output$plot2 <- renderPlotly({
    mouse_event <- event_data("plotly_hover")
    year <- mouse_event[3]
    monthly_subset <- monthly[monthly$Year==year$x,]
    plot_ly(monthly_subset, x=~Month, y=~Deviation, type = "scatter", mode="lines + points")
  })
}