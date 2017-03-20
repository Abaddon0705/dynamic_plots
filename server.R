library(shiny)
library(plotly)
library(moments)

server <- function(input, output) {
  
  # Read and transform data
  df <- data.frame(read.csv(file = "D:/Users/acibis/Desktop/dynamic_histogram/dji.csv", sep = ",", header = TRUE))
  
  # Generate statistic magic values
  for (i in 3000:650){
    n = i-649
    df$means[i] <- mean(df$dji_p[n:i])
  }
  
  for (i in 1:649){
    df$means[i] <- mean(df$dji_p[1:650])
  }
  
  
  df$means <- format(round(df$means, 2), nsmall = 2)
  
  
  
  # render 
  output$plot <- renderPlotly({ 
    
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Stopa zwrotu"
    )
    p <- plot_ly() %>%
      add_lines(data = df, x = ~Data, y = ~dji_p, name = "cena", text = ~paste("srednia: ", df$means)) %>%
      add_lines(data = df, x = ~Data, y = ~dji_r, name = "stopa zwrotu", yaxis = "y2") %>%
      layout(title = "Cena i stopa zwrotu", yaxis2 = ay)
  })
  

  output$plot2 <- renderPlotly({
    d <- event_data("plotly_hover")
    b <- d$pointNumber[1]
    e <- b+649
    rangy <- (df$dji_r[b:e])
    if (b > 649){ 
      plot_ly(x=rangy,type = "histogram")
    }
  })
  
}