library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)
library(forcats)

df <- WA.Housing
df$date <- format(as.Date(df$date, format="%m/%d/%Y"), "%Y-%m-%d")
df$date <- as.Date(df$date)

ui = fluidPage(
  theme = shinytheme("superhero"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "city_in", label="City", choices = c("All", sort(unique(df$city))), selected="All")
    ),
    mainPanel(
      plotlyOutput(outputId = "price"),
      br(),
      plotlyOutput(outputId = "bedrooms"),
      br(),
      plotlyOutput(outputId = "bathrooms")
    )
  )
)

server = function(input, output) {
  output$price = renderPlotly({
    df = df
    
    if(input$city_in != "All") {
      df = df[df$city == input$city_in,]
    }
    
    df %>% 
      plot_ly(x = ~date, y = ~price, type="scatter", mode="markers", source="price") %>% 
      layout(title = "price vs date",
             xaxis = list(title = "date"),
             yaxis = list(title = "price"),
             margin = list(l = 50, r = 50, b = 50, t = 50),
             dragmode = "select")
  })
  
  output$bedrooms = renderPlotly({
    e_dat1 = event_data("plotly_selected", source="price")
    e_dat2 = event_data("plotly_click", source="price")
    
    df = df
    
    if(input$city_in != "All") {
      df = df[df$city == input$city_in,]
    }
    
    if(!is.null(e_dat1)) {
      df = df[e_dat1$pointNumber + 1,]
    } else if(!is.null(e_dat2)) {
      df = df[e_dat2$pointNumber + 1,]
    }
    
    df %>% 
      plot_ly(x = ~bedrooms, type="histogram") %>% 
      layout(title = "Number of houses by number of Bedrooms",
             xaxis = list(title = "Bedrooms"),
             margin = list(l = 50, r = 50, b = 50, t = 50))
  })
  
 }

shinyApp(ui = ui, server = server)