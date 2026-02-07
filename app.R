library(shiny)
library(plotly)
library(tidyquant)
library(tidyverse)

#User Interface (The "Frontend")
ui <- fluidPage(
  

  titlePanel(" Stock Portfolio Analyzer"),
  
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter stock symbols separated by commas."),
      
      # Input: Ticker Symbols
      textInput("tickers", "Stock Symbols:", 
                value = "AAPL, GOOG, TSLA, NFLX"),
      
      # Input: Date Range
      dateRangeInput("dates", "Date Range:",
                     start = "2020-01-01",
                     end   = Sys.Date()),
      
      # Action Button (Crucial!)
      actionButton("analyze", "Analyze Portfolio", 
                   class = "btn-primary") # Makes it blue
    ),
    
    # Main Panel (Results)
    mainPanel(
      tabsetPanel(
        tabPanel("Growth Chart", plotlyOutput("growth_plot")),
        tabPanel("Risk Metrics", tableOutput("risk_table"))
      )
    )
  )
)

# Server Logic (The "Backend")
server <- function(input, output) {
  
  # Reactive Data: Only runs when "Analyze" is clicked
  portfolio_data <- eventReactive(input$analyze, {
    
    # 1. Get Tickers from text box
    # "AAPL, GOOG" -> c("AAPL", "GOOG")
    symbols <- str_split(input$tickers, ",")[[1]] %>% str_trim()
    
    # 2. Download Data 
    withProgress(message = 'Downloading Data...', value = 0.5, {
      prices <- tq_get(symbols, 
                       get  = "stock.prices", 
                       from = input$dates[1], 
                       to   = input$dates[2])
    })
    
    # 3. Calculate Returns 
    returns <- prices %>%
      group_by(symbol) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "daily", 
                   type       = "log",
                   col_rename = "daily.returns") %>%
      ungroup()
    
    return(returns)
  })
  
  # Output 1: Interactive Plot
  output$growth_plot <- renderPlotly({
    req(portfolio_data()) # Stop if data isn't ready
    
    # Calculate Growth of $1
    growth_data <- portfolio_data() %>%
      group_by(symbol) %>%
      mutate(growth = exp(cumsum(daily.returns))) %>%
      ungroup()
    
    # Plot
    p <- ggplot(growth_data, aes(x = date, y = growth, color = symbol)) +
      geom_line(size = 1) +
      labs(title = "Growth of $1 Investment",
           y = "Investment Value ($)", x = "") +
      theme_tq() +
      scale_color_tq()
    
    ggplotly(p) # Makes it interactive!
  })
  
  # Output 2: Risk Table
  output$risk_table <- renderTable({
    req(portfolio_data())
    
    # Calculate Risk Metrics (Your Step 5 code!)
    portfolio_data() %>%
      tq_performance(Ra = daily.returns, 
                     performance_fun = table.AnnualizedReturns)
  })
}

# --- 4. Run the App ---
shinyApp(ui = ui, server = server)