# --- 🚀 QUANTDASH: INSTITUTIONAL EDITION (Final v2) ---
library(shiny)
library(plotly)
library(tidyquant)
library(tidyverse)
library(bslib)
library(shinycssloaders)

# --- 1. THEME ENGINE ---
my_theme <- bs_theme(
  bootswatch = "cyborg",
  base_font = font_google("Lato"),
  heading_font = font_google("Oswald"),
  primary = "#2ecc71",
  secondary = "#95a5a6"
)

# --- UI: The Frontend ---
ui <- fluidPage(
  theme = my_theme,
  
  tags$head(
    tags$style(HTML("
      .well { background: linear-gradient(145deg, #1a1a1a, #2c2c2c); border: 1px solid #444; box-shadow: 5px 5px 15px rgba(0,0,0,0.5); border-radius: 12px; }
      .card-box { background-color: #1e1e1e; border: 1px solid #333; padding: 20px; border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.3); margin-bottom: 20px; }
      .value-box { text-align: center; padding: 15px; background: #252525; border-radius: 8px; border-left: 4px solid #2ecc71; }
      .value-title { font-size: 0.8em; text-transform: uppercase; color: #aaa; letter-spacing: 1px; }
      .value-number { font-size: 1.8em; font-weight: bold; color: #fff; }
      .btn-success { background: linear-gradient(90deg, #2ecc71, #27ae60); border: none; font-weight: bold; letter-spacing: 1px; box-shadow: 0 0 10px rgba(46, 204, 113, 0.4); }
      .live-dot { height: 10px; width: 10px; background-color: #2ecc71; border-radius: 50%; display: inline-block; box-shadow: 0 0 8px #2ecc71; margin-right: 5px; }
      .footer { text-align: center; color: #555; padding: 40px; font-size: 0.8em; }
      /* Custom Slider Styling */
      .irs-bar { border-top: 1px solid #2ecc71; border-bottom: 1px solid #2ecc71; background: #2ecc71; }
      .irs-bar-edge { border: 1px solid #2ecc71; background: #2ecc71; }
    "))
  ),
  
  # --- APP LAYOUT ---
  div(style = "padding: 20px;",
      
      # Header
      fluidRow(
        column(12,
               h2(icon("chart-area"), " QUANT PORTFOLIO ANALYTICS"),
               p(class = "text-muted", "Institutional-grade performance analysis & optimization.")
        )
      ),
      
      hr(style = "border-color: #333;"),
      
      sidebarLayout(
        
        # LEFT: Control Panel
        sidebarPanel(
          h4("⚡ Control Center"),
          div(style="margin-bottom: 15px; font-size: 0.9em; color: #2ecc71;", 
              span(class="live-dot"), "System Online"),
          
          numericInput("investment", "Initial Investment ($)", 
                       value = 10000, min = 100, step = 500),
          
          textInput("tickers", "Assets (Ticker Symbols)", 
                    value = "AAPL, MSFT, NVDA, GLD", 
                    placeholder = "e.g. BTC-USD, TSLA"),
          
          checkboxInput("benchmark", "Compare to S&P 500 (SPY)", value = FALSE),
          
          dateRangeInput("dates", "Analysis Horizon",
                         start = Sys.Date() - 365,
                         end   = Sys.Date()),
          
          br(),
          actionButton("analyze", "RUN ANALYSIS", 
                       class = "btn-success btn-lg btn-block", icon = icon("rocket")),
          
          br(), br(),
          div(class = "alert alert-dismissible alert-dark",
              h6(icon("lightbulb"), " Pro Tip:"),
              p(style="font-size: 0.8em; margin-bottom: 0;", 
                "Check the 'Risk Lab' to ensure your assets aren't all moving together!")
          ),
          width = 3
        ),
        
        # RIGHT: Visualization Panel
        mainPanel(
          
          # A. Key Metrics Row
          uiOutput("value_boxes"),
          
          br(),
          
          # B. Tabbed Results
          tabsetPanel(type = "pills",
                      
                      # TAB 1: Historical Performance
                      tabPanel("📈 Backtest", 
                               br(),
                               div(class = "card-box",
                                   uiOutput("chart_title"), 
                                   withSpinner(plotlyOutput("growth_plot", height = "55vh"), 
                                               type = 5, color = "#2ecc71")
                               ),
                               div(class = "card-box",
                                   h4("Risk/Return Profile"),
                                   tableOutput("risk_table")
                               )
                      ),
                      
                      # TAB 2: AI OPTIMIZER
                      tabPanel("🧠 AI Optimizer",
                               br(),
                               fluidRow(
                                 column(8,
                                        div(class = "card-box",
                                            h4("Efficient Frontier (Monte Carlo)"),
                                            p(class="text-muted", "Simulating 2,000 portfolio combinations to find the sweet spot."),
                                            withSpinner(plotlyOutput("frontier_plot", height = "50vh"), 
                                                        type = 4, color = "#2ecc71")
                                        )
                                 ),
                                 column(4,
                                        div(class = "card-box",
                                            h4("🏆 Optimal Allocation"),
                                            p(class="text-muted", "Recommended weights for Max Sharpe Ratio:"),
                                            tableOutput("optimal_table"),
                                            hr(),
                                            uiOutput("optimal_stats")
                                        )
                                 )
                               )
                      ),
                      
                      # TAB 3: RISK LAB
                      tabPanel("⚠️ Risk Lab",
                               br(),
                               div(class = "card-box",
                                   h4("Correlation Matrix (Diversification Check)"),
                                   p(class="text-muted", "Lighter colors (Yellow/Green) = High Correlation. Darker/Red = Diversification."),
                                   withSpinner(plotlyOutput("heatmap_plot", height = "60vh"), 
                                               type = 5, color = "#e74c3c")
                               )
                      ),
                      
                      # TAB 4: FUTURE FORECAST (UPDATED)
                      tabPanel("🔮 Future Forecast",
                               br(),
                               div(class = "card-box",
                                   # Header Row with Slider
                                   fluidRow(
                                     column(8, 
                                            h4("Monte Carlo Wealth Projection"),
                                            p(class="text-muted", "Simulating 500 possible market scenarios based on your portfolio's risk profile.")
                                     ),
                                     column(4, 
                                            sliderInput("sim_years", "Projection Horizon (Years):", 
                                                        min = 1, max = 10, value = 3, step = 1)
                                     )
                                   ),
                                   hr(style="border-color: #444;"),
                                   withSpinner(plotlyOutput("projection_plot", height = "60vh"), 
                                               type = 5, color = "#9b59b6")
                               ),
                               fluidRow(
                                 column(4, div(class="value-box", div(class="value-title", "Worst Case (5%)"), uiOutput("mc_min"))),
                                 column(4, div(class="value-box", div(class="value-title", "Expected (50%)"), uiOutput("mc_med"))),
                                 column(4, div(class="value-box", div(class="value-title", "Best Case (95%)"), uiOutput("mc_max")))
                               )
                      )
          ),
          
          width = 9
        )
      )
  ),
  
  # Footer
  div(class = "footer",
      p("Developed by Sabbir Saown | Powered by R Shiny & TidyQuant"),
      p("© 2026 Financial Analytics Suite")
  )
)

# --- SERVER: The Backend ---
server <- function(input, output, session) {
  
  # 1. DATA ENGINE
  portfolio_data <- eventReactive(input$analyze, {
    validate(need(input$tickers != "", "Enter tickers to begin."))
    
    user_symbols <- str_split(input$tickers, ",")[[1]] %>% str_trim()
    all_symbols <- if(input$benchmark) c(user_symbols, "SPY") else user_symbols
    
    showNotification("Fetching market data...", type = "message", duration = 1.5)
    
    tryCatch({
      prices <- tq_get(all_symbols, get = "stock.prices", 
                       from = input$dates[1], to = input$dates[2])
      
      if (nrow(prices) == 0) return(NULL)
      
      returns <- prices %>%
        group_by(symbol) %>%
        tq_transmute(select = adjusted, mutate_fun = periodReturn, 
                     period = "daily", type = "log", col_rename = "daily.returns") %>%
        ungroup()
      
      return(returns)
    }, error = function(e) { return(NULL) })
  })
  
  # 2. OPTIMIZATION ENGINE
  optimization_results <- reactive({
    req(portfolio_data())
    
    data_for_opt <- portfolio_data()
    if(input$benchmark) {
      data_for_opt <- data_for_opt %>% filter(symbol != "SPY")
    }
    
    returns_wide <- data_for_opt %>%
      pivot_wider(names_from = symbol, values_from = daily.returns) %>%
      na.omit() %>%
      select(-date)
    
    returns_matrix <- as.matrix(returns_wide)
    mean_returns   <- colMeans(returns_matrix) * 252
    cov_matrix     <- cov(returns_matrix) * 252
    
    num_portfolios <- 2000
    results_matrix <- matrix(nrow = num_portfolios, ncol = 3 + ncol(returns_matrix))
    colnames(results_matrix) <- c("Return", "Risk", "Sharpe", colnames(returns_matrix))
    
    for (i in 1:num_portfolios) {
      w <- runif(ncol(returns_matrix))
      w <- w / sum(w)
      port_return <- sum(w * mean_returns)
      port_risk   <- sqrt(t(w) %*% cov_matrix %*% w)
      sharpe      <- port_return / port_risk
      results_matrix[i, ] <- c(port_return, port_risk, sharpe, w)
    }
    
    df <- as.data.frame(results_matrix)
    max_sharpe <- df[which.max(df$Sharpe), ]
    
    list(all = df, max = max_sharpe, wide_data = returns_wide) 
  })
  
  # 3. FUTURE PROJECTION ENGINE (UPDATED)
  monte_carlo_sim <- reactive({
    req(optimization_results())
    
    # 1. Get stats of the OPTIMAL portfolio
    max_pt <- optimization_results()$max
    mu     <- max_pt$Return / 252 
    sigma  <- max_pt$Risk / sqrt(252) 
    
    # 2. Simulation Settings (UPDATED: Uses Input Slider)
    years  <- input$sim_years
    days   <- 252 * years 
    sims   <- 500     
    init   <- input$investment
    
    # 3. Generate Paths 
    mat <- matrix(nrow = days, ncol = sims)
    
    set.seed(123) 
    for (j in 1:sims) {
      mat[1, j] <- init
      for (i in 2:days) {
        shock <- rnorm(1)
        mat[i, j] <- mat[i-1, j] * exp((mu - 0.5 * sigma^2) + sigma * shock)
      }
    }
    
    df_sim <- as.data.frame(mat)
    df_sim$Day <- 1:days
    
    df_long <- df_sim %>% 
      pivot_longer(cols = -Day, names_to = "Simulation", values_to = "Value")
    
    list(long = df_long, final_values = mat[days, ])
  })
  
  # --- OUTPUTS ---
  
  output$chart_title <- renderUI({
    h4(paste("Portfolio Growth (Initial Investment: $", format(input$investment, big.mark=","), ")", sep=""))
  })
  
  output$value_boxes <- renderUI({
    req(portfolio_data())
    df <- portfolio_data()
    calc_df <- if(input$benchmark) df %>% filter(symbol != "SPY") else df
    
    avg_return <- mean(calc_df$daily.returns, na.rm=T) * 252 * 100
    avg_risk   <- sd(calc_df$daily.returns, na.rm=T) * sqrt(252) * 100
    
    fluidRow(
      column(4, div(class="value-box", div(class="value-title", "Avg Annual Return"), div(class="value-number", style="color: #2ecc71;", paste0(round(avg_return, 1), "%")))),
      column(4, div(class="value-box", div(class="value-title", "Avg Volatility"), div(class="value-number", style="color: #e74c3c;", paste0(round(avg_risk, 1), "%")))),
      column(4, div(class="value-box", div(class="value-title", "Assets Analyzed"), div(class="value-number", style="color: #3498db;", length(unique(calc_df$symbol)))))
    )
  })
  
  output$growth_plot <- renderPlotly({
    req(portfolio_data())
    initial_investment <- input$investment 
    
    all_data <- portfolio_data() %>%
      group_by(symbol) %>%
      mutate(growth = exp(cumsum(daily.returns)) * initial_investment) %>% 
      ungroup()
    
    if(input$benchmark) {
      main_data <- all_data %>% filter(symbol != "SPY")
      spy_data  <- all_data %>% filter(symbol == "SPY")
    } else {
      main_data <- all_data
      spy_data  <- NULL
    }
    
    p <- ggplot(main_data, aes(x = date, y = growth, color = symbol)) +
      geom_line(size = 1) +
      scale_y_continuous(labels = scales::dollar_format()) + 
      labs(title = NULL, y = "Portfolio Value ($)", x = "") +
      theme_dark() + 
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        panel.grid.major = element_line(color = "#333"),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#ddd")
      )
    
    if(input$benchmark && nrow(spy_data) > 0) {
      p <- p + geom_line(data = spy_data, aes(x = date, y = growth), 
                         color = "#95a5a6", linetype = "dashed", size = 1.2, alpha = 0.8) +
        annotate("text", x = max(spy_data$date), y = max(spy_data$growth), 
                 label = "S&P 500", color = "#95a5a6", hjust = 1, vjust = -1)
    }
    
    ggplotly(p) %>% layout(paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)', legend = list(orientation = "h", x = 0, y = 1.05), font = list(color = '#ccc'))
  })
  
  output$frontier_plot <- renderPlotly({
    req(optimization_results())
    data <- optimization_results()$all
    max_pt <- optimization_results()$max
    
    p <- ggplot(data, aes(x = Risk, y = Return, color = Sharpe)) +
      geom_point(alpha = 0.5, size = 1) +
      scale_color_gradient(low = "#e74c3c", high = "#2ecc71") +
      geom_point(data = max_pt, aes(x = Risk, y = Return), color = "white", size = 4, shape = 18) +
      labs(x = "Risk (Volatility)", y = "Annualized Return") +
      theme_dark() + 
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(color = "#333"),
        text = element_text(color = "#ddd")
      )
    
    ggplotly(p) %>% layout(paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)', font = list(color = '#ccc'))
  })
  
  output$heatmap_plot <- renderPlotly({
    req(optimization_results())
    
    wide_data <- optimization_results()$wide_data
    cor_matrix <- cor(wide_data)
    
    plot_ly(
      x = colnames(cor_matrix),
      y = colnames(cor_matrix),
      z = cor_matrix,
      type = "heatmap",
      colors = colorRamp(c("#2ecc71", "#2c3e50", "#e74c3c")),
      zmin = -1, zmax = 1
    ) %>%
      layout(
        title = "",
        paper_bgcolor='rgba(0,0,0,0)', 
        plot_bgcolor='rgba(0,0,0,0)', 
        font = list(color = '#ccc'),
        xaxis = list(side = "bottom"),
        yaxis = list(autorange = "reversed")
      )
  })
  
  output$projection_plot <- renderPlotly({
    req(monte_carlo_sim())
    
    df <- monte_carlo_sim()$long
    
    p <- ggplot(df, aes(x = Day, y = Value, group = Simulation)) +
      geom_line(alpha = 0.05, color = "#9b59b6", size = 0.5) +
      stat_summary(fun = median, geom = "line", aes(group = 1), color = "white", size = 1.5) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(title = NULL, x = "Trading Days into Future", y = "Projected Wealth") +
      theme_dark() + 
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = "#333"),
        text = element_text(color = "#ddd")
      )
    
    ggplotly(p) %>% layout(paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)', font = list(color = '#ccc'))
  })
  
  output$mc_min <- renderUI({
    req(monte_carlo_sim())
    final_vals <- monte_carlo_sim()$final_values
    val <- quantile(final_vals, 0.05)
    div(class="value-number", style="color: #e74c3c;", scales::dollar(val))
  })
  
  output$mc_med <- renderUI({
    req(monte_carlo_sim())
    final_vals <- monte_carlo_sim()$final_values
    val <- median(final_vals)
    div(class="value-number", style="color: #9b59b6;", scales::dollar(val))
  })
  
  output$mc_max <- renderUI({
    req(monte_carlo_sim())
    final_vals <- monte_carlo_sim()$final_values
    val <- quantile(final_vals, 0.95)
    div(class="value-number", style="color: #2ecc71;", scales::dollar(val))
  })
  
  output$optimal_table <- renderTable({
    req(optimization_results())
    max_pt <- optimization_results()$max
    weights <- max_pt[4:ncol(max_pt)] %>% t() %>% as.data.frame()
    colnames(weights) <- "Weight"
    weights$Asset <- rownames(weights)
    weights$Weight <- paste0(round(weights$Weight * 100, 1), "%")
    weights %>% select(Asset, Weight)
  }, striped = TRUE, width = "100%", align = "c")
  
  output$optimal_stats <- renderUI({
    req(optimization_results())
    max_pt <- optimization_results()$max
    tagList(
      h5("Predicted Performance:"),
      p(style="color: #2ecc71; font-weight: bold;", paste("Return:", round(max_pt$Return * 100, 1), "%")),
      p(style="color: #e74c3c; font-weight: bold;", paste("Risk:", round(max_pt$Risk * 100, 1), "%")),
      p(style="color: #3498db; font-weight: bold;", paste("Sharpe Ratio:", round(max_pt$Sharpe, 2)))
    )
  })
  
  output$risk_table <- renderTable({
    req(portfolio_data())
    portfolio_data() %>%
      group_by(symbol) %>%
      tq_performance(Ra = daily.returns, performance_fun = table.AnnualizedReturns)
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")
}

# --- RUN ---
shinyApp(ui = ui, server = server)