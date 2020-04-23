source("functions/functions.R")
source("server/stock functions.R")

df_year <- data.frame(choices = c("1 year", "3 years", "5 years", "All data"), 
                      year = c("y1", "y3", "y5", "All"), 
                      stringsAsFactors = FALSE)

server <- function(input, output, session) {
  
  ## Tab 1:
  
  react_var1 <- reactiveValues(calculate_done = FALSE, 
                              res = NULL, 
                              out_res = NULL, 
                              df_type = NULL)
  
  observeEvent(input$go_risk, {
    
    if (length(input$pick_investments) == 0) {
      showNotification("Select investment(s)", type = "error", duration = 4)
      return()
    }
    
    id <- which(df_invest$Name %in% input$pick_investments)
    df_sub <- df_invest[id, ]
    #files <- df_sub$File
    stock_names <- df_sub$Name

    #df_type <- data.frame("Name" = stock_names, 
    #                      "Type" = c("Crypto", "Crypto", "Commodities", "Stock", "Index", 
    #                                 "Bond", "Index", "Crypto", "Currency", "Currency", 
    #                                 "Index", "Index", "ETF", "ETF", "ETF", "ETF", "ETF", "ETF", "ETF", 
    #                                 "Crypto", "Index", "Index", "Index", "ETF", "ETF", "ETF", "Crypto", 
    #                                 "Bond", "Bond", "Bond", "Bond", "Bond", "Bond", 
    #                                 "Currency", "Currency", "Commodities", "Commodities"), 
    #                      stringsAsFactors = FALSE)
    #saveRDS(object = df_type, file = paste0(getwd(), "/Data/investments and types.Rda"))
    df_type <- df_invest

    react_var1$df_type <- df_type[id, ]
    # ETF skal splittes på ETF - stock og ETF bond
    
    #data_list <- f_load(files)
    #saveRDS(object = data_list, file = paste0(getwd(), "/Data/Investing com/all data.Rda"))
    
    data_list <- readRDS(file = "./data/all data.Rda")
    data_list <- f_subsetList(myList = data_list, elementNames = id)
    
    n <- length(stock_names)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Calculating risk ratio", value = 0)

    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (1 / n)
      }
      progress$set(value = value, detail = detail)
    }

    res <- f_risk_loop(data_list, stock_names, type = react_var1$df_type$Type, updateProgress)
    res <- res[order(res$RR, decreasing = TRUE), ]
    react_var1$res <- res
    react_var1$out_res <- res
    
    updateCheckboxGroupInput(session, inputId = "check_type", selected = "All")
    updateCheckboxGroupInput(session, inputId = "check_type_2", selected = "")
    
    react_var1$calculate_done <- TRUE 
    
  })
  
  observe({
    if (react_var1$calculate_done) {
      output$check_years <- renderUI({
        checkboxGroupInput(inputId = "check_years", 
                           label = "Select time period(s)", 
                           choices = c("1 year", "3 years", "5 years", "All data"), 
                           selected = "3 years"
        )
      })
    }
  })
  
  observe({
    if (react_var1$calculate_done) {
      output$check_type <- renderUI({
        checkboxGroupInput(inputId = "check_type", 
                           label = "Select investment type(s)", 
                           choices = c("All", "Bond", "Commodities", "Crypto"), 
                           selected = "All"
        )
      })
      
      output$check_type_2 <- renderUI({
        checkboxGroupInput(inputId = "check_type_2", 
                           label = "", 
                           choices = c("Currency","ETF", "Index", "Stock"), 
                           selected = ""
        )
      })
    }
  })
  
  observeEvent(c(input$check_years,
                 input$check_type, 
                 input$check_type_2), {
    type <- c(input$check_type, input$check_type_2)
    df_type <- react_var1$df_type
    res <- react_var1$res
    tmp_res <- res[res$year %in% df_year$year[df_year$choices %in% input$check_years], ]
    if ("All" %in% type) {
      react_var1$out_res <- tmp_res
    } else {
      react_var1$out_res <- tmp_res[tmp_res$Name %in% df_type$Name[df_type$Type %in% type], ]
    }
  })
  
  output$table_risk <- DT::renderDataTable({
    DT::datatable(react_var1$out_res[, c("Type", "Name", "Years", "SD", "Return", "RR")], rownames = FALSE)
  })
  
  ## Tab 2:
  
  react_var2 <- reactiveValues(data = NULL, 
                               Company = NULL,                               
                               date = 0, 
                               stock = 0, 
                               counter = 1,
                               day1 = NULL,
                               day2 = NULL,
                               start_day = NULL,
                               end_day = NULL,
                               lines = NULL,
                               line_names = NULL, 
                               start_candlestick = NULL, 
                               candlestick = NULL)
  
  ticker <- reactive({
    
    df <- symbols
    
    ticker <- df$Symbol[df$combi == input$select_stock]
    react_var2$Company <- df$Company[df$Symbol == ticker]
    
    return(ticker)
    
  })
  
  data <- reactive({
    
    if (is.null(input$select_stock)) {
      return()
    }
    
    progress <- Progress$new(session, min = 1, max = 5)
    on.exit(progress$close())
    
    progress$set(message = 'Loading data...')
    
    ticker <- ticker()

    data <- f_TryCatch(quantmod::getSymbols(ticker, 
                                  auto.assign = FALSE, 
                                  from = input$date_stock, 
                                  warnings = FALSE,
                                  src = 'yahoo'))
    if (is.null(data$value)) {
      showNotification("Downloading failed. Try another stock", 
                       type = "error", duration = 4)
      return()
    }
    
    data <- data.frame(data$value)
    data$Date <- as.Date(rownames(data))
    names(data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Date")
    data <- data[, c("Date", "Open", "High", "Low", "Close", "Adjusted", "Volume")]
    
    return(data)
    
  })
  
  output$text_n_stocks <- renderText({
    
    paste0("Select among ", nrow(symbols), " stocks...")
    
  })
  
  observeEvent(input$go_load, {
    
    if (input$select_stock == "") {
      showNotification("Select company first", type = "error", duration = 4)
      return()
    }
    
    if (input$date_stock == react_var2$date & input$select_stock == react_var2$stock) {
      showNotification("Change the company or date", type = "error", duration = 4)
      return()
    }
    
    react_var2$data <- data()
    react_var2$date <- input$date_stock
    react_var2$stock <- input$select_stock
    
    react_var2$price_today <- react_var2$data$Close[nrow(react_var2$data)]
  
    react_var2$start_candlestick <- plot_ly(react_var2$data, x = ~Date, type = "candlestick",
                                      open = ~Open, close = ~Close,
                                      high = ~High, low = ~Low, name = "candlestick") %>% 
      layout(title = react_var2$stock,
             yaxis = list(title = "Stock price"),
             xaxis = list(rangeslider = list(visible = F)), 
             legend = list(orientation = 'h', x = 0.2, y = 1))
    
    react_var2$candlestick <- react_var2$start_candlestick
    
  })
  
  output$plot_candlestick <- renderPlotly({
    
    react_var2$candlestick
  
  })
  
  observeEvent(input$go_levels, {
    
    react_var2$counter <- 1
    
    data <- react_var2$data
    
    day1 <- input$date_range[1]
    day2 <- input$date_range[2]
    
    upper <- max(data$High[data$Date >= day1 & data$Date <= day2])
    lower <- min(data$Low[data$Date >= day1 & data$Date <= day2])
    
    f <- c(0.618, 0.5, 0.382)
    
    dist <- upper - lower
    
    digits <- 2
    lines <- round(dist * f + lower, digits)
    react_var2$lines <- lines
    
    price_today <- react_var2$price_today

    react_var2$line_names <- ifelse(lines >= price_today, "Resistance", "Support")
    
    react_var2$day1 <- day1
    react_var2$day2 <- day2
    
    react_var2$start_date <- min(data$Date)
    react_var2$end_date <- max(data$Date)

    react_var2$candlestick <- react_var2$start_candlestick %>%
      add_segments(x = react_var2$start_date, 
                   xend = react_var2$end_date,
                   y = react_var2$lines[1],
                   yend = react_var2$lines[1],
                   name = react_var2$line_names[1], 
                   line = list(color = 'black'),
                   inherit = FALSE)
    
  })
  
  observeEvent(input$go_add_level, {
    
    react_var2$counter <- react_var2$counter + 1
    
    if (react_var2$counter >= 4) {
      showNotification("All Fibonacci levels added", duration = 4, type = "error")
      return()
    }
    
    new_line <- react_var2$lines[react_var2$counter]
    new_name <- react_var2$line_names[react_var2$counter]
    
    dates1 <- rep(react_var2$start_date, react_var2$counter)
    dates2 <- rep(react_var2$end_date, react_var2$counter)
    
    react_var2$candlestick <- react_var2$candlestick %>%
      add_segments(x = dates1,
                   xend = dates2,
                   y = new_line,
                   yend = new_line,
                   name = new_name, 
                   line = list(color = 'black'),
                   inherit = FALSE)
    
  })

}