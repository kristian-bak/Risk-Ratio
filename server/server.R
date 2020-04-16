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
                               global_line_1 = NA,
                               global_line_2 = NA,
                               global_line_3 = NA,
                               global_name_1 = NA,
                               global_name_2 = NA,
                               global_name_3 = NA,
                               local_line_1 = NA, 
                               local_line_2 = NA, 
                               local_line_3 = NA, 
                               local_name_1 = NA, 
                               local_name_2 = NA, 
                               local_name_3 = NA, 
                               x1 = NA, 
                               x2 = NA, 
                               level_counter = 1, 
                               all_global_lines = NULL,
                               all_local_lines = NULL, 
                               all_global_names = NULL, 
                               all_local_names = NULL)
  
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
    
    x1 <- min(input$date_global_range[1], input$date_local_range[1])
    x2 <- max(input$date_global_range[2], input$date_local_range[2])
    react_var2$x1 <- x1
    react_var2$x2 <- x2
    
    react_var2$price_today <- react_var2$data$Close[nrow(react_var2$data)]
    
    react_var2$candlestick <- plot_ly(react_var2$data, x = ~Date, type = "candlestick",
                                      open = ~Open, close = ~Close,
                                      high = ~High, low = ~Low, name = "candlestick") %>% 
      layout(title = react_var2$stock,
             yaxis = list(title = "Stock price"),
             xaxis = list(rangeslider = list(visible = F))
      )
    
  })
  
  output$plot_candlestick <- renderPlotly({
    
    react_var2$candlestick
  
  })
  
  observeEvent(input$go_levels, {
    
    data <- react_var2$data
    g1 <- input$date_global_range[1]
    g2 <- input$date_global_range[2]
    
    global_upper <- max(data$High[data$Date >= g1 & data$Date <= g2])
    global_lower <- min(data$Low[data$Date >= g1 & data$Date <= g2])
    
    l1 <- input$date_local_range[1]
    l2 <- input$date_local_range[2]
    
    local_upper <- max(data$High[data$Date >= l1 & data$Date <= l2])
    local_lower <- min(data$Low[data$Date >= l1 & data$Date <= l2])
    
    f1 <- 0.618
    f2 <- 0.5
    f3 <- 0.382
    
    global_dist <- global_upper - global_lower
    local_dist <- local_upper - local_lower
    
    react_var2$global_line_1 <- round(global_dist * f1 + global_lower, 2)
    react_var2$global_line_2 <- round(global_dist * f2 + global_lower, 2)
    react_var2$global_line_3 <- round(global_dist * f3 + global_lower, 2)
    
    react_var2$all_global_lines <- c(react_var2$global_line_1, 
                                     react_var2$global_line_2, 
                                     react_var2$global_line_3)

    price_today <- react_var2$price_today
    
    if (price_today >= react_var2$global_line_1) {
      react_var2$global_name_1 <- "Support"
    } else {
      react_var2$global_name_1 <- "Resistance"
    }
    
    if (price_today >= react_var2$global_line_2) {
      react_var2$global_name_2 <- "Support"
    } else {
      react_var2$global_name_2 <- "Resistance"
    }
    
    if (price_today >= react_var2$global_line_3) {
      react_var2$global_name_3 <- "Support"
    } else {
      react_var2$global_name_3 <- "Resistance"
    }
    
    react_var2$all_global_names <- c(react_var2$global_name_1, 
                                     react_var2$global_name_2, 
                                     react_var2$global_name_3)

    react_var2$local_line_1 <- round(local_dist * f1 + local_lower, 2)
    react_var2$local_line_2 <- round(local_dist * f2 + local_lower, 2)
    react_var2$local_line_3 <- round(local_dist * f3 + local_lower, 2)
    
    react_var2$all_local_lines <- c(react_var2$local_line_1, 
                                    react_var2$local_line_2, 
                                    react_var2$local_line_3)

    if (price_today >= react_var2$local_line_1) {
      react_var2$local_name_1 <- "Support"
    } else {
      react_var2$local_name_1 <- "Resistance"
    }
    
    if (price_today >= react_var2$local_line_2) {
      react_var2$local_name_2 <- "Support"
    } else {
      react_var2$local_name_2 <- "Resistance"
    }
    
    if (price_today >= react_var2$local_line_3) {
      react_var2$local_name_3 <- "Support"
    } else {
      react_var2$local_name_3 <- "Resistance"
    }
    
    react_var2$all_local_names <- c(react_var2$local_name_1, 
                                    react_var2$local_name_2, 
                                    react_var2$local_name_3)
    
    react_var2$x1 <- min(input$date_global_range[1], input$date_local_range[1])
    react_var2$x2 <- max(input$date_global_range[2], input$date_local_range[2])
    
    react_var2$global_lines <- react_var2$all_global_lines[1]
    react_var2$global_names <- react_var2$all_global_names[1]

    react_var2$candlestick <- react_var2$candlestick %>%
      add_segments(x = rep(react_var2$x1, length(react_var2$global_lines)), 
                   xend = rep(react_var2$x2, length(react_var2$global_lines)),
                   y = react_var2$global_lines,
                   yend = react_var2$global_lines,
                   name = react_var2$global_names, inherit = FALSE)
    
  })
  
  observeEvent(input$go_add_level, {
    
    react_var2$level_counter <- react_var2$level_counter + 1
    
    react_var2$global_lines <- c(react_var2$global_lines, 
                                 react_var2$all_global_lines[react_var2$level_counter])
    react_var2$global_names <- c(react_var2$global_names, 
                                 react_var2$all_global_names[react_var2$level_counter])
    react_var2$global_names <- unique(react_var2$global_names)
    
    n_lines <- length(react_var2$global_lines)
    
    react_var2$candlestick <- react_var2$candlestick %>%
      add_segments(x = rep(react_var2$x1, n_lines), 
                   xend = rep(react_var2$x2, n_lines),
                   y = react_var2$global_lines,
                   yend = react_var2$global_lines,
                   name = react_var2$global_names, inherit = FALSE)
    
  })

}