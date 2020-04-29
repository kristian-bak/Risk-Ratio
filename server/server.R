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
                               day1 = NULL,
                               day2 = NULL,
                               start_day = NULL,
                               end_day = NULL,
                               lines = NULL,
                               line_names = NULL, 
                               start_candlestick = NULL, 
                               candlestick = NULL, 
                               fib = FALSE, 
                               trend = FALSE)
  
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
  
  observeEvent(input$go_load_stock, {
    
    if (input$select_stock == "") {
      showNotification("Select company first", type = "error", duration = 4)
      return()
    }
    
    if (input$date_stock == react_var2$date & input$select_stock == react_var2$stock) {
      showNotification("Change the company or date", type = "error", duration = 4)
      return()
    }
    
    data <- data()
    
    n_data <- nrow(data)
    ## added 10 days to make trend canals look prettier
    data <- f_add_dates(data, from = max(data$Date) + 1, by = 1, length.out = 10)
    
    react_var2$price_today <- data$Close[n_data]
    react_var2$data <- data
    react_var2$date <- input$date_stock
    react_var2$stock <- input$select_stock
    
  
    react_var2$start_candlestick <- plot_ly(react_var2$data, x = ~Date, type = "candlestick",
                                      open = ~Open, close = ~Close,
                                      high = ~High, low = ~Low, name = "candlestick") %>% 
      layout(title = react_var2$stock,
             yaxis = list(title = "Stock price"),
             xaxis = list(rangeslider = list(visible = F)), 
             legend = list(orientation = 'h', x = 0.2, y = 1))
    
    react_var2$candlestick <- react_var2$start_candlestick
    
    react_var2$fib <- FALSE 
    react_var2$trend <- FALSE
    
  })
  
  output$plot_candlestick <- renderPlotly({
    
    react_var2$candlestick
  
  })
  
  observeEvent(input$go_levels, {
    
    data <- react_var2$data
    
    day1 <- input$date_fib_range[1]
    day2 <- input$date_fib_range[2]
    
    upper <- max(data$High[data$Date >= day1 & data$Date <= day2 & !is.na(data$High)])
    lower <- min(data$Low[data$Date >= day1 & data$Date <= day2 & !is.na(data$Low)])
    
    f <- c(0.618, 0.5, 0.382)
    
    dist <- upper - lower
    
    digits <- 2
    lines <- round(dist * f + lower, digits)
    react_var2$lines <- lines
    
    price_today <- react_var2$price_today

    react_var2$line_names <- ifelse(lines >= price_today, "Resistance", "Support")
    
    react_var2$day1 <- day1
    react_var2$day2 <- day2
    
    react_var2$start_date <- rep(min(data$Date), 3)
    react_var2$end_date <- rep(max(data$Date), 3)
    
    react_var2$fib <- TRUE
    
    i <- 1:2
    j <- 3
    
    react_var2$candlestick <- react_var2$candlestick %>%
      add_segments(x = react_var2$start_date[i], 
                   xend = react_var2$end_date[i],
                   y = react_var2$lines[i],
                   yend = react_var2$lines[i],
                   name = react_var2$line_names[i], 
                   line = list(color = 'black'),
                   inherit = FALSE) %>%
      add_segments(x = react_var2$start_date[j], 
                   xend = react_var2$end_date[j],
                   y = react_var2$lines[j],
                   yend = react_var2$lines[j],
                   name = react_var2$line_names[j], 
                   line = list(color = 'black'),
                   inherit = FALSE)
    
    if (react_var2$trend) {
      react_var2$candlestick <- react_var2$candlestick %>%
        add_lines(x = ~Date, y = ~p_lower, name = "lower", inherit = FALSE) %>%
        add_lines(x = ~Date, y = ~p_upper, name = "upper", inherit = FALSE)
    }

  })
  
  observeEvent(input$go_trend, {
    
    data <- react_var2$data
    start <- input$date_trend_range[1]
    end <- input$date_trend_range[2]
    subdata <- data[data$Date >= start & data$Date <= end, ]
    pred_data <- data[data$Date >= start, ]
    m_lower <- lm(Low ~ Date, data = subdata)
    m_upper <- lm(High ~ Date, data = subdata)
    
    n_na <- nrow(data) - nrow(pred_data)
    
    pred_lower <- round(predict(m_lower, newdata = pred_data), 2)
    pred_upper <- round(predict(m_upper, newdata = pred_data), 2)

    react_var2$data$p_lower <- c(rep(NA, n_na), pred_lower)
    react_var2$data$p_upper <- c(rep(NA, n_na), pred_upper)
    
    react_var2$trend <- TRUE
    
    react_var2$candlestick <- plot_ly(react_var2$data, x = ~Date, type = "candlestick",
                                      open = ~Open, close = ~Close,
                                      high = ~High, low = ~Low, name = "candlestick") %>% 
      add_lines(x = ~Date, y = ~p_lower, name = "lower", inherit = FALSE) %>%
      add_lines(x = ~Date, y = ~p_upper, name = "upper", inherit = FALSE) %>%
      layout(title = react_var2$stock,
             yaxis = list(title = "Stock price"),
             xaxis = list(rangeslider = list(visible = F)), 
             legend = list(orientation = 'h', x = 0.2, y = 1))

    if (react_var2$fib) {
      i <- 1:2
      j <- 3
      react_var2$candlestick <- react_var2$candlestick %>%
        add_segments(x = react_var2$start_date[i], 
                     xend = react_var2$end_date[i],
                     y = react_var2$lines[i],
                     yend = react_var2$lines[i],
                     name = react_var2$line_names[i], 
                     line = list(color = 'black'),
                     inherit = FALSE) %>%
        add_segments(x = react_var2$start_date[j], 
                     xend = react_var2$end_date[j],
                     y = react_var2$lines[j],
                     yend = react_var2$lines[j],
                     name = react_var2$line_names[j], 
                     line = list(color = 'black'),
                     inherit = FALSE) 
      
    }
  
  })
  
  observeEvent(input$go_clear, {
    
    react_var2$candlestick <- react_var2$start_candlestick
    
    react_var2$fib <- react_var2$trend <- FALSE
    
  })
  
  ## Tab 3
  
  
  
  #react_var3 <- reactiveValues(portfolio = data.table("Type" = character(), 
  #                                                    "Name" = character(), 
  #                                                    "Price" = numeric(), 
  #                                                    "Number of shares" = numeric(), 
  #                                                    "Value" = numeric()), 
  #                             DT_plot = NULL, 
  #                             key = "Type",
  #                             str_title = "Portfolio distribution"
  #)
  #
  #output$table_portfolio <- DT::renderDataTable({
  #  DT::datatable(react_var3$portfolio, editable = TRUE, 
  #            option = list(dom = 't', columnDefs = list(list(targets = 1:4, class = "dt-right"))))
  #})
  #
  #observeEvent(input$go_load_portfolio, {
  #  result_file <- "results/portfolio.csv"
  #  if (file.exists(result_file)) {
  #    react_var3$portfolio <- read.csv(result_file, 
  #                                     header = TRUE, 
  #                                     stringsAsFactors = FALSE)
  #    react_var3$portfolio <- data.table(react_var3$portfolio)
  #    setnames(x = react_var3$portfolio, old = "Number.of.shares", new = "Number of shares")
  #    react_var3$DT_plot <- react_var3$portfolio
  #  } else {
  #    showNotification("Create your portfolio first", type = "error", duration = 4)
  #  }
  #})
  #
  #observeEvent(input$go_add, {
  #  
  #  showModal(modalDialog(title = "Add a new row",
  #                        selectInput(inputId = "add_type", 
  #                                    label = "Type", 
  #                                    choices = c("Stock", "Cash", "ETF", "Index", "Bond"), 
  #                                    selected = NULL, 
  #                                    multiple = FALSE),
  #                        selectInput(inputId = "add_name", 
  #                                    label = "Stock", 
  #                                    choices = all_stocks$name_tick,
  #                                    selected = NULL,
  #                                    multiple = FALSE),
  #                        numericInput(inputId = "add_price", label = "Purchase price", value = NA),
  #                        numericInput(inputId = "add_shares", label = "Number of shares", value = NA),
  #                        tipify(numericInput(inputId = "add_value", label = "Value", value = NA),
  #                               title = "Only for cash - autofilled for the remaining types"),
  #                        actionButton("go_add_real", "Add item"),
  #                        easyClose = TRUE, footer = NULL))
  #})
  #
  #observeEvent(input$go_add_real, {
  #  new_row <- data.table("Type" = input$add_type, 
  #                        "Name" = input$add_name, 
  #                        "Price" = input$add_price, 
  #                        "Number of shares" = input$add_shares, 
  #                        "Value" = input$add_value)
  #  non_cash_id <- which(new_row$Type != "Cash" & !is.na(new_row$Price) & !is.na(new_row[["Number of shares"]]))
  #  if (length(non_cash_id) > 0) {
  #    new_row$Value[non_cash_id] <- new_row$Price[non_cash_id] * new_row[["Number of shares"]][non_cash_id]
  #  }
  #  
  #  cash_id <- which(new_row$Type == "Cash")
  #  if (length(cash_id) > 0) {
  #    new_row$Name[cash_id] <- NA
  #    new_row$Price[cash_id] <- NA
  #    new_row[["Number of shares"]][cash_id] <- NA
  #  }
  #  
  #  react_var3$portfolio <- rbind(react_var3$portfolio, new_row)
  #  react_var3$DT_plot <- react_var3$portfolio
  #  removeModal()
  #})
  #
  #observeEvent(input$table_portfolio_cell_edit, {
  #  info <- input$table_portfolio_cell_edit
  #  i <- info$row
  #  j <- info$col
  #  v <- info$value
  #  react_var3$portfolio[i, j] <- v
  #  react_var3$DT_plot <- react_var3$portfolio
  #})
  #
  #observeEvent(input$go_delete, {
  #  showModal(
  #    if (length(input$table_portfolio_rows_selected) >= 1) {
  #      modalDialog(
  #        title = "Warning",
  #        paste("Are you sure deleting", length(input$table_portfolio_rows_selected),"rows?"),
  #        footer = tagList(
  #          actionButton("go_delete_real", "Yes"),
  #          modalButton("Cancel")
  #        ), easyClose = TRUE)
  #    } else {
  #      modalDialog(
  #        title = "Warning",
  #        paste("Please select row(s) that you want to delect!"), easyClose = TRUE
  #      )
  #    }
  #  )
  #})
  #
  #### If user say OK, then delete the selected rows
  #observeEvent(input$go_delete_real, {
  #  react_var3$portfolio <- react_var3$portfolio[-input$table_portfolio_rows_selected, ]
  #  removeModal()
  #})
  #
  #### edit button
  #observeEvent(input$go_edit, {
  #  showModal(
  #    if (length(input$table_portfolio_rows_selected) >= 1) {
  #      modalDialog(
  #        fluidPage(
  #          h3(strong("Modification"), align = "center"),
  #          hr(),
  #          DT::dataTableOutput('row_modif'),
  #          actionButton(inputId = "save_changes", label = "Save changes"),
  #          tags$script(HTML("$(document).on('click', '#save_changes', function () {
  #                           var list_value=[]
  #                           for (i = 0; i < $( '.new_input' ).length; i++)
  #                           {
  #                           list_value.push($( '.new_input' )[i].value)
  #                           }
  #                           Shiny.onInputChange('go_edit_real', list_value) });")) ), size = "l" )
  #    } else {
  #      modalDialog(
  #        title = "Warning",
  #        paste("Please select the row that you want to edit!" ),easyClose = TRUE
  #      )
  #    }
  #    
  #  )
  #})
  #
  ##### modify part
  #output$row_modif <- DT::renderDataTable({
  #  selected_row <- input$table_portfolio_rows_selected
  #  old_row <- react_var3$portfolio[selected_row, ]
  #  row_change = list()
  #  for (i in colnames(old_row)) {
  #    if (is.numeric(react_var3$portfolio[[i]])) {
  #      row_change[[i]] <- paste0('<input class="new_input" value= ','"', old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
  #    } else if (lubridate::is.Date(react_var3$portfolio[[i]])) {
  #      row_change[[i]] <- paste0('<input class="new_input" value= ','"', old_row[[i]],'"',' type="date" id=new_  ',i,'  ><br>') 
  #    } else {
  #      row_change[[i]] <- paste0('<input class="new_input" value= ','"', old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
  #    }
  #  }
  #  row_change <- as.data.table(row_change)
  #  setnames(row_change, colnames(old_row))
  #  DT <- row_change
  #  DT 
  #},escape = F, options = list(dom = 't',ordering = F, scrollX = TRUE), selection = "none")
  #
  #
  #### This is to replace the modified row to existing row
  #observeEvent(input$go_edit_real, {
  #  edit_vector <- input$go_edit_real
  #  edit_mat <- matrix(edit_vector, ncol = ncol(react_var3$portfolio), byrow = TRUE)
  #  DT <- data.table(edit_mat)
  #  colnames(DT) <- colnames(react_var3$portfolio)
  #  colTypes <- sapply(react_var3$portfolio, is.character)
  #  DT <- f_format(DT, type = colTypes)
  #  react_var3$portfolio[input$table_portfolio_rows_selected] <- DT
  #  react_var3$portfolio$Value <- react_var3$portfolio$Price * react_var3$portfolio[["Number of shares"]]
  #  
  #}
  #)
  #
  #observeEvent(input$go_save_portfolio, {
  #  result_file <- "results/portfolio.csv"
  #  write.csv(x = react_var3$portfolio, file = result_file, row.names = FALSE)
  #})
  #
  #risk_appetite <- reactive({
  #  DT <- react_var3$portfolio
  #  Value <- DT[, list(Investment = sum(Value)), by = Type]
  #  Value[, risk := f_risk(Type)]
  #  Value[, weight := Investment / sum(Investment)]
  #  round(Value[, sum(weight * risk) / .N], 2)
  #})
  #
  #output$text_risk <- renderPrint({
  #  if (risk_appetite() == "NaN") {
  #    return()
  #  }
  #  paste("Your portfolio has a risk level of", risk_appetite())
  #})
  #
  #output$plot_portfolio <- renderPlotly({
  #  
  #  if (is.null(react_var3$DT_plot)) {
  #    return()
  #  }
  #  
  #  plot_ly(source = "portfolio", data = react_var3$DT_plot, 
  #          labels = ~get(react_var3$key), values = ~Value, type = 'pie', key = ~get(react_var3$key)) %>%
  #    layout(title = react_var3$str_title,
  #           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  #})
  #
  #observeEvent(input$go_zoom, {
  #  
  #  if (is.null(react_var3$DT_plot)) {
  #    return()
  #  }
  #  
  #  click <- event_data("plotly_click", source = "portfolio")
  #  
  #  if (is.null(click)) {
  #    showNotification("Click on subset to zoom on", type = "error", duration = 4)
  #    return()
  #  }
  #  
  #  subset <- unique(click$key[[1]])
  #  react_var3$key <- "Name"
  #  DT <- react_var3$portfolio
  #  react_var3$DT_plot <- DT[Type == subset, ]
  #  react_var3$str_title <- paste0(subset, " distribution")
  #  
  #})
  #
  #observeEvent(input$go_back, {
  #  react_var3$DT_plot <- react_var3$portfolio
  #  react_var3$key <- "Type"
  #  react_var3$str_title <- "Portfolio distribution"
  #})
  #
  #output$plot_golden_pie <- renderPlotly({
  #  df <- data.frame("Name" = c("Stock", "Cash", "ETF"), 
  #                   "Value" = rep(1 / 3, 3))
  #  plot_ly(source = "golden_pie", data = df, 
  #          labels = ~Name, values = ~Value, type = 'pie', key = ~Name) %>%
  #    layout(title = "The golden pie",
  #           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  #})

}