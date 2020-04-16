source("functions/functions.R")

df_year <- data.frame(choices = c("1 year", "3 years", "5 years", "All data"), 
                      year = c("y1", "y3", "y5", "All"), 
                      stringsAsFactors = FALSE)

server <- function(input, output, session) {
  
  react_var <- reactiveValues(calculate_done = FALSE, 
                              res = NULL, 
                              out_res = NULL, 
                              df_type = NULL)
  
  observeEvent(input$go_risk, {
    
    if (length(input$pick_investments) == 0) {
      showNotification("Select investment(s)", type = "error", duration = 4)
      return()
    }
    
    df_invest <- readRDS("investments and types.Rda")
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

    react_var$df_type <- df_type[id, ]
    # ETF skal splittes på ETF - stock og ETF bond
    
    #data_list <- f_load(files)
    #saveRDS(object = data_list, file = paste0(getwd(), "/Data/Investing com/all data.Rda"))
    
    data_list <- readRDS(file = "all data.Rda")
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

    res <- f_risk_loop(data_list, stock_names, type = react_var$df_type$Type, updateProgress)
    res <- res[order(res$RR, decreasing = TRUE), ]
    react_var$res <- res
    react_var$out_res <- res
    
    updateCheckboxGroupInput(session, inputId = "check_type", selected = "All")
    updateCheckboxGroupInput(session, inputId = "check_type_2", selected = "")
    
    react_var$calculate_done <- TRUE 
    
  })
  
  observe({
    if (react_var$calculate_done) {
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
    if (react_var$calculate_done) {
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
    df_type <- react_var$df_type
    res <- react_var$res
    tmp_res <- res[res$year %in% df_year$year[df_year$choices %in% input$check_years], ]
    if ("All" %in% type) {
      react_var$out_res <- tmp_res
    } else {
      react_var$out_res <- tmp_res[tmp_res$Name %in% df_type$Name[df_type$Type %in% type], ]
    }
  })
  
  output$table_risk <- DT::renderDataTable({
    DT::datatable(react_var$out_res[, c("Type", "Name", "Years", "SD", "Return", "RR")], rownames = FALSE)
  })

}