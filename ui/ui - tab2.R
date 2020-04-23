
symbols <- read.csv("./data/symbols.csv", 
                    header = TRUE, 
                    sep = "|", 
                    stringsAsFactors = FALSE)

f_tab2 <- function() {
  tabPanel(title = "Technical analysis", 
           h3("Support and resistance levels based on the Fibonacci sequence"), 
           fluidRow(
             column(3, 
                    selectInput(inputId = "select_stock",
                                label = "Select company", 
                                choices = symbols[["combi"]],
                                selected = symbols[["combi"]][2],
                                multiple = FALSE, width = "300px"),
                    style = "margin-top: 12px"
                    ),
             column(2,
                    dateInput(inputId = "date_stock", 
                              label = "Select start date", 
                              value = "2019-08-01"), 
                    style = "margin-top: 12px"
                    ), 
             column(1, offset = 2,  
                    actionButton(inputId = "go_load", 
                                 label = "Load data"),
                    style = "margin-top: 38px"
                    )
             ),
           fluidRow(
             column(4, 
                    textOutput("text_n_stocks"), 
                    style = "margin-top: -12px;
                    margin-bottom: 12px;
                    margin-left: 2px;
                    font-size: 13px"
                    )
             ), 
           fluidRow(
             column(8, 
                    plotlyOutput("plot_candlestick")
                    )
           ), 
           conditionalPanel(
             condition = "input.go_load > 0", 
             fluidRow(
               column(3, offset = 3,
                      dateRangeInput(inputId = "date_range",
                                     label = 'date range input:',
                                     start = Sys.Date() - 180, end = Sys.Date()
                      )
               ), 
               column(1, offset = 1,  
                      actionButton(inputId = "go_levels", 
                                   label = "Get levels"),
                      style = "margin-top: 24px"
               )
             ),
             fluidRow(
               column(1, offset = 7, 
                      actionButton(inputId = "go_add_level", 
                                   label = "Add level"),
                      style = "margin-top: 24px")
             ),
             br()
             )
           )
}