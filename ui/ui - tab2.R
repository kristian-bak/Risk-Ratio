
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
                    tipify(actionButton(inputId = "go_load_stock", 
                                 label = "Load data"), title = "This will load data from Yahoo"),
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
             condition = "input.go_load_stock > 0", 
             fluidRow(
               column(3, offset = 3,
                      tipify(dateRangeInput(inputId = "date_fib_range",
                                     label = 'Fibonacci date range:',
                                     start = Sys.Date() - 180, end = Sys.Date()
                      ), title = "Fibonacci levels will be calculated using minimum and maximum stock price within the date range. Levels: 61.8%, 50% and 38.2%. Max. and min. price are the 100% and 0% levels respectively", 
                      placement = "top")
               ), 
               column(1, 
                      tipify(actionButton(inputId = "go_levels", 
                                   label = "Get levels"), title = "Show Fibonacci levels", placement = "top"),
                      style = "margin-top: 24px"
               )
             ),
             fluidRow(
               column(3, offset = 3,
                      tipify(dateRangeInput(inputId = "date_trend_range",
                                            label = 'Trend date range:',
                                            start = Sys.Date() - 180, end = Sys.Date()
                      ), title = "Trend lines will be calculated within the date range", 
                      placement = "top")
               ), 
               column(1,
                      tipify(actionButton(inputId = "go_trend", label = "Add trend"), 
                             title = "Upper and lower trend lines will be added", placement = "top"), 
                      style = "margin-top: 24px"
               ),
               column(1,
                      tipify(actionButton(inputId = "go_clear", label = "Clear"), 
                             title = "Clear everything", placement = "top"), 
                      style = "margin-top: 24px"
                      ),

             ),
             br()
             )
           )
}