f_tab3 <- function() {
  tabPanel(title = "Portfolio",
           fluidRow(
             column(12, 
                    "List your stocks in this table:", 
                    style = "margin-top: 12px; margin-bottom: 12px")
                    ),
           fluidRow(
             column(12, 
                    dataTableOutput("table_portfolio")
                    )
             ),
           br(),
           fluidRow(
             column(2, offset = 1,
                    tipify(actionButton(inputId = "go_load_portfolio", label = "Load table"), 
                           title = "Use my data to play around with the app")),
             column(2, actionButton(inputId = "go_add", label = "Add")), 
             column(2, actionButton(inputId = "go_edit", label = "Edit")),
             column(2, actionButton(inputId = "go_delete", label = "Delete")), 
             column(2, 
                    tipify(actionButton(inputId = "go_save_portfolio", label = "Save table"), 
                           title = "Save table (This allows for fast loading of your portfolio next time)"))),
           br(),
           fluidRow(
             column(6, 
                    verbatimTextOutput(outputId = "text_risk"), 
                    style = "margin-top: 24px"),
             column(6, 
                    sliderInput(inputId = "slide_risk", 
                                label = "Risk appetite", min = 0, max = 8, value = 4)
             )
           ),
           fluidRow(
             column(6, 
                    plotlyOutput("plot_portfolio")
                    ), 
             column(6, 
                    plotlyOutput("plot_golden_pie")
                    )
             ), 
           fluidRow(
             column(2, offset = 4,
                    tipify(actionButton(inputId = "go_zoom", label = "Zoom"), 
                           title = "Click on the subset and press zoom in order to zoom. This works on both your portfolio and the golden pie.", 
                           placement = "top") 
                           ),
             column(2,
                    actionButton(inputId = "go_back", label = "Go back")
                    )
             ), 
           br()
           )
}