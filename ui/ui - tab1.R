df_invest <- readRDS("data/investments and types.Rda")

f_tab1 <- function() {
  tabPanel(title = "Risk Ratio", 
           fluidRow(
             column(6, 
                    shinyWidgets::pickerInput(inputId = "pick_investments", 
                                              label = "Select investments", 
                                              choices = df_invest$Name, 
                                              multiple = TRUE, 
                                              options = list(
                                                `actions-box` = TRUE,
                                                `live-search` = TRUE
                                              ))
             ), 
             column(2,
                    actionButton(inputId = "go_risk", label = "Calculate risk ratio"), 
                    style = "margin-top: 30px"
             )
           ),
           br(),
           
           fluidRow(
             column(2,
                    uiOutput("check_years")
             ), 
             column(2, 
                    uiOutput("check_type")
             ), 
             column(1, 
                    uiOutput("check_type_2")
             )
           ),
           
           fluidRow(
             column(8, 
                    DT::dataTableOutput("table_risk")
                    )
             )
           )
}