
#files <- list.files(paste0(getwd(), "/Data/Investing com"), full.names = TRUE)
#all_investments <- basename(files)
#all_investments <- gsub(" Historical Data| Historical Data.csv| - Investing.com.csv", "", all_investments)
#
#df_invest <- data.frame("File" = files, "Investment" = all_investments, stringsAsFactors = FALSE)

source("ui/ui - tab1.R")
source("ui/ui - tab2.R")
source("ui/ui - tab3.R")

ui <- fluidPage(
  
  titlePanel(title = "Risk Ratio"), 
  
  tabsetPanel(
    f_tab1(), 
    f_tab2(),
    f_tab3()
  ) 
  
)
