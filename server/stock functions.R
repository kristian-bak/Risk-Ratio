f_TryCatch <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value=value, warning=warn, error=err)
}

f_stocks <- function() {
  data_file <- "data/allexchanges2020.csv"
  data <- read.csv(data_file, stringsAsFactors = FALSE)
  return(data)
}

all_stocks <- f_stocks()

f_risk <- function(type) {
  ## Tag al downloadede data fra investing.com og lav en cluster analyse, der viser 
  ## sammenhængen mellem investeringstype og risikoen(= standardafvigelsen)
  ## Resultatet skal være en tabel med risiko = id (1,2,..,7), investeringstypen og 
  ## interval med standardafvigelsen: [0, 0.1), [0.1, 0.5), [0.5, 1) osv. 
  
  ## Lav en funktion, der tager standardafvigelsen og returnerer en risiko. 
  ## Denne funktion skal være baseret på cluster analysen. 
  
  ifelse(type == "Cash", 1, 
         ifelse(type == "Stock", 7, 
                ifelse(type %in% c("Index", "ETF"), 5, 
                       ifelse(type == "Bond", 2, NA)
                )
         )
  )
}