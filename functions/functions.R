f_data <- function(file) {
  data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  data$Change <- as.numeric(gsub("%|,", "", data$Change..))
  data$Price <- as.numeric(gsub(",", "", data$Price))
  data$Date <- anydate(data$i..Date)
  data$Year <- as.numeric(substring(data$Date, 1, 4))
  data$Month <- substring(data$Date, 6, 7)
  data <- data[, c("Date", "Year", "Month", "Open", "Low", "Price", "High", "Change")]
  data <- data.table(data)
  return(data)
  
}

f_load <- function(files) {
  lapply(files, f_data)
}

f_risk_ratio <- function(data, rf = 0, digits = 5) {
  s <- round(sd(data[["Change"]]), digits)
  end <- data[["Price"]][1]
  start <- data[["Price"]][nrow(data)]
  r <- round((end - start) / start, digits)
  RR <- round((r - rf) / s, digits)
  df <- data.frame("SD" = s, "Return" = r, "RR" = RR)
  return(df)
}

f_risk_year <- function(data, stock_name, type) {
  res <- list()
  end <- max(data$Year) - 1
  start3 <- end - 2
  start5 <- end - 4
  start_all <- min(data$Year)
  data1y <- data[data$Year == end, ]
  data3y <- data[data$Year >= start3 & data$Year <= end, ]
  data5y <- data[data$Year >= start5 & data$Year <= end, ]
  res$y1 <- f_risk_ratio(data1y)
  res$y3 <- f_risk_ratio(data3y)
  res$y5 <- f_risk_ratio(data5y)
  res$All <- f_risk_ratio(data)
  res <- do.call("rbind", res)
  res$year <- row.names(res)
  res$Years <- res$year
  res$Years[res$Years == "y1"] <- end
  res$Years[res$Years == "y3"] <- paste0(start3, " - ", end)
  res$Years[res$Years == "y5"] <- paste0(start5, " - ", end)
  res$Years[res$Years == "All"] <- paste0("Since ", start_all)
  row.names(res) <- c()
  res$Name <- stock_name
  res$Type <- type
  res <- res[, c("Type", "Name", "year", "Years", "SD", "Return", "RR")]
  return(res)
}

f_risk_loop <- function(data_list, stock_names, type, updateProgress = NULL) {
  res <- list()
  n <- length(data_list)
  for (i in 1:n) {
    res[[i]] <- f_risk_year(data = data_list[[i]], stock_name = stock_names[i], type = type[i])
    #cat("\r", i, "of", n) 
    #flush.console()
    
    if (is.function(updateProgress)) {
      text <- paste(i, "of", n, sep = " ")
      updateProgress(detail = text)
    }
    
  }
  
  res <- do.call("rbind", res)
  
  return(res)
}

f_subsetList <- function(myList, elementNames) {
  lapply(elementNames, FUN=function(x) myList[[x]])
}

f_seqDate <- function(from, by = 1, length.out = 10, excludeDays = c("Saturday", "Sunday")) {
  dates <- seq.Date(from = from, by = by, length.out = length.out)
  dates[!weekdays(dates) %in% excludeDays]
}

f_add_dates <- function(data, from, by, length.out) {
  var <- names(data)
  dates <- f_seqDate(from, by, length.out)
  n <- length(dates)
  df_new <- data.frame(matrix(NA, nrow = n, ncol = length(var)))
  names(df_new) <- var
  df_new$Date <- dates
  data <- rbind(data, df_new)
  return(data)
}

f_format <- function(data, type) {
  charCols <- names(type)[type]
  numCols <- names(type)[!type]
  data <- data[, (charCols) := lapply(.SD, as.character), .SDcols = charCols]
  data <- data[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
  return(data)
}

