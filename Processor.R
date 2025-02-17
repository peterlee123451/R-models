library('quantmod')
library('tidyverse')


###Read input sheets

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  # Running in RStudio: use the folder of the active document
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  # Running outside of RStudio: attempt to derive the script's location from the command line arguments
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  fileArgName <- "--file="
  scriptPath <- dirname(sub(fileArgName, "", cmdArgs[grep(fileArgName, cmdArgs)]))
  
  if (length(scriptPath) > 0 && nzchar(scriptPath)) {
    setwd(scriptPath)
    message("Working directory set to script location: ", scriptPath)
  } else {
    message("Unable to determine script location. Using current working directory: ", getwd())
  }
}


tickers <- read.csv("input.csv")
colnames(tickers)[1] <- "Ticker"
colnames(tickers)[2] <- "Amount"
tickers <- tickers %>%
  dplyr::filter(Amount != 0) #remove errors/ empty

config <- read.csv("input2.csv") #load config

#Download stock data

Today <- ymd(Sys.Date())#parse dates
Start <- ymd(Sys.Date() - config[1,1]*365)
Enter <- ymd("2020-02-19")
Exit <- ymd("2021-01-01")

getSymbols(tickers$Ticker, src = "yahoo", warnings = F, 
           from = Start, to = Today)

columns <- lapply(tickers$Ticker, function(x) get(x)[, 6])
Portfolio <- data.frame(do.call(cbind,columns))


#Remove Covid if chosen.

if (config[1,2]=="Yes"){
  Portfolio <- Portfolio %>%
    filter(row.names(Portfolio) < Enter | row.names(Portfolio) > Exit )
  
}

##Individual variance


Ind_Var <- function(df){
  Portfolio_log_returns <- as.data.frame(apply(Portfolio, 2, 
                                               function(x) diff(log(x)))) 
  rownames(Portfolio_log_returns) <- rownames(Portfolio)[-1]
  Portfolio_log_returns[is.na(Portfolio_log_returns)] <- 0
  Ind_variance_log_returns <- apply(Portfolio_log_returns, 2, var, na.rm = TRUE)
  return(Ind_variance_log_returns) 
}

Ind_SD_Output <- sqrt(Ind_Var(Portfolio))*sqrt(252)



##Portfolio variance
Por_Var <- function(df) {
  weights <- tickers$Amount / sum(tickers$Amount)
  NAV <- as.matrix(df) %*% weights
  NAV_log_returns <- diff(log(NAV))
  NAV_log_returns[is.na(NAV_log_returns)] <- 0
  NAV_var <- var(NAV_log_returns, na.rm = TRUE)
  
  return(NAV_var)
}

Por_SD_Output <- sqrt(Por_Var(Portfolio))*sqrt(252)

#Output

DATA <- data.frame(Por_SD_Output, t(Ind_SD_Output))
write.csv(DATA, file = "Output.csv", row.names = FALSE)



