# Global variables
parent_folder <- "~/Desktop/Nigeria/Hard_to_reach/New_Organization/MayFactsheet"
setwd(parent_folder)  
#INPUT FOLDER--IMPORTING SETTLEMENT DATA FROM HERE
FOLDER_AGGREGATED <- paste0("Settlements_Merged","/") 
#OUTPUT FOLDER--EXPORTING AGGREGATED GLOBAL DATA HERE
FOLDER_DATA_OUTPUTS <- paste0("Global_Results","/")   

#NAME OF CSV SETTLMENT MERGE
settment_merge <- "H2R_Settlements_Merged_Month_May19"

# files we read PASTE IN NAME
AGGREGATED_SETTLEMENT_DATASET <- paste0(FOLDER_AGGREGATED,settment_merge , ".csv")

#TIMEFRAME: "month" OR "window"
timeframe <- "month"
#########################################################################
#files we write
if(timeframe=="window"){
  adjusted_many_months_name <- gsub("Settlements_Merged", "Global_Results", settment_merge)
  DATA_OUTPUT <- paste0(FOLDER_DATA_OUTPUTS, adjusted_many_months_name, ".csv")
  } else if(timeframe == "month"){
    ###EXTRACT X CHARACTERS FROM THE RIGHT                      
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }
     month_year <- substrRight(settment_merge,5)
    DATA_OUTPUT <- paste0(FOLDER_DATA_OUTPUTS, "H2R_Global_Results_Month_", month_year, ".csv")
} else( print("TIMEFRAME ERROR"))



# library imports and working directory
library(tidyverse)
# Reading our data
d.f <- read.csv(AGGREGATED_SETTLEMENT_DATASET, stringsAsFactors = FALSE)

hasdata <- function(x) {
  x[which(!is.null(x) & !is.na(x) & x !="")]  
}

cbindPad <- function(...) {
  args <- list(...) ##I need to define again the list as a list because do.whatever functions turn list into anything, therefore I need to redefine.
  n <- sapply(args, nrow)
  mx <- max(n) ##getting the maximun number of rows of the longest table
  pad <- function(x) {
    if (nrow(x) < mx) {
      padTemp <- matrix(NA, mx - nrow(x), ncol(x)) ##creating a matrix of NA
      colnames(padTemp) <- colnames(x)
      if (ncol(x) == 0) {
        return(padTemp)
      }
      else {
        return(rbind(x, padTemp))
      }
    }
    else {
      return(x)
    }
  }
  rs <- lapply(args, pad)
  return(do.call(cbind, rs))
}
sumSort <- function(x) {
  d.f2 <- as.data.frame(prop.table(table(hasdata(x))))
  return(d.f2[order(d.f2[,2], decreasing = TRUE),])
}

## Analysis output

d.f %>%
  dplyr :: select( -ki_coverage) %>%
  lapply(sumSort) %>%
  do.call(what = cbindPad) %>%
  write.csv(file = DATA_OUTPUT, na = "")

