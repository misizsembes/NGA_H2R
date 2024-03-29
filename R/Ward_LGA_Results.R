#PARENT FOLDER
parent_folder <- "~/Desktop/Nigeria/Hard_to_reach/New_Organization/MayFactsheet"
#SETTLEMENT PATH
settlement_working_directory <- paste0(parent_folder,"/","Settlements_Merged")  #DO NOT TOUCH
#H2R EXCEL SHEET--NAME OF SETTLEMENT MERGED FILE
h2r_csv <- "H2R_Settlements_Merged_Month_May19"
h2r_cleaned_data <- read_csv(paste0(settlement_working_directory,"/",h2r_csv,".csv"))
   
#WARD & LGA (OUTPUT) FOLDER
ward_working_directory <- paste0(parent_folder,"/","Ward_Results")
lga_working_directory <- paste0(parent_folder,"/","LGA_Results")
global_working_directory <- paste0(parent_folder,"/","Global_Results")
#GIS (INPUT) FOLDER
gis_working_directory <- paste0(parent_folder,"/","GIS_Settlement_list")
gis_sheet_name <- "Feuil1"

threshold <- 0.05  # % OF SETTLEMENTS THRESHOLD

 #"ALL"; otherwise "01" to "12" (characters)
 month_choose <- "05"
 ##############################################################################
 #WARD FILE NAMES
 if(month_choose !="ALL"){
   ###EXTRACT X CHARACTERS FROM THE RIGHT                      
   substrRight <- function(x, n){
     substr(x, nchar(x)-n+1, nchar(x))
   }
   ward_named_dataset <- paste0("H2R_Ward_Results_",substrRight(h2r_csv, 5) )
 } else if (month_choose =="ALL"){
   ward_named_dataset <- gsub("Settlements_Merged", "Ward_Results", h2r_csv)
 } else{ ward_named_dataset <- "NO_NAME"
 }
 
 #LGA FILE NAMES
 if(month_choose !="ALL"){
   lga_named_dataset <- paste0("H2R_LGA_Results_",substrRight(h2r_csv, 5) )
 } else if (month_choose =="ALL"){
   lga_named_dataset <- gsub("Settlements_Merged", "LGA_Results", h2r_csv)
 } else{ lga_named_dataset <- "NO_NAME"
 }
 
 #GLOBAL FILE NAMES
 if(month_choose !="ALL"){
   global_named_dataset <- paste0("H2R_GLOBAL_Results_",substrRight(h2r_csv, 5) )
 } else if (month_choose =="ALL"){
   global_named_dataset <- gsub("Settlements_Merged", "GLOBAL_Results", h2r_csv)
 } else{ global_named_dataset <- "NO_NAME"
 }
#####LOAD PACKAGES#####
if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(randomForest)) install.packages('randomForest')
library(randomForest)

if (!require(weights)) install.packages('weights')
library(weights)

if (!require(agricolae)) install.packages('agricolae')
library(agricolae)

if (!require(pls)) install.packages('pls')
library(pls)

if (!require(gmodels)) install.packages('gmodels')
library(gmodels)

if (!require(splitstackshape)) install.packages('splitstackshape')
library(splitstackshape)

if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

if (!require(reshape2)) install.packages('reshape2')
library(reshape2)

if (!require(data.table)) install.packages('data.table')
library(data.table)

if (!require(magrittr)) install.packages('magrittr')
library(magrittr)

if (!require(scales)) install.packages('scales')
library(scales)

if (!require(tm)) install.packages('tm')
library(tm)

if (!require(SDMTools)) install.packages('SDMTools')
library(SDMTools)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

if (!require(plotly)) install.packages('plotly')
library(plotly)

if (!require(tibble)) install.packages('tibble')
library(tibble)

if (!require(plotly)) install.packages('plotly')
library(plotly)

if (!require(plyr)) install.packages('plyr')
library(plyr)

if (!require(tidyr)) install.packages('tidyr')
library(tidyr)

if (!require(stringr)) install.packages('stringr')
library(stringr)

if (!require(ggrepel)) install.packages('ggrepel')
library(ggrepel)

if (!require(reshape2)) install.packages('reshape2')
library(reshape2)

if (!require(MASS)) install.packages('MASS')
library(MASS)

if (!require(magrittr)) install.packages('magrittr')
library(magrittr)

if (!require(foreign)) install.packages('foreign')
library(foreign)

if (!require(sandwich)) install.packages('sandwich')
library(sandwich)

if (!require(lmtest)) install.packages('lmtest')
library(lmtest)

if (!require(corrplot)) install.packages('corrplot')
library(corrplot)

if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)

if (!require(xtable)) install.packages('xtable')
library(xtable)

if (!require(Hmisc)) install.packages('Hmisc')
library(Hmisc)

if (!require(car)) install.packages('car')
library(car)

if (!require(readr)) install.packages('readr')
library(readr)

######################FUNCTIONS######################

############Calculate proportions with or without "dont know"
#data == dataframe
#agg_var == Name of the geographic aggregation unit(e.g., lga)
#indicator_index == Column INDEX of the first column to be aggregated
#dont_denom = TRUE if "dont know" is included in the calculations/denominator; FALSE if not
make_proportions <- function(data,agg_var,indicator_index, dont_denom){
  var_name <- colnames(data)[indicator_index]
  locationz <- as.vector(unique(data[grep(paste0("^",deparse(substitute(agg_var)),"$"),colnames(data))]))
  #locationz <- sort(locationz, decreasing=TRUE)
  data<- add_column(data, onesz = 1)
  if(dont_denom == FALSE){
    data  <- data %>%
      dplyr ::filter(!is.na(data[grep(paste0("^",var_name,"$"), colnames(data))])) 
    data  <- dplyr:: filter_(data, paste(var_name,"!=", "'dontknow'",sep=" "))
  } else {
    data  <- data %>%
      dplyr ::  filter(!is.na(data[grep(paste0("^",var_name,"$"), colnames(data))]))
  }
  agg_var_indicator <- as.formula(paste0(deparse(substitute(agg_var)),"~",var_name))  
  result <- data %>% dcast(agg_var_indicator, fun.aggregate = sum,value.var="onesz", na.rm=TRUE)
  namez <- paste0(names(result)[2:ncol(result)],"_",var_name)
  names(result)[2:ncol(result)] <- namez
  result<- add_column(result, total_respondents = rowSums(result[2:ncol(result)]))
  denom_column <- ncol(result)
  props <- list()
  for(i in 2:(ncol(result)-1)){
    props[[i]]<-  result[i]/result[denom_column]
  }
  props[sapply(props, is.null)] <- NULL
  props <- as.data.frame(props)
  names(props) <- paste0("pr_",names(props))
  result <- data.frame(result, props )
  result<-merge(locationz, result, by=deparse(substitute(agg_var)) , all.x=TRUE)
  return(result)
}
#aa<- make_proportions(nonnumeric,lga, 3,TRUE)

########ADD COLUMN IF DOES NOT EXIST########
#data == dataframe
#cname <- single column name or vector of colmn names
add_col_nonexist <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  if(length(add)!=0) data[add] <- 0
  data
}
#add_col_nonexist(togo, c("No","No.information","Yes"))


############RANK VALUES##################
#df == Dataframe of columns -- NOTE THAT IT MUST BE THE ID COLUMNS AND THE REST ARE THE COLUMNS TO BE RANKED
#aggunit == IN QUOTATIONS: Aggregation unit
#toprank == Top-n ranking (e.g., 5 produces the top 5)
rank_money <- function(df, aggunit, toprank) {
  callag <- melt(df, id.vars = c(aggunit))
  id_index <- grep(paste("^",aggunit,"$", sep=""),colnames(callag))
  unique_units <- unique(callag[id_index])
  unique_units<-as.data.frame(unique_units)
  snowflakes <- vector("list")
  for (i in 1:nrow(unique_units)){
    snowflakes[[i]] <- subset(callag, get(aggunit) == unique_units[i,])
  }
  snowflakes<-  lapply(snowflakes, function(x) x[!duplicated(x), ])
  sorted_dataframes_list <- lapply(snowflakes, function(df){
    df[order(df$value,decreasing = TRUE),]
  })
  rankked <- lapply(sorted_dataframes_list,head,n=toprank)
  castedd <- lapply(rankked, function(df){
    units_variable <- as.formula(paste0(as.symbol(aggunit),"~", "factor(",as.symbol("variable"),",levels=unique(",as.symbol("variable"),"))","+",as.symbol("value")))
    dcast(df, units_variable) 
  }) 
  trimcast <- lapply(castedd, function(df){
    sub("_[^_]+$", "", names(df[2:(toprank+1)]))
  })
  for (k in 1: nrow(unique_units)){
    for (j in (toprank+2):(toprank+1+toprank)){
      castedd[[k]][j]<-NA
    }
  }
  for (k in 1: nrow(unique_units)){
    for (j in 1: toprank){
      castedd[[k]][j+toprank+1] <- trimcast[[k]][j] 
    }
  }
  named <-c()  
  for (h in 1:toprank){
    named[h] <- paste0("rank",h,sep="")
  }
  ranknamed <-c() 
  for (l in 1:toprank ){
    ranknamed[l] <- paste0("name",l,sep="")
  }
  titles <- c("geounit", named,ranknamed)
  castedd <- lapply(castedd, setNames, titles)
  locations <- df[grep(paste0("^",aggunit,"$"),colnames(df))]
  locations <- unique(locations)
  ordername <- data.frame(matrix(unlist(castedd), nrow=nrow(unique_units), byrow=T),stringsAsFactors=FALSE)
  colnames(ordername) <- titles
  for (j in 1: toprank+1){
    ordername[j]<-round(as.numeric(unlist(ordername[j])),4)
  }
  ordername$geounit<-locations
  ordername[ordername == 0] <- NA
  names(ordername)[1]<-aggunit
  for(i in 2:(1+toprank)){
    ordername[,i+toprank] <- ifelse(is.na(ordername[,i]),NA,ordername[,i+toprank])
  }
  return(ordername)
}
#aaa <- rank_money(push_reasons, "lga_group", 3)



############RANK LOWEST VALUES##################
#df == Dataframe of columns -- NOTE THAT IT MUST BE THE ID COLUMNS AND THE REST ARE THE COLUMNS TO BE RANKED
#aggunit == IN QUOTATIONS: Aggregation unit
#toprank == Top-n ranking (e.g., 5 produces the top 5)
rank_least <- function(df, aggunit, toprank) {
  callag <- melt(df, id.vars = c(aggunit))
  id_index <- grep(paste("^",aggunit,"$", sep=""),colnames(callag))
  unique_units <- unique(callag[id_index])
  unique_units<-as.data.frame(unique_units)
  snowflakes <- vector("list")
  for (i in 1:nrow(unique_units)){
    snowflakes[[i]] <- subset(callag, get(aggunit) == unique_units[i,])
  }
  snowflakes<-  lapply(snowflakes, function(x) x[!duplicated(x), ])
  sorted_dataframes_list <- lapply(snowflakes, function(df){
    df[order(df$value,decreasing =  FALSE),]
  })
  rankked <- lapply(sorted_dataframes_list,head,n=toprank)
  castedd <- lapply(rankked, function(df){
    units_variable <- as.formula(paste0(as.symbol(aggunit),"~", "factor(",as.symbol("variable"),",levels=unique(",as.symbol("variable"),"))","+",as.symbol("value")))
    dcast(df, units_variable) 
  }) 
  trimcast <- lapply(castedd, function(df){
    sub("_[^_]+$", "", names(df[2:(toprank+1)]))
  })
  for (k in 1: nrow(unique_units)){
    for (j in (toprank+2):(toprank+1+toprank)){
      castedd[[k]][j]<-NA
    }
  }
  for (k in 1: nrow(unique_units)){
    for (j in 1: toprank){
      castedd[[k]][j+toprank+1] <- trimcast[[k]][j] 
    }
  }
  named <-c()  
  for (h in 1:toprank){
    named[h] <- paste0("rank",h,sep="")
  }
  ranknamed <-c() 
  for (l in 1:toprank ){
    ranknamed[l] <- paste0("name",l,sep="")
  }
  titles <- c("geounit", named,ranknamed)
  castedd <- lapply(castedd, setNames, titles)
  locations <- df[grep(paste0("^",aggunit,"$"),colnames(df))]
  locations <- unique(locations)
  ordername <- data.frame(matrix(unlist(castedd), nrow=nrow(unique_units), byrow=T),stringsAsFactors=FALSE)
  colnames(ordername) <- titles
  for (j in 1: toprank+1){
    ordername[j]<-round(as.numeric(unlist(ordername[j])),4)
  }
  ordername$geounit<-locations
  return(ordername)
}
#aaa <- rank_money(push_reasons, "lga_group", 3)

 #########PIVOT/AGGREGATE SETTLEMENT-LEVEL DATA (SETTLEMENT PROPORTIONS#########
 #dataset == dataframe with data to be aggregated 
 #first_column_to_agg == IN QUOTATIONS: Name of the first column to be aggregated -- all columns to aggregate must be consecutive
 #last_column_to_agg == IN QUOTATIONS: Name of the last column to be aggregated -- all columns to aggregate must be consecutive
 #geo_agg_level == IN QUOTATIONS: Name of the desired geographic aggregation level
 agg_pivot <- function(dataset,first_column_to_agg,last_column_to_agg,geo_agg_level){
   results <- list()
   dataset<-add_column(dataset, oneszz := 1)
   for(i in grep(paste0("^",first_column_to_agg,"$"),colnames(dataset)):grep(paste0("^",last_column_to_agg,"$"),colnames(dataset))){
     #ONE QUESTION AT A TIME
     units_variable <- as.formula(paste0(geo_agg_level,"~",colnames(dataset[i]))) 
     colnamedd <- colnames(dataset[i])  
     agg_level <- dataset %>% dcast(units_variable, fun.aggregate = sum,value.var="oneszz", na.rm=TRUE)
     agg_level <- add_col_nonexist(agg_level, c("dontknow","SL","NC"))
     agg_level<-as.data.frame(agg_level)
     #REMOVE "dontknow" "SL" "NC" responses from aggregation
     agg_level<-agg_level[ , -which(names(agg_level) %in% c("dontknow","SL","NC"))]
     agg_level<-as.data.frame(agg_level)
     cols_agg_level <- ncol(agg_level) 
     if(ncol(agg_level)>1){
       colnames(agg_level)[2:cols_agg_level] <- paste0(colnamedd,"_",colnames(agg_level)[2:cols_agg_level])
       for(k in 2:cols_agg_level){
         agg_level[k+(cols_agg_level-1)] <- agg_level[k]/rowSums(agg_level[2:cols_agg_level])
         colnames(agg_level)[k+(cols_agg_level-1)] <- paste0("pr_",colnames(agg_level)[k+(cols_agg_level-1)])
         names(agg_level) <- gsub(x = names(agg_level), pattern = "\\.1", replacement = "") 
       }
     }else{ agg_level <- NULL
     }
     results[[i]] <- agg_level
   }
   #REMOVE NULLS
   results[sapply(results, is.null)] <- NULL
   #EXPORT
   agg_pivots <- as.data.frame(results)
   agg_pivots<-agg_pivots[!grepl("lga_location_real.", colnames(agg_pivots))]
   return(agg_pivots)
 }
 
 
####################################BEGIN AGGREGATION###################################
##REMOVE SPECIAL CHARACTERS AND "consent_yes" FROM HEADERS
names(h2r_cleaned_data) <- names(h2r_cleaned_data) %<>%
  gsub("/", "_", .) %>%
  gsub("-", "_", .)%>%
  gsub("/", "_", .) %>%
  gsub("'", "", .) %>%
  gsub("consent_ok_hoh_ok_", "", .) 
h2r_cleaned_data<-as.data.frame(h2r_cleaned_data)
#CREATE COMBINED LGA AND GROUP VARIABLE TO AGGREGATE BY
h2r_cleaned_data$lga_group <- paste0(h2r_cleaned_data$lga,".",h2r_cleaned_data$group)
h2r_cleaned_data$onesz <-1 

#MAKE LOCATION COLUMN
h2r_cleaned_data$ward_location_real <- paste0(h2r_cleaned_data$C_info_state,";",h2r_cleaned_data$C_info_lga,";",h2r_cleaned_data$C_info_ward)
h2r_cleaned_data$lga_location_real <- paste0(h2r_cleaned_data$C_info_state,";",h2r_cleaned_data$C_info_lga)

#GIS SETTLEMENT COUNTS--WARDS
#GIS ROUTE
gis <- read_excel(paste0(gis_working_directory,"/","Wards_X_GRID3_number-settlements.xlsx"), sheet = gis_sheet_name)
gis$onesz <- 1 
gis$ward_location_real <- tolower(paste0(gis$ADM1_EN,";",gis$ADM2_EN,";",gis$ADM3_EN))
gis_ward <- gis %>% dcast(ward_location_real~onesz, fun.aggregate = sum,value.var="Number_of_settlements", na.rm=TRUE)
colnames(gis_ward)[2] <- "gis_settlment_cnt"
gis_ward$ward_location_real %<>%
  gsub("&", "_", .) %>%
  gsub(" ", "_", .) 
gis$lga_location_real <- tolower(paste0(gis$ADM1_EN,";",gis$ADM2_EN))
gis_lga <- gis %>% dcast(lga_location_real~onesz, fun.aggregate = sum,value.var="Number_of_settlements", na.rm=TRUE)
colnames(gis_lga)[2] <- "gis_settlment_cnt"
gis_lga$lga_location_real %<>%
  gsub("&", "_", .) %>%
  gsub(" ", "_", .) 
#SUBSET ONLY settlements with >=20% Ward coverage
#h2r_cleaned_data <- subset(h2r_cleaned_data,ward_prcnt20ov_yn ==1)
#SELECT CERTAIN LGAs
h2r_cleaned_data <- subset(h2r_cleaned_data, C_info_lga=="bama" |C_info_lga==  "dikwa" |C_info_lga== "gwoza" |C_info_lga== "kala_balge" |C_info_lga== "kukawa" |C_info_lga== "ngala")
h2r_cleaned_data <- as.data.frame(h2r_cleaned_data)

#H2R SETTLEMENT COUNTS
h2r_cleaned_data$onesz <-1
ward_settlment_cnt <- h2r_cleaned_data %>% dcast(ward_location_real~onesz, fun.aggregate = sum,value.var="onesz", na.rm=TRUE)
colnames(ward_settlment_cnt)[2] <- "h2r_settlement_cnt"


##########################WARDS PIVOTS##########################
h2r_cleaned_data$onesz <-1
wards_settlment_cnt <- h2r_cleaned_data %>% dcast(ward_location_real~onesz, fun.aggregate = sum,value.var="onesz", na.rm=TRUE)
colnames(wards_settlment_cnt)[2] <- "h2r_settlement_cnt"

#RUN PIVOT
ward_level_pivots <- agg_pivot(h2r_cleaned_data,"D_D1_hc_now","D_D2_idp_source_settlement","ward_location_real")

#CHECK GIS
ward_gis_joined <- merge(ward_level_pivots,gis_ward, by="ward_location_real", all.x=TRUE)
ward_gis_select <- subset(ward_gis_joined,select=c(ward_location_real,gis_settlment_cnt))
ward_gis_select <- merge(ward_gis_select,wards_settlment_cnt, by="ward_location_real",all.x=TRUE)
ward_gis_select$pr_settlments <-  ward_gis_select$h2r_settlement_cnt/ward_gis_select$gis_settlment_cnt
ward_gis_select$pr_settlments <- ceiling(ward_gis_select$pr_settlments/0.005)*0.005
ward_gis_select$over20prcnt_wards <- ifelse(ward_gis_select$pr_settlments>=threshold,1,0)
#write.csv(ward_gis_select, paste0("settlement_props",month_info,".csv"))

#n <- 2
#pat <- paste0('^([^;]+(?:;[^;]+){',n-1,'}).*')
#ward_gis_select$ward_location_real <-sub(pat, '\\1', ward_gis_select$ward_location_real)
#JOIN TO GIS AND SUBSET ONLY WARDS WITH >=20% SETTLMENTS
ward_level_pivots <- merge(ward_level_pivots, ward_gis_select, by="ward_location_real", all.x=TRUE)
ward_level_pivots <- dplyr::filter(ward_level_pivots,over20prcnt_wards==1)

#SEPARATE GEOGRAPHIES & EXPORT
ward_level_pivots<-  ward_level_pivots %>% separate(ward_location_real, c("State", "LGA","Ward"),sep =";")
ward_level_pivots <- dplyr:: select(ward_level_pivots, -contains("ward_location_real"))
setwd(ward_working_directory)
write.csv(ward_level_pivots,paste0(ward_named_dataset,".csv"),na="")

####################################BEGIN LGA AGGREGATION###################################
setwd(settlement_working_directory)
h2r_cleaned_data <- read_csv(paste0(h2r_csv,".csv"))
##REMOVE SPECIAL CHARACTERS AND "consent_yes" FROM HEADERS
names(h2r_cleaned_data) <- names(h2r_cleaned_data) %<>%
  gsub("/", "_", .) %>%
  gsub("-", "_", .)%>%
  gsub("/", "_", .) %>%
  gsub("'", "", .) %>%
  gsub("consent_ok_hoh_ok_", "", .) 
h2r_cleaned_data<-as.data.frame(h2r_cleaned_data)
#CREATE COMBINED LGA AND GROUP VARIABLE TO AGGREGATE BY
h2r_cleaned_data$lga_group <- paste0(h2r_cleaned_data$lga,".",h2r_cleaned_data$group)
h2r_cleaned_data$onesz <-1 

#MAKE LOCATION COLUMN
h2r_cleaned_data$ward_location_real <- paste0(h2r_cleaned_data$C_info_state,";",h2r_cleaned_data$C_info_lga,";",h2r_cleaned_data$C_info_ward)
h2r_cleaned_data$lga_location_real <- paste0(h2r_cleaned_data$C_info_state,";",h2r_cleaned_data$C_info_lga)

#SUBSET ONLY settlements with >=20% LGA coverage
#h2r_cleaned_data <- subset(h2r_cleaned_data,lga_prcnt_ov20_yn ==1)

#SELECT CERTAIN LGAs
#h2r_cleaned_data <- subset(h2r_cleaned_data, C_info_lga=="bama" |C_info_lga==  "dikwa" |C_info_lga== "gwoza" |C_info_lga== "kala_balge" |C_info_lga== "kukawa" |C_info_lga== "ngala")
h2r_cleaned_data <- as.data.frame(h2r_cleaned_data)

#H2R SETTLEMENT COUNTS
h2r_cleaned_data$onesz <-1
lga_settlment_cnt <- h2r_cleaned_data %>% dcast(lga_location_real~onesz, fun.aggregate = sum,value.var="onesz", na.rm=TRUE)
colnames(lga_settlment_cnt)[2] <- "h2r_settlement_cnt"

##########################LGA PIVOTS##########################
#RUN PIVOT
lga_level_pivots <- agg_pivot(h2r_cleaned_data,"D_D1_hc_now","D_D2_idp_source_settlement","lga_location_real")
lga_to_bind <- lga_level_pivots
colnames(lga_to_bind)[1] <- "aggregation_level"
#GIS CHECK
lga_gis_joined <- merge(lga_level_pivots,gis_lga, by="lga_location_real", all.x=TRUE)
lga_look_gis_percent <- subset(lga_gis_joined, select=c(lga_location_real,gis_settlment_cnt))
lga_look_gis_percent <- merge(lga_look_gis_percent,lga_settlment_cnt, by="lga_location_real", all.x=TRUE)
lga_look_gis_percent$pr_settlments <- lga_look_gis_percent$h2r_settlement_cnt/lga_look_gis_percent$gis_settlment_cnt
#write.csv(lga_look_gis_percent, paste0("lga_settlement_props",".csv"))
lga_look_gis_percent$pr_settlments <- ceiling(lga_look_gis_percent$pr_settlments/0.005)*0.005
lga_look_gis_percent$over20pr_settlements <- ifelse(lga_look_gis_percent$pr_settlments>=threshold,1,0)

#JOIN GIS SETTLEMENT COUNTS AND SUBSET ONLY THOSE >=20%
lga_level_pivots <- merge(lga_level_pivots, lga_look_gis_percent, by="lga_location_real", all.x=TRUE)
lga_level_pivots <- dplyr::filter(lga_level_pivots,over20pr_settlements==1)

#SEPARATE GEOGRAPHIES & EXPORT
lga_level_pivots<- lga_level_pivots %>% separate(lga_location_real, c("State", "LGA"),sep =";")
lga_level_pivots <- dplyr:: select(lga_level_pivots, -contains("lga_location_real"))
setwd(lga_working_directory)
write.csv(lga_level_pivots,paste0(lga_named_dataset,".csv"),na="")

####################################BEGIN GLOBAL AGGREGATION###################################
setwd(settlement_working_directory)
h2r_cleaned_data <- read_csv(paste0(h2r_csv,".csv"))
##REMOVE SPECIAL CHARACTERS AND "consent_yes" FROM HEADERS
names(h2r_cleaned_data) <- names(h2r_cleaned_data) %<>%
  gsub("/", "_", .) %>%
  gsub("-", "_", .)%>%
  gsub("/", "_", .) %>%
  gsub("'", "", .) %>%
  gsub("consent_ok_hoh_ok_", "", .) 
h2r_cleaned_data<-as.data.frame(h2r_cleaned_data)
#CREATE COMBINED LGA AND GROUP VARIABLE TO AGGREGATE BY
h2r_cleaned_data$lga_group <- paste0(h2r_cleaned_data$lga,".",h2r_cleaned_data$group)
h2r_cleaned_data$onesz <-1 

#MAKE LOCATION COLUMN
h2r_cleaned_data$ward_location_real <- paste0(h2r_cleaned_data$C_info_state,";",h2r_cleaned_data$C_info_lga,";",h2r_cleaned_data$C_info_ward)
h2r_cleaned_data$lga_location_real <- paste0(h2r_cleaned_data$C_info_state,";",h2r_cleaned_data$C_info_lga)

#SUBSET ONLY settlements with >=20% LGA coverage
#h2r_cleaned_data <- subset(h2r_cleaned_data,lga_prcnt_ov20_yn ==1)

h2r_cleaned_data <- as.data.frame(h2r_cleaned_data)

#H2R SETTLEMENT COUNTS
h2r_cleaned_data$onesz <-1
global_settlment_cnt <- h2r_cleaned_data %>% dcast(onesz~onesz, fun.aggregate = sum,value.var="onesz", na.rm=TRUE)
colnames(global_settlment_cnt)[2] <- "h2r_settlement_cnt"

##########################GLOBAL PIVOTS##########################
#RUN PIVOT
global_level_pivots <- agg_pivot(h2r_cleaned_data,"D_D1_hc_now","D_D2_idp_source_settlement","onesz")

global_level_pivots <- global_level_pivots %>% dplyr:: select(-contains("onesz")) #%>% dplyr:: select(contains("pr_"))

setwd(global_working_directory)
write.csv(global_level_pivots,paste0("WIDE_",global_named_dataset,".csv"),na="")

lga_global_match <- names(lga_to_bind) ==  names(global_level_pivots)

#COMBINE LGA & GLOBAL RESPONSES
global_level_pivots$aggregation_level <- "Global"
if(any(lga_global_match==FALSE)){
  print("LGA & GLOBAL HEADER MISMATCH")
} else{
  print("LGA & GLOBAL HEADERS MATCH")
}
lga_global_stacked <- rbind(lga_to_bind,global_level_pivots)
setwd(lga_working_directory)
write.csv(lga_global_stacked,paste0("LGA_GLOBAL",lga_named_dataset,".csv"),na="")



