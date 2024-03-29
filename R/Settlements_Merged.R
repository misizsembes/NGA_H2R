# Working directory
parent_folder <- "~/Desktop/Nigeria/Hard_to_reach/New_Organization/MayFactsheet"
setwd(parent_folder)
#"EXCEL" OR "CSV" KOBO OUTPUT
output_type <- "EXCEL"

gis_data_file <- "Wards_X_GRID3_number-settlements.xlsx"
gis_data_sheet <-  "Feuil1"

#FOLDER WHERE KOBO OUTPUT IS LOCATED WITHIN PARENT FOLDER
folder_with_kobo <- "Nov_May"

#KOBO FILE TO OPEN--"CLEANED" DATASET
CLEANED_DATASET <- "H2R_Clean_Dataset_Nov18_May19"
cleaned_excel_sheet_name = "Sheet1"  #IGNORE If THE FILE IS A CSV

#DATA YEAR: DOES NOT MATTER IF "ALL" MONTHS ARE AGGREGATED 
#2018 = "18"; 2021 = "21" etc... (text)
year_data<- "19"

#CHOOSE MONTH: "ALL"; otherwise "01" to "12" (characters)
month_choose <- "04"

#####LOAD PACKAGES#####
if (!require(readxl)) install.packages('readxl')
library(readxl)

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

#FOLDER WHERE THE CLEANED KOBO OUTPUT IS STORED
folder_with_kobo <- paste0(folder_with_kobo,"/")   #DON'T TOUCH

#DATA OF CLEANED NAMES --- DO NOT TOUCH
adjusted_all_months_name <- gsub("Clean_Dataset", "Settlements_Merged", CLEANED_DATASET)

#FILE NAMES BASED ON MONTH
if(month_choose =="11"){
  named_dataset <- "H2R_Settlements_Merged_Month_Nov"
} else if (month_choose =="12"){
  named_dataset <- "H2R_Settlements_Merged_Month_Dec"
} else if(month_choose =="01"){
  named_dataset <- "H2R_Settlements_Merged_Month_Jan"
}else if(month_choose =="02"){
  named_dataset <- "H2R_Settlements_Merged_Month_Feb"
}else if(month_choose =="03"){
  named_dataset <- "H2R_Settlements_Merged_Month_Mar"
}else if(month_choose =="04"){
  named_dataset <- "H2R_Settlements_Merged_Month_Apr"
}else if(month_choose =="05"){
  named_dataset <- "H2R_Settlements_Merged_Month_May"
}else if(month_choose =="06"){
  named_dataset <- "H2R_Settlements_Merged_Month_Jun"
}else if(month_choose =="07"){
  named_dataset <- "H2R_Settlements_Merged_Month_Jul"
}else if(month_choose =="08"){
  named_dataset <- "H2R_Settlements_Merged_Month_Aug"
}else if(month_choose =="09"){
  named_dataset <- "H2R_Settlements_Merged_Month_Sep"
}else if(month_choose =="10"){
  named_dataset <- "H2R_Settlements_Merged_Month_Oct"
}else if(month_choose =="ALL"){
  named_dataset <- adjusted_all_months_name
} else{ named_dataset <- "NO_NAME"
}

# files we save
if(month_choose !="ALL"){
  AGGREGATED_DATASET <- paste0(named_dataset,year_data,".csv")
  } else if (month_choose =="ALL"){
    AGGREGATED_DATASET <- paste0(adjusted_all_months_name,".csv")
  } else{AGGREGATED_DATASET <- print("NO NAME")
  }
  

## Aggregating function to pick "Yes" over "no" responses
aok_yes <- function(x) {
  ux <- unique(x[!is.na(x)])
  # This checks to see if we have more than one mode (a tie), return blank if so.
  if ("yes" %in% x) {
    return("yes") # Blanks are coerced to values in logical vectors, so we specifically identify columns with TRUE/FALSE (KoBo's select multiples) and output a "logical" blank.
  }
  else if("no" %in% x) {
    return("no")
  }
  
  else {
    return("")
  }
}


## Aggregating function to pick "no" over "yes" responses

aok_no <- function(x) {
  ux <- unique(x[!is.na(x)])
  # This checks to see if we have more than one mode (a tie), return blank if so.
  if ("no"%in% x) {
    return("no") # Blanks are coerced to values in logical vectors, so we specifically identify columns with TRUE/FALSE (KoBo's select multiples) and output a "logical" blank.
  }
  else if("yes" %in% x) {
    return("yes")
  }
  
  else {
    return("")
  }
  
}



## Aggregating function to calculate mode, while outputting NC (No consensus) if we don't have a clear winner.

aok_mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  # This checks to see if we have more than one mode (a tie), return blank if so.
  if (length(which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))) > 1) {
    if (class(x) == "logical") {
      return(as.logical("")) # Blanks are coerced to values in logical vectors, so we specifically identify columns with TRUE/FALSE (KoBo's select multiples) and output a "logical" blank.
    }
    else {
      return("")
    }
  }
  
  else {
    ux[which.max(tabulate(match(x, ux)))] ## This occurs if no tie, so we return the value which has max value! Wambam.
  }
  
}


AoK <- function(x) {
  aok_mode(x)
}

## Aggregating function to pick most recent responses  over others. For frequency questions

aok_frequency <- function(x) {
  ux <- unique(x[!is.na(x)])
  # This checks to see if we have more than one mode (a tie), return blank if so.
  if ("week"%in% x) {
    return("week")
  }
  else if("two_weeks" %in% x) {
    return("two_weeks")
  }
  else if("one_month" %in% x) {
    return("one_month")
  }
  else if("few_months" %in% x) {
    return("few_months")
  }
  else if("emergency" %in% x) {
    return("emergency")
  }
  else if("asneeded" %in% x) {
    return("asneeded")
  }
  else if("dontknow" %in% x) {
    return("dontknow")
  }
  
  else {
    return("")
  }
  
}

##Aggregating function to pick most recent responses  over others. For duration questions

aok_recent <- function(x) {
  ux <- unique(x[!is.na(x)])
  # This checks to see if we have more than one mode (a tie), return blank if so.
  if ("1_month"%in% x) {
    return("1_month")
  }
  else if ("3_months"%in% x) {
    return("3_months")
  }
  else if("4_6_months" %in% x) {
    return("4_6_months")
  }
  else if("7_12_months" %in% x) {
    return("7_12_months")
  }
  else if("1_year" %in% x) {
    return("1_year")
  }
  else if("dontknow" %in% x) {
    return("dontknow")
  }
  
  else {
    return("")
  }
  
}

moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}

## read.csv() simply reads in a .csv file to R, which we assign to the data frame "d.f" using the assignment
## operator, "<-". "d.f" is now an object within R itself, and manipulation of "d.f." has no impact on the
## original .csv (unlike Excel, for example)!!

if(output_type == "CSV"){
  print("KOBO OUTPUT = CSV")
  d.f <- read.csv(paste0(folder_with_kobo,CLEANED_DATASET,".csv"), stringsAsFactors = FALSE, skipNul = TRUE, colClasses = "character")
} else if (output_type == "EXCEL"){
print("KOBO OUTPUT = EXCEL")
  d.f <- read_excel(paste0(folder_with_kobo,CLEANED_DATASET,".xlsx"), cleaned_excel_sheet_name)
} else {
  print("NO KOBO OUTPUT TYPE DEFINED")
}
## Let's remove columns we don't need. Notes first, no data in those!!

d.f <- dplyr::select(d.f, everything(), -contains("X_notes"))

d.f$C_info_settlement_final <- tolower(d.f$C_info_settlement_final)

#CALCULATE THE MONTH CREATED
d.f$new_date <-sub("\\T.*","",d.f$end)
d.f$month <- substr(d.f$new_date,6,7) 
#SUBSET BY MONTH  
d.f$month
if( month_choose != "ALL") {
d.f <- subset(d.f, month==month_choose)
} else{
  d.f <- d.f
}
## CREATING THE SETTLEMENTS DATASET WOOHOO!
settlement_yes <- d.f %>%
  dplyr:: select(C_info_state, C_info_lga, C_info_ward, C_info_settlement_final,
                 D_D1_hc_now, D_D2_idp_now, D_D3_returnees_now, E_food_wild_now, E_food_distr, E_market_now, E_cereal_price_increase,
                 E_harvest_who, F_health_now, F_supp_feeding, F_mortality_increase, G_prot_incidence, G_prot_looting, G_prot_unaccompanied,
                 G_disputes_violence, G_lighting, G_night_walk, G_abduction_girls, G_abduction_boys, G_suicide_bombing, I_I2_shelter_open_yn,
                 I_shelter_damage, I_shelter_flooding, I_nfi_distribution, J_water_boreholes, J_water_boreholes_functional, J_water_source_animals,
                 J_water_safety, J_latrine_now, K_edu_now, K_edu_girl_formal, K_edu_girl_informal, K_edu_boy_formal, K_edu_boy_informal,
                 L_community_leadership, L_idp_leadership, M_mines, M_mine_acc_numb, N_mobilephone, N_prevent_info) %>%
  dplyr::group_by_(.dots = c("C_info_state", "C_info_lga", "C_info_ward", "C_info_settlement_final")) %>% ## .dots basically accepts the column values for the function. In this function, we are saying group by the three columns listed!
  dplyr::  summarise_all(funs(aok_yes))


## Let us apply the "aok_no" function to these set of columns

settlement_no <- d.f %>%
  dplyr:: select(C_info_state, C_info_lga, C_info_ward,C_info_settlement_final,E_food_now, E_livelihoods_barriers, E_land_available, E_ag_inputs,E_livestock_possession,
                 E_livestock_access) %>%
  dplyr:: group_by_(.dots = c("C_info_state", "C_info_lga", "C_info_ward", "C_info_settlement_final")) %>% ## .dots basically accepts the column values for the function. In this function, we are saying group by the three columns listed!
  dplyr:: summarise_all(funs(aok_no))

## Let us apply the "aok_mode" function to these set of columns
settlement_equal <- d.f %>%
  dplyr::  select(C_info_state, C_info_lga, C_info_ward,C_info_settlement_final, D_D1_hc_remain_perc, D_D2_idp_perc, D_D2_idp_source_state, D_D2_idp_source_lga,
                  D_D2_idp_source_ward, D_D4_child_perc, D_D4_pregnant_lactating_perc, E_food_no_reason1, E_food_now_type_fruit,
                  E_food_now_type_main_staples, E_food_now_type_meat, E_food_now_type_pulses, E_food_now_type_milk_dairy, E_food_now_type_vegetables,
                  E_food_source, E_meals_number, E_food_coping_consumption_borrow_food, E_food_coping_consumption_less_expensive_food,
                  E_food_coping_consumption_limit_meal_size, E_food_coping_consumption_wild_food, E_food_coping_consumption_only_children_eat,
                  E_food_coping_consumption_reduce_meals, E_food_coping_consumption_skip_days, E_food_coping_livelihoods_borrow_food,
                  E_food_coping_livelihoods_borrow_money, E_food_coping_livelihoods_gather_wild_food, E_food_coping_livelihoods_consume_seeds,
                  E_food_coping_livelihoods_send_children_to_neighbors, E_food_coping_livelihoods_household_begs, E_food_coping_livelihoods_sell_home_assets,
                  E_food_coping_livelihoods_sell_livestock, E_food_coping_livelihoods_slaughter_livestock, E_food_coping_livelihoods_hunting,
                  E_food_coping_livelihoods_cattle_camps, E_food_coping_livelihoods_fishing, E_food_coping_livelihoods_displacement_camp,
                  E_food_coping_livelihoods_none, E_food_coping_consumption_none, E_shock_hunger, E_shock_arrival, E_shock_cereal_price,
                  E_shock_protection, E_current_activities_casual_labour, E_current_activities_crops_for_cash, E_current_activities_crops_for_sustenance,
                  E_current_activities_livestock, E_current_activities_hunting, E_current_activities_fishing, E_current_activities_market,
                  E_current_activities_salaries, E_current_activities_remittances, E_current_activities_poultry, E_current_activities_none,
                  E_livelihoods_practice_subsistence, E_livelihoods_practice_farmingsell, E_livelihoods_practice_grazing, E_livelihoods_practice_markets,
                  E_livelihoods_practice_transportation, E_livelihoods_practice_trade, E_livelihoods_practice_artisan, E_livelihoods_practice_service,
                  E_livelihoods_barriers_choice_farmland, E_livelihoods_barriers_choice_grazing, E_livelihoods_barriers_choice_markets,
                  E_livelihoods_barriers_choice_transportation, E_livelihoods_barriers_choice_waterways, E_harvest_use, E_fuel_cooking, E_fuel_lighting,
                  E_fuel_obtain, E_fuel_tripstime, E_fuel_insufficient, F_health_dist, F_health_no_reason1, F_health_problems1, F_death_cause1,
                  F_mortality_increase_why, G_prot_now, G_prot_women_concern, G_prot_men_concern, G_prot_girls_concern, G_prot_boys_concern,
                  G_comm_relations, G_disputes, I_I1_nd_shelter_type1, I_I1_nd_location, I_I1_rt_location, I_I1_rt_shelter_type1,
                  I_I2_idp_location, I_I2_idp_shelter_type1, I_I2_shelter_open, I_shelter_materials_grass, I_shelter_materials_mud,
                  I_shelter_materials_timber, I_shelter_materials_rope, I_shelter_materials_cgi, I_shelter_materials_ps, I_shelter_materials_pole,
                  I_shelter_materials_none, I_shelter_destroyed_percent, I_nfi_available_blanket, I_nfi_available_sleeping_mat,
                  I_nfi_available_jerry_can, I_nfi_available_cooking_pot, I_nfi_available_mosquito_net, I_nfi_available_bucket, I_nfi_available_soap,
                  I_nfi_available_none, I_nfi_source_pre_displacement, I_nfi_source_didnt_get, I_nfi_source_market, I_nfi_source_local_community,
                  I_nfi_source_ngo, I_nfi_source_other, I_shelter_materials_other, I_nfi_available_other, E_current_activities_other,
                  E_livelihoods_barriers_choice_other, E_livelihoods_practice_other, I_nfi_need1, J_water_now_how, J_water_now_time,
                  J_latrine_usage, J_latrine_no_usage, J_hand_washing, K_edu_no_reason, K_edu_girl_formal_attendance, K_edu_girl_informal_attendance,
                  K_attendance_no_reason1_girls, K_edu_boy_formal_attendance, K_edu_boy_informal_attendance, K_attendance_no_reason1_boys,
                  L_community_leadership_groups_youth, L_community_leadership_groups_women, L_community_leadership_groups_elderly,
                  L_community_leadership_groups_disabled, L_community_leadership_groups_none, N_info_source, N_info_source_who, N_electricity,
                  N_mobilephone_percent, N_prevent_info_how, N_prevent_info_what, N_info_trust) %>%
  dplyr:: group_by_(.dots = c("C_info_state", "C_info_lga", "C_info_ward", "C_info_settlement_final")) %>% ## .dots basically accepts the column values for the function. In this function, we are saying group by the three columns listed!
  dplyr::  summarise_all(funs(AoK))

## Let us apply the "aok_frequency" function to these set of columns
settlement_frequency <- d.f %>%
  dplyr::  select(C_info_state, C_info_lga, C_info_ward,C_info_settlement_final, L_leadership_meetings) %>%
  dplyr:: group_by_(.dots = c("C_info_state", "C_info_lga", "C_info_ward", "C_info_settlement_final")) %>% ## .dots basically accepts the column values for the function. In this function, we are saying group by the three columns listed!
  dplyr:: summarise_all(funs(aok_frequency))

## Let us apply the "aok_recent" function to these set of columns
settlement_recent <- d.f %>%
  dplyr::  select(C_info_state, C_info_lga, C_info_ward, C_info_settlement_final,D_D2_idp_time_arrive,D_D3_returnee_time_arrived) %>%
  dplyr::  group_by_(.dots = c("C_info_state", "C_info_lga", "C_info_ward", "C_info_settlement_final")) %>% ## .dots basically accepts the column values for the function. In this function, we are saying group by the three columns listed!
  dplyr::  summarise_all(funs(aok_recent))

## Averaging the one integer question

settlement_numbers <- d.f %>%
  dplyr::  select(C_info_state, C_info_lga, C_info_ward, C_info_settlement_final, M_quantity_uxo) %>%
  dplyr:: group_by_(.dots = c("C_info_state", "C_info_lga", "C_info_ward", "C_info_settlement_final")) 
settlement_numbers<- as.data.frame(settlement_numbers)
settlement_numbers$M_quantity_uxo <- as.numeric(settlement_numbers$M_quantity_uxo)
settlement_numbers$new_id <- paste(settlement_numbers$C_info_state,settlement_numbers$C_info_lga,settlement_numbers$C_info_ward,settlement_numbers$C_info_settlement_final,sep=".") 
settlement_numbers$onesz <- 1 
settlement_numbers_old <- settlement_numbers[!duplicated(settlement_numbers$new_id), ]
settlement_numbers<-settlement_numbers %>% dcast(new_id~onesz, fun.aggregate = mean,value.var="M_quantity_uxo", na.rm=TRUE)
settlement_numbers[sapply(settlement_numbers, is.nan)] <- ""
colnames(settlement_numbers)[2] <-  "M_quantity_uxo"
settlement_numbers<-merge(settlement_numbers_old,settlement_numbers,by="new_id",all.x=TRUE)
settlement_numbers[c(1,6,7)]<-NULL
colnames(settlement_numbers)[5] <-  "M_quantity_uxo"
settlement_numbers_old<-NULL

#table join. Let us merge all these columns/fields into one database
Settlement1a<-merge(settlement_yes, settlement_no, by = c("C_info_state", "C_info_lga", "C_info_ward", "C_info_settlement_final") )
Settlement1b<-merge(settlement_equal, settlement_recent, by = c("C_info_state", "C_info_lga", "C_info_ward", "C_info_settlement_final") )
Settlement1c<-merge(Settlement1a, Settlement1b, by = c("C_info_state", "C_info_lga", "C_info_ward", "C_info_settlement_final") )
Settlement1d<-merge(Settlement1c, settlement_numbers, by = c("C_info_state", "C_info_lga", "C_info_ward", "C_info_settlement_final") )
settlement<-merge(Settlement1d, settlement_frequency, by = c("C_info_state", "C_info_lga", "C_info_ward", "C_info_settlement_final") )



# #Let us rearrange the columns in our database inthe same order appear on the tool
settlement <- settlement %>%  dplyr:: select(order(match(names(settlement), names(d.f))))

# ## Now we want to clean the dataset for some skiplogic errors and some NAs that have appeared after our settlement
## aggregation has finished. This looks long, but it's very simple! Then we can save it, print it, and BAM! Done.

## SKIP LOGIC
# The following sets ensure that when data is aggregated to the settlement level, if there are a mix of yes/no, that the consensus response respects the skip logic
# if a question is supposd to be skipped after selecting "no" to a previous question, ensure it remains blank
# if a question is supposd to be skipped after selecting "yes" to a previous question, ensure it remains blank

## No host community

settlement$D_D1_hc_remain_perc[settlement$D_D1_hc_now != "yes"] <- "SL"

## No IDPs

settlement$D_D2_idp_perc[settlement$D_D2_idp_now != "yes"] <- "SL"
settlement$D_D2_idp_time_arrive[settlement$D_D2_idp_now != "yes"] <- "SL"
settlement$D_D2_idp_source_state[settlement$D_D2_idp_now != "yes"] <- "SL"
settlement$D_D2_idp_source_lga[settlement$D_D2_idp_now != "yes"] <- "SL"
settlement$D_D2_idp_source_ward[settlement$D_D2_idp_now != "yes"] <- "SL"
settlement$D_D2_idp_source_settlement[settlement$D_D2_idp_now != "yes"] <- "SL"

#No returnees

settlement$D_D3_returnee_time_arrived[settlement$D_D3_returnees_now !="yes"]<- "SL"

## Food
settlement$E_food_no_reason1[settlement$E_food_now != "no"] <- "SL"
settlement$E_cereal_price_increase[settlement$E_market_now != "yes"] <- "SL"
settlement$E_shock_hunger[settlement$E_food_now != "no"] <- "SL"
settlement$E_shock_arrival[settlement$D_D2_idp_now != "yes" & settlement$D_D3_returnees_now != "yes"] <- "SL"
settlement$E_shock_cereal_price[settlement$E_cereal_price_increase != "yes"] <- "SL"

# Livelihoods barriers
settlement$E_livelihoods_barriers_choice_farmland[settlement$E_livelihoods_barriers != "no"] <- "SL"
settlement$E_livelihoods_barriers_choice_waterways[settlement$E_livelihoods_barriers != "no"] <- "SL"
settlement$E_livelihoods_barriers_choice_grazing[settlement$E_livelihoods_barriers != "no"] <- "SL"
settlement$E_livelihoods_barriers_choice_markets[settlement$E_livelihoods_barriers != "no"] <- "SL"
settlement$E_livelihoods_barriers_choice_transportation[settlement$E_livelihoods_barriers != "no"] <- "SL"
settlement$E_livelihoods_barriers_choice_other[settlement$E_livelihoods_barriers != "no"] <- "SL"

# Health
settlement$F_health_dist[settlement$F_health_now != "yes"] <- "SL"
settlement$F_health_no_reason1[settlement$F_health_now != "no"] <- "SL"
settlement$F_mortality_increase_why[settlement$F_mortality_increase != "yes"] <- "SL"

# Protection
settlement$G_comm_relations[settlement$D_D2_idp_now != "yes"] <- "SL"
settlement$G_disputes_violence[settlement$G_comm_relations != "neutral" & settlement$G_comm_relations != "poor"] <- "SL"

# Shelter
settlement$I_I1_nd_location[settlement$D_D1_hc_now != "yes"] <- "SL"
settlement$I_I1_nd_shelter_type1[settlement$D_D1_hc_now != "yes"] <- "SL"
settlement$I_I1_rt_location[settlement$D_D3_returnees_now != "yes"] <- "SL"
settlement$I_I1_rt_shelter_type1[settlement$D_D3_returnees_now != "yes"] <- "SL"

settlement$I_I2_idp_location[settlement$D_D2_idp_now != "yes"] <- "SL"
settlement$I_I2_idp_shelter_type1[settlement$D_D2_idp_now != "yes"] <- "SL"
settlement$I_I2_shelter_open_yn[settlement$D_D2_idp_now != "yes"] <- "SL"

settlement$I_I2_shelter_open[settlement$I_I2_shelter_open_yn != "yes"] <- "SL"
settlement$I_shelter_destroyed_percent[settlement$I_shelter_damage != "yes"] <- "SL"
# WASH
settlement$J_water_boreholes_functional[settlement$J_water_boreholes != "yes"] <- "SL"
settlement$J_latrine_usage[settlement$J_latrine_now != "yes"] <- "SL"
settlement$J_latrine_no_usage[settlement$J_latrine_usage %in% c("more_half", "dontknow", "SL")] <- "SL"

# Education
settlement$K_edu_no_reason[settlement$K_edu_now != "no"] <- "SL"
settlement$K_edu_boy_formal[settlement$K_edu_now != "yes"] <- "SL"
settlement$K_edu_boy_informal[settlement$K_edu_now != "yes"] <- "SL"
settlement$K_edu_girl_formal[settlement$K_edu_now != "yes"] <- "SL"
settlement$K_edu_girl_informal[settlement$K_edu_now != "yes"] <- "SL"

settlement$K_edu_boy_formal_attendance[settlement$K_edu_boy_formal != "yes"] <- "SL"
settlement$K_edu_boy_informal_attendance[settlement$K_edu_boy_informal != "yes"] <- "SL"
settlement$K_edu_girl_formal_attendance[settlement$K_edu_girl_formal != "yes"] <- "SL"
settlement$K_edu_girl_informal_attendance[settlement$K_edu_girl_informal != "yes"] <- "SL"

# CCCM
settlement$L_community_leadership_groups_disabled[settlement$L_community_leadership != "yes"] <- "SL"
settlement$L_community_leadership_groups_women[settlement$L_community_leadership != "yes"] <- "SL"
settlement$L_community_leadership_groups_elderly[settlement$L_community_leadership != "yes"] <- "SL"
settlement$L_community_leadership_groups_youth[settlement$L_community_leadership != "yes"] <- "SL"
settlement$L_community_leadership_groups_none[settlement$L_community_leadership != "yes"] <- "SL"

settlement$L_idp_leadership[settlement$D_D2_idp_now != "yes" | settlement$L_community_leadership != "yes"] <- "SL"

# UXOs
settlement$M_quantity_uxo[settlement$M_mines != "yes"] <- "SL"
settlement$M_mine_acc_numb[settlement$M_mines != "yes"] <- "SL"

# Communications
settlement$N_mobilephone_percent[settlement$N_mobilephone != "yes"] <- "SL"
settlement$N_prevent_info_how[settlement$N_prevent_info != "yes"] <- "SL"
settlement$N_prevent_info_what[settlement$N_prevent_info != "yes"] <- "SL"

## Filling in all blank values

settlement[settlement == ""] <- "NC"
settlement$settlement_ward_lga <-  paste0(settlement$C_info_settlement_final,settlement$C_info_ward,settlement$C_info_lga)

## Counting KI coverage per village!

ki_coverage <- d.f %>%
  dplyr::  select(c(C_info_state, C_info_lga, C_info_ward, C_info_settlement_final, B_disp_status)) %>%
  dplyr:: group_by_(.dots = c("C_info_state", "C_info_lga", "C_info_ward", "C_info_settlement_final")) %>%
  dplyr:: summarise(length(B_disp_status)) %>%
  ungroup()
colnames(ki_coverage)[ncol(ki_coverage)]  <- "ki_coverage"

ki_coverage$settlement_ward_lga <- paste0(ki_coverage$C_info_settlement_final,ki_coverage$C_info_ward,ki_coverage$C_info_lga)
ki_coverage <- ki_coverage %>% dplyr:: select(ki_coverage,settlement_ward_lga)
#JOIN
settlement <- merge(settlement,ki_coverage, by="settlement_ward_lga",all.x=TRUE)
#MOVE KI COVERAGE TO THE BEGINNING OF THE DATASET
settlement<-settlement[moveme(names(settlement), "ki_coverage first")]

## Adding month columns

#settlement <- add_column(as.data.frame(settlement), month = rep(month_data, len = nrow(settlement)), .before = 1)

## Writing the files, we are done

################################SETTLEMENT & WARD PROPORTIONS################################
#REMOVE "other" WARDS
settlement <- settlement %>% filter(C_info_ward != "other")
settlement$ward_lga <- paste0(settlement$C_info_ward,settlement$C_info_lga)
#ADD JOIN COLUMN TO "settlement"
settlement$settlement_ward_lga <- paste0(settlement$C_info_settlement_final,settlement$C_info_ward,settlement$C_info_lga)
#IMPORT GIS SETTLEMENTS
gis_settlements <- read_excel(paste0("GIS_Settlement_list/",gis_data_file), sheet = gis_data_sheet)
gis_settlements$ADM3_REF_lower <- tolower(gis_settlements$ADM3_REF)
gis_settlements$ADM2_EN_lower <- tolower(gis_settlements$ADM2_EN)
gis_settlements$ward_lga	<- paste0(gis_settlements$ADM3_REF_lower ,gis_settlements$ADM2_EN_lower)
gis_settlements$lga <- gis_settlements$ADM2_EN_lower
gis_settlements$ward_lga <-gis_settlements$ward_lga %<>%
  gsub("&", "_", .) %>%
  gsub(" ", "_", .) 
gis_settlements$oneszz <- 1 
#COUNT SETTLEMENTS PER-WARD
settlement$onesz <- 1
#COUNTS UNIQUE SETTLEMENTS
settlements_cnt <-dcast(settlement, ward_lga ~ onesz, value.var = "C_info_settlement_final", fun.aggregate = length)
colnames(settlements_cnt)[1]<-"ward_lga"
colnames(settlements_cnt)[2]<-"cnt_from_H2R"

#JOIN H2R+WARD DATA AND CALCULATE THE PROPORTION ABOVE THRESHOLD
settlements_cnt<-merge(settlements_cnt,gis_settlements, by="ward_lga",all.x=TRUE)
settlements_cnt$ward_htr_settle_cnt <- as.numeric(settlements_cnt$cnt_from_H2R)
settlements_cnt$ward_gis_settle_cnt <- as.numeric(ifelse(is.na(settlements_cnt$Number_of_settlements),0,settlements_cnt$Number_of_settlements))
#PERCENT OF SETTLEMENTS IN WARDS
settlements_cnt$ward_prcnt_of_H2R <- as.numeric(settlements_cnt$ward_htr_settle_cnt/settlements_cnt$ward_gis_settle_cnt)
settlements_cnt[sapply(settlements_cnt, is.infinite)] <- 0
settlements_cnt$ward_prcnt_of_H2R <- ceiling(settlements_cnt$ward_prcnt_of_H2R/0.005)*0.005
settlements_cnt$ward_prcnt20ov_yn <- ifelse(settlements_cnt$ward_prcnt_of_H2R >=.2,1,0)
#SUBSET GIS+SETTLMENT MERGE AND JOIN TO MAIN DATASET
settlements_cnt <- subset(settlements_cnt, select=c(ward_lga,ward_htr_settle_cnt,ward_gis_settle_cnt,ward_prcnt_of_H2R,ward_prcnt20ov_yn))
settlement<- merge(settlement,settlements_cnt,by="ward_lga",all.x=TRUE)
#write.csv(settlement,"check_wards.csv")

#COUNT SETTLEMENTS PER-LGA
#COUNTS UNIQUE SETTLEMENTS
settlement$state_lga <- paste0(settlement$C_info_state,";",settlement$C_info_lga)
settlements_lga_cnt <-dcast(settlement, state_lga ~ onesz, value.var = "C_info_settlement_final", fun.aggregate = length)
colnames(settlements_lga_cnt)[1]<-"state_lga"
colnames(settlements_lga_cnt)[2]<-"lga_settle_cnt_H2R"
settlement <- merge(settlement,settlements_lga_cnt, by="state_lga", all.x=TRUE)

#GIS SETTLEMENTS PER-LGA
gis_settlements$state_lga <- tolower(paste0(gis_settlements$ADM1_EN,";",gis_settlements$ADM2_EN))
gis_lga_settlement_cnt <-dcast(gis_settlements, state_lga ~ oneszz, value.var = "Number_of_settlements", fun.aggregate = sum)
colnames(gis_lga_settlement_cnt)[1]<-"state_lga"
colnames(gis_lga_settlement_cnt)[2]<-"lga_settle_cnt_GIS"
settlement <- merge(settlement,gis_lga_settlement_cnt, by="state_lga", all.x=TRUE)
settlement$lga_prcnt_of_H2R <- settlement$lga_settle_cnt_H2R/settlement$lga_settle_cnt_GIS
settlement$lga_prcnt20ov_yn <- ifelse(settlement$lga_prcnt_of_H2R>=.2,1,0)

#CHANGE WORKING DIRECTORY & EXPORT
settlement$onesz <- NULL
setwd(paste0(parent_folder,"/","Settlements_Merged"))
write.csv(settlement, AGGREGATED_DATASET, na = "NC", row.names = TRUE)


