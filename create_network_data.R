#  Copyright 2016, INSEAD
#  by T. Evgeniou, Theo Vermaelen, and Ling Yue
#  Dual licensed under the MIT or GPL Version 2 licenses.

##########################################################################################
# Creates the raw data for the buybacks network paper
##########################################################################################

rm(list=ls()) # Clean up the memory, if we want to rerun from scratch

source("../FinanceLibraries/lib_helpers.R", chdir=TRUE)
source("../FinanceLibraries/latex_code.R")
source("../FinanceData/rawdata_fama_french/ff_industries_sic.R")
source("Paper_global_parameters.R")

load("../FinanceData/created_projects_datasets/BUYBACKSnew.Rdata")
BUYBACK_DATA_NETWORK = BUYBACK_DATA
rm("BUYBACK_DATA")
Risk_Factors_Monthly = BUYBACK_DATA_NETWORK$Risk_Factors_Monthly

##########################################################################################
#### Add the customer supplier data
##########################################################################################

load("../FinanceData/created_supplier_customer_networkdata/GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE.Rdata")
load("../FinanceData/created_supplier_customer_networkdata/GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE_SIMPLE.Rdata")

#########################
# First add the scores for the networks

for (field in c("supplier_betweenness1","supplier_betweenness2","supplier_degree1","supplier_degree2","supplier_eigenvector2","supplier_kbcntrl2"))
  if (!is.null(dim(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE[[field]])))
    GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE[[paste(field,"score",sep="_")]] <- get_cross_section_score(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE[[field]])

for (field in c("dgr1cust_all","dgr1cust_subs","dgr1sc_all","dgr1sc_subs","dgr1supp_all","dgr1supp_subs","dgr2cust_all",
                "dgr2cust_subs","dgr2sc_all","dgr2sc_subs","dgr2supp_all","dgr2supp_subs",
                "eigenvector2_cust","degree2_cust","degree1_cust","eigenvector2_strg","degree2_strg","degree1_strg"))
  if (!is.null(dim(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE_SIMPLE[[field]])))
    GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE_SIMPLE[[paste(field,"score",sep="_")]] <- get_cross_section_score(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE_SIMPLE[[field]])

for (commonname in intersect(names(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE), names(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE_SIMPLE)))
  GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE_SIMPLE[[commonname]] <- NULL

#########################
### Add the data now
load("../FinanceData/created_monthly_data/GLOBAL_MONTHLY_DATABASE.Rdata")

# A helper that allows to add more company features in the data
get_feature_from_feature_datamatrix <- function(firm_characteristic_monthly){
  firm_characteristic_monthly[is.na(GLOBAL_MONTHLY_DATABASE$returns_monthly)] <- NA
  
  event_permno = as.character(BUYBACK_DATA_NETWORK$DATASET$SDC$permno)
  event_month = str_sub(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date,start=1,end=7)
  check_only =  event_month %in% rownames(firm_characteristic_monthly) & event_permno %in% colnames(firm_characteristic_monthly)
  sapply(1:length(BUYBACK_DATA_NETWORK$DATASET$SDC$permno), function(i) ifelse(check_only[i], firm_characteristic_monthly[event_month[i], event_permno[i]], NA))
}

# Raw data first 
BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network <- list()
useonly = which(!str_detect(names(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE),"score")) # Note these are the first ones anyway
#useonly = 1:length(names(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE))
for (iter_i in 1:length(useonly)){
  iter = useonly[iter_i]
  cat(iter,", ")
  tmp = GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE[[iter]]
  rownames(tmp) <- str_sub(as.character(AddMonths(as.Date(rownames(tmp)), +1)), start=1, end=7) #forget the month date, get the previous month
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network[[iter_i]] =  get_feature_from_feature_datamatrix(tmp)
  names(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network)[iter_i]<- names(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE)[iter]
}

# Scores now 
BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores <- list()
useonly = which(
  str_detect(names(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE), "score") &
    str_detect(names(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE), "supplier"))
for (iter_i in 1:length(useonly)){
  iter = useonly[iter_i]
  cat(iter,", ")
  tmp = GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE[[iter]]
  rownames(tmp) <- str_sub(as.character(AddMonths(as.Date(rownames(tmp)), +1)), start=1, end=7) #forget the month date, get the previous month
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores[[iter_i]] =  get_feature_from_feature_datamatrix(tmp)
  names(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores)[iter_i]<- names(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE)[iter]
}

############
## Add the simple network data now. 
# Raw data first 
#Start from the index we reached so far
start_count = length(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network)
useonly = which(!str_detect(names(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE_SIMPLE),"score")) # Note these are the first ones anyway
for (iter_i in 1:length(useonly)){
  iter = useonly[iter_i]
  cat(iter,", ")
  tmp = GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE_SIMPLE[[iter]]
  rownames(tmp) <- str_sub(as.character(AddMonths(as.Date(rownames(tmp)), +1)), start=1, end=7) #forget the month date, get the previous month
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network[[start_count+iter_i]] =  get_feature_from_feature_datamatrix(tmp)
  names(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network)[start_count+iter_i]<- names(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE_SIMPLE)[iter]
}

# Scores now 
#Start from the index we reached so far
start_count = length(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores)
useonly = which(str_detect(names(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE_SIMPLE), "score"))
for (iter_i in 1:length(useonly)){
  iter = useonly[iter_i]
  cat(iter,", ")
  tmp = GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE_SIMPLE[[iter]]
  rownames(tmp) <- str_sub(as.character(AddMonths(as.Date(rownames(tmp)), +1)), start=1, end=7) #forget the month date, get the previous month
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores[[start_count+iter_i]] =  get_feature_from_feature_datamatrix(tmp)
  names(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores)[start_count+iter_i]<- names(GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE_SIMPLE)[iter]
}

#########################
### Add the suppliers analysts data now

load("../FinanceData/created_ibes_data/GLOBAL_IBES_SUPPLYANALYSTS_DATABASE.Rdata")

# Create some other interesting company features first
GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$NdirectPercent <- ifelse(scrub(GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$Nanalyst), GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$Ndirect/GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$Nanalyst, GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$Nanalyst)
GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$Ndirect_followPercent <- ifelse(scrub(GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$Nanalyst), GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$Ndirect_follow/GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$Nanalyst, GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$Nanalyst)
GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$Ngeneral_over_Nanalyst <- ifelse(scrub(GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$Nanalyst), GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$Ngeneral/GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$Nanalyst, GLOBAL_IBES_SUPPLYANALYSTS_DATABASE$Nanalyst)

# First add the scores for the analysts

for (field in names(GLOBAL_IBES_SUPPLYANALYSTS_DATABASE))
  if (!is.null(dim(GLOBAL_IBES_SUPPLYANALYSTS_DATABASE[[field]])))
    GLOBAL_IBES_SUPPLYANALYSTS_DATABASE[[paste(field,"score",sep="_")]] <- get_cross_section_score(GLOBAL_IBES_SUPPLYANALYSTS_DATABASE[[field]],zero_special=T) # WE TREAT 0s AS 0s, not as NAs

### Add the data now
BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts = list()
useonly = 1:length(names(GLOBAL_IBES_SUPPLYANALYSTS_DATABASE))
for (iter_i in 1:length(useonly)){
  iter = useonly[iter_i]
  cat(iter,", ")
  tmp = GLOBAL_IBES_SUPPLYANALYSTS_DATABASE[[iter]]
  rownames(tmp) <- str_sub(as.character(AddMonths(as.Date(rownames( GLOBAL_IBES_SUPPLYANALYSTS_DATABASE[[iter]])), +1)), start=1, end=7) #forget the month date, get the previous month
  BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts[[iter_i]] =  get_feature_from_feature_datamatrix(tmp)
  names(BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts)[iter_i]<- names(GLOBAL_IBES_SUPPLYANALYSTS_DATABASE)[iter]
}

rm("tmp","get_feature_from_feature_datamatrix")

##########################################################################################
# Now some clean up as we only use the events for which we have all data, and also are outside our period

cleanup_network = list()

cleanup_network$initial_data = length(BUYBACK_DATA_NETWORK$DATASET$SDC$CUSIP)
to_remove = which(!(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date >= "1996-10-01" & BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date < "2016-01-01"))
if (length(to_remove) > 0){
  
  BUYBACK_DATA_NETWORK$clenupnetwork_outsideperiod = length(to_remove)
  
  # just in alphabetic order not to forget any   
  BUYBACK_DATA_NETWORK$BEME_used <- BUYBACK_DATA_NETWORK$BEME_used[-to_remove]
  BUYBACK_DATA_NETWORK$Performance_used <- BUYBACK_DATA_NETWORK$Performance_used[-to_remove]
  BUYBACK_DATA_NETWORK$Size_used <- BUYBACK_DATA_NETWORK$Size_used[-to_remove]
  BUYBACK_DATA_NETWORK$Valuation_Index <- BUYBACK_DATA_NETWORK$Valuation_Index[-to_remove]
  
  BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly <- BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,-to_remove]
  BUYBACK_DATA_NETWORK$DATASET$SDC <- BUYBACK_DATA_NETWORK$DATASET$SDC[-to_remove,]
  for(field in ls(BUYBACK_DATA_NETWORK$DATASET$CRSP))  BUYBACK_DATA_NETWORK$DATASET$CRSP[[field]] <- BUYBACK_DATA_NETWORK$DATASET$CRSP[[field]][-to_remove]
  for(field in ls(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network))  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network[[field]] <- BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network[[field]][-to_remove]
  for(field in ls(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores))  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores[[field]] <- BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores[[field]][-to_remove]
  for(field in ls(BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts))  BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts[[field]] <- BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts[[field]][-to_remove]
  
  for(field1 in ls(BUYBACK_DATA_NETWORK$DATASET$ibes))  
    for (field in ls(BUYBACK_DATA_NETWORK$DATASET$ibes[[field1]])) 
      BUYBACK_DATA_NETWORK$DATASET$ibes[[field1]][[field]]<- BUYBACK_DATA_NETWORK$DATASET$ibes[[field1]][[field]][-to_remove]
}

cleanup_network$pre_missingnetwork = length(BUYBACK_DATA_NETWORK$DATASET$SDC$CUSIP)

features_used = which(names(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network) %in% 
                        c("supplier_degree1","supplier_degree2","supplier_eigenvector2","supplier_kbcntrl2"))
#                          "dgr1sc_subs","dgr1cust_subs"))
features_used_score = which(names(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores) %in% 
                              c("supplier_degree1_score","supplier_degree2_score","supplier_eigenvector2_score","supplier_kbcntrl2_score"))
#                                "dgr1sc_subs_score","dgr1cust_subs_score"))
to_remove = which(
  # !(BUYBACK_DATA_NETWORK$DATASET$SDC$Industry %in% INDUSTRY_USED) | # We remove these later as needed
  #is.na(BUYBACK_DATA_NETWORK$DATASET$ibes$mean_rec_last_month) |
  #  is.na(BUYBACK_DATA_NETWORK$DATASET$ibes$mean_rec_last_last_month) | 
  Reduce("|", lapply(features_used, function(iter) is.na(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network[[iter]]))) |
    Reduce("|", lapply(features_used_score, function(iter) is.na(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores[[iter]])))
)
if (length(to_remove) > 0){
  
  cleanup_network$missing_network = length(to_remove)
  
  # just in alphabetic order not to forget any   
  BUYBACK_DATA_NETWORK$BEME_used <- BUYBACK_DATA_NETWORK$BEME_used[-to_remove]
  BUYBACK_DATA_NETWORK$Performance_used <- BUYBACK_DATA_NETWORK$Performance_used[-to_remove]
  BUYBACK_DATA_NETWORK$Size_used <- BUYBACK_DATA_NETWORK$Size_used[-to_remove]
  BUYBACK_DATA_NETWORK$Valuation_Index <- BUYBACK_DATA_NETWORK$Valuation_Index[-to_remove]
  
  BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly <- BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,-to_remove]
  BUYBACK_DATA_NETWORK$DATASET$SDC <- BUYBACK_DATA_NETWORK$DATASET$SDC[-to_remove,]
  for(field in ls(BUYBACK_DATA_NETWORK$DATASET$CRSP))  BUYBACK_DATA_NETWORK$DATASET$CRSP[[field]] <- BUYBACK_DATA_NETWORK$DATASET$CRSP[[field]][-to_remove]
  for(field in ls(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network))  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network[[field]] <- BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network[[field]][-to_remove]
  for(field in ls(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores))  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores[[field]] <- BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores[[field]][-to_remove]
  for(field in ls(BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts))  BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts[[field]] <- BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts[[field]][-to_remove]
  
  for(field1 in ls(BUYBACK_DATA_NETWORK$DATASET$ibes))  
    for (field in ls(BUYBACK_DATA_NETWORK$DATASET$ibes[[field1]])) 
      BUYBACK_DATA_NETWORK$DATASET$ibes[[field1]][[field]]<- BUYBACK_DATA_NETWORK$DATASET$ibes[[field1]][[field]][-to_remove]
}

BUYBACK_DATA_NETWORK$cleanupnetwork = cleanup_network 

##########################################################################################
# Finally save only what is needed

save(BUYBACK_DATA_NETWORK, Risk_Factors_Monthly, file = "../FinanceData/created_projects_datasets/bb_network.Rdata")


