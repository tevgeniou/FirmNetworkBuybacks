#  Copyright 2016, INSEAD
#  by T. Evgeniou, Theo Vermaelen, and Ling Yue
#  Dual licensed under the MIT or GPL Version 2 licenses.

##########################################################################################
##########################################################################################
# Filter the data used for the paper
##########################################################################################
##########################################################################################

##########################################################################################
# Remove events for which we have missing values (for the main missing values)
##########################################################################################

to_remove = which(
  # just in alphabetic order not to forget any    
  is.na(BUYBACK_DATA_NETWORK$BEME_used) | 
    is.na(BUYBACK_DATA_NETWORK$Performance_used) | 
    is.na(BUYBACK_DATA_NETWORK$Size_used) | 
    is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$pre_vol_Score) |
    is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$IVOL_score) |
    is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$Rsq_score) |
    is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap) 
)
if (length(to_remove) > 0){
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
  rm("field","field1")
}
BUYBACK_DATA_NETWORK$cleanupMissingSomeValues = length(to_remove)

##########################################################################################
# Project specific filters now 

# Note the use of the CRSP list instead of the SDC list for data like closing prices, market cap, etc (unlike earlier version)
# Buybacks first
events = BUYBACK_DATA_NETWORK$DATASET
No_filter = rep(T,length(events$SDC$Event.Date))
Basic_filter =  No_filter # !is.na(events$SDC$ME_quantile) & !is.na(events$SDC$BEME_quantile)  
Period_filter <- (as.Date(events$SDC$Event.Date) >= First) & (as.Date(events$SDC$Event.Date) <= Last)
#Penny_stock_filter = ifelse(events$SDC$Event.Date < "1995-01-01", scrub(events$SDC$Closing.Price) >= penny_stock_price_old, scrub(events$SDC$Closing.Price) >= penny_stock_price_recent) 
Penny_stock_filter = ifelse(events$SDC$Event.Date < "1995-01-01", scrub(events$CRSP$closing.price) >= penny_stock_price_old, scrub(events$CRSP$closing.price) >= penny_stock_price_recent) 
# REMOVE FINANCIALS AND UTILITIES: We leave them for now so we can use them as needed. This decision is made in bb_issuers_new.R
#Industry_filter = events$SDC$Industry %in% INDUSTRY_USED 
Industry_filter = 1
US_only = (events$SDC$Currency %in% good_currencies)
major_markets_only = sapply(events$SDC$Stock.Exchange, function(i) sum(str_split(i, "\\+")[[1]] %in% major_markets)>0)
#BUYBACK SPECIFIC NOW:
Technique_filter =  events$SDC$Tech..nique.Code %in% BB_allowed_techniques # OP, OPNG, and ""    
TOTAL_FILTER_basic = No_filter & Basic_filter & Period_filter & Penny_stock_filter  & Industry_filter & US_only & major_markets_only & Technique_filter
#Market_cap_filter =  (scrub(events$SDC$Market.Cap) >= MIN_SIZE) & (scrub(events$SDC$Market.Cap) <= MAX_SIZE)
Market_cap_filter =  (scrub(events$CRSP$Market.Cap) >= MIN_SIZE) & (scrub(events$CRSP$Market.Cap) <= MAX_SIZE)
#Leverage_filter = (scrub(events$SDC$lt/events$SDC$at) > 0.5)*(!is.na(events$SDC$lt/events$SDC$at))
EventSize_filter = (events$SDC$Event.Size >= MIN_EVENT_SIZE) & (events$SDC$Event.Size <= MAX_EVENT_SIZE)
TOTAL_FILTER_complex = Market_cap_filter  & EventSize_filter 
### remove
TOTAL_FILTER = TOTAL_FILTER_basic & TOTAL_FILTER_complex
BIZ_initial_data = length(TOTAL_FILTER)
to_remove = which(!TOTAL_FILTER)
if (length(to_remove) > 0){
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
  rm("field","field1")
}
### Keep track
cleanup = list()
cleanup$initial_data <- BIZ_initial_data
cleanup$total_removed = length(to_remove)
cleanup$Basic_filter <- sum(!Basic_filter)
cleanup$Period_filter <- sum(!Period_filter)
cleanup$Penny_stock_filter <- sum(!Penny_stock_filter)
cleanup$Industry_filter <- sum(!Industry_filter)
cleanup$US_only <- sum(!US_only)
cleanup$BIZ_allowed_techniques <- sum(!Technique_filter)
cleanup$major_markets_only <- sum(!major_markets_only)
cleanup$Market_cap_filter <- sum(!Market_cap_filter)
cleanup$EventSize_filter <- sum(!EventSize_filter)
BUYBACK_DATA_NETWORK$cleanupBIZ = cleanup

###### Check time order of events
ordered_events = sort(as.numeric(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date),index.return = T)$ix
if (length(unique(diff(ordered_events)))!=1)
  stop("The time order was messed up somewhere for buybacks")
rm("ordered_events")

############################################################################################################
############################################################################################################

# remove_financials_utilities is defined in the Paper_global_parameters.R file... default is 1
if (remove_financials_utilities){
  
  # Buybacks first
  Industry_filter = BUYBACK_DATA_NETWORK$DATASET$SDC$Industry %in% INDUSTRY_USED 
  to_remove = which(!Industry_filter)
  if (length(to_remove) > 0){
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
    rm("field","field1")
  }
  BUYBACK_DATA_NETWORK$cleanupBIZ$Industry_filter = sum(!Industry_filter)

  rm("to_remove")
}

############################################################################################################

if (remove_missing_permnosV2){
  
  # Buybacks first
  to_remove = which(is.na(BUYBACK_DATA_NETWORK$DATASET$SDC$permnoV2))
  if (length(to_remove) > 0){
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
    rm("field","field1")
  }
  BUYBACK_DATA_NETWORK$cleanupNoPermno = BUYBACK_DATA_NETWORK$cleanupNoPermno + length(to_remove)

  rm("to_remove")
}

############################################################################################################

if (remove_CRSP_minor_markets){
  
  # Buybacks first
  to_remove = which(!(BUYBACK_DATA_NETWORK$DATASET$CRSP$exchange %in% major_markets_crsp))
  if (length(to_remove) > 0){
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
    rm("field","field1")
  }
  BUYBACK_DATA_NETWORK$cleanupBIZ$major_markets_only = BUYBACK_DATA_NETWORK$cleanupBIZ$major_markets_only + length(to_remove)

  rm("to_remove")
}


