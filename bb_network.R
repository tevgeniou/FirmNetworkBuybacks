#  Copyright 2016, INSEAD
#  by T. Evgeniou, Theo Vermaelen, and Ling Yue
#  Dual licensed under the MIT or GPL Version 2 licenses.

if (!exists("generate_all_appendices")){
  rm(list=ls()) # Clean up the memory, if we want to rerun from scratch
  
  # centrality_name can be one of these (in quotes): betweenness1, betweenness2, degree1, degree2, eigenvector2, kbcntrl2 
  # QUANTILE_COMPARED can be one of these numbers: 4,3,2
  centrality_name = "supplier_degree1"
  QUANTILE_COMPARED  = 4
}
centality_used_name = paste(centrality_name, "_score", sep="")

supplier_quantile = 0.5 # How to define the low vs high supply chain analysts quantiles

# For the self-financed CEU and EU portfolios

LOW_CEU_THRESHOLD = 2
HIGH_CEU_THRESHOLD = 6

LOW_EU_THRESHOLD = 1
HIGH_EU_THRESHOLD = 5

HOLDING_PERIOD = "Four.Years.After"

windsorize <- function(r) {r0 = r[!is.na(r)]; minval = quantile(r[!is.na(r)],0.01); maxval= quantile(r[!is.na(r)],0.99); r[!is.na(r)] <- ifelse(r[!is.na(r)] < minval | r[!is.na(r)] > maxval, NA, r[!is.na(r)]); r}


###############################################################################################
source("../FinanceLibraries/lib_helpers.R", chdir=TRUE)
source("../FinanceLibraries/latex_code.R")
source("../FinanceData/rawdata_fama_french/ff_industries_sic.R")
source("Paper_global_parameters.R")

###############################################################################################
initial_vars = ls(all = TRUE) # takes time to save and load, so we save only what is needed at the end. 
centrality_name = centrality_name # just to have this for the knit2pdf
QUANTILE_COMPARED = QUANTILE_COMPARED # just to have this for the knit2pdf

load("../FinanceData/created_projects_datasets/bb_network.Rdata")

if (continuous_valuation_index){
  BUYBACK_DATA_NETWORK$Valuation_Index = 
    ifelse(is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$recent_performance_score), NA, (1-BUYBACK_DATA_NETWORK$DATASET$CRSP$recent_performance_score)) +
    ifelse(is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap_score), NA, (1-BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap_score)) + 
    ifelse(is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME_score), NA, BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME_score)
} else {
  ###############################################################
  # Make the U-Index scores
  BUYBACK_DATA_NETWORK$Performance_used <- ifelse(is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$recent_performance_score), NA, ceiling(5*(1-BUYBACK_DATA_NETWORK$DATASET$CRSP$recent_performance_score)))
  BUYBACK_DATA_NETWORK$Performance_used[BUYBACK_DATA_NETWORK$Performance_used==0] <- 1
  BUYBACK_DATA_NETWORK$Size_used <- ifelse(is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap_score), NA, ceiling(5*(1-BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap_score)))
  BUYBACK_DATA_NETWORK$Size_used[BUYBACK_DATA_NETWORK$Size_used==0] <- 1
  # USE OUR BE.ME, NOT THE FF
  BUYBACK_DATA_NETWORK$BEME_used <- ifelse(is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME_score), NA, ceiling(5*BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME_score))
  BUYBACK_DATA_NETWORK$BEME_used[BUYBACK_DATA_NETWORK$BEME_used==0] <- 1
  BUYBACK_DATA_NETWORK$Valuation_Index = BUYBACK_DATA_NETWORK$Performance_used + BUYBACK_DATA_NETWORK$Size_used + BUYBACK_DATA_NETWORK$BEME_used 
}
############################################################################################################
# All the data filters are done in here
source("filter_bbnetwork_data.R")

############################################################################################################
centrality_used = BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores[[centality_used_name]]
to_remove_centrality_missing  = which(is.na(centrality_used))
if (length(to_remove_centrality_missing) > 0){
  centrality_used <- centrality_used[-to_remove_centrality_missing]
  # just in alphabetic order not to forget any    
  BUYBACK_DATA_NETWORK$BEME_used <- BUYBACK_DATA_NETWORK$BEME_used[-to_remove_centrality_missing]
  BUYBACK_DATA_NETWORK$Performance_used <- BUYBACK_DATA_NETWORK$Performance_used[-to_remove_centrality_missing]
  BUYBACK_DATA_NETWORK$Size_used <- BUYBACK_DATA_NETWORK$Size_used[-to_remove_centrality_missing]
  BUYBACK_DATA_NETWORK$Valuation_Index <- BUYBACK_DATA_NETWORK$Valuation_Index[-to_remove_centrality_missing]
  
  BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly <- BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,-to_remove_centrality_missing]
  BUYBACK_DATA_NETWORK$DATASET$SDC <- BUYBACK_DATA_NETWORK$DATASET$SDC[-to_remove_centrality_missing,]
  for(field in ls(BUYBACK_DATA_NETWORK$DATASET$CRSP))  BUYBACK_DATA_NETWORK$DATASET$CRSP[[field]] <- BUYBACK_DATA_NETWORK$DATASET$CRSP[[field]][-to_remove_centrality_missing]
  for(field in ls(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network))  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network[[field]] <- BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network[[field]][-to_remove_centrality_missing]
  for(field in ls(BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores))  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores[[field]] <- BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores[[field]][-to_remove_centrality_missing]
  for(field in ls(BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts))  BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts[[field]] <- BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts[[field]][-to_remove_centrality_missing]
  for(field1 in ls(BUYBACK_DATA_NETWORK$DATASET$ibes))  
    for (field in ls(BUYBACK_DATA_NETWORK$DATASET$ibes[[field1]])) 
      BUYBACK_DATA_NETWORK$DATASET$ibes[[field1]][[field]]<- BUYBACK_DATA_NETWORK$DATASET$ibes[[field1]][[field]][-to_remove_centrality_missing]
  rm("field","field1")
}
BUYBACK_DATA_NETWORK$cleanupMoreCentralityMissing = length(to_remove_centrality_missing)

####################################################################################
# These are one by one the tables in the .Rnw. 
####################################################################################

Risk_Factors_Monthly = BUYBACK_DATA_NETWORK$Risk_Factors_Monthly
Market_Monthly = BUYBACK_DATA_NETWORK$Market_Monthly

if (do.value.weight == 1){   
  value.weights = BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap
} else {
  value.weights = rep(1,length(BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap)) # This is the first version of the paper - no value weighted calendar method
}

############################################################################
# Define the recessions, bear/bull, high/low vol markets as in table 7 of http://www.econ.yale.edu/~shiller/behfin/2013_04-10/asness-frazzini-pedersen.pdf
# Use http://www.nber.org/cycles.html for NBER recessions

BUYBACK_DATA_NETWORK$NBER_Recession_YEARS_flag = Reduce("+", lapply(1:length(BEAR_YEARS), function(i){
  (BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date > BEAR_YEARS[[i]][1] & BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date < BEAR_YEARS[[i]][2]) 
}))

###
market_cumulative = 100*shift(ms(BUYBACK_DATA_NETWORK$Market_Monthly,12),1)
names(market_cumulative) <- names(BUYBACK_DATA_NETWORK$Market_Monthly)
Severe_Bear = names(market_cumulative)[market_cumulative < -25]
Severe_Bull = names(market_cumulative)[market_cumulative > 25]
BUYBACK_DATA_NETWORK$Severe_Bear= str_sub(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date,start=1,end=7) %in% str_sub(Severe_Bear,start=1,end=7)
BUYBACK_DATA_NETWORK$Severe_Bull= str_sub(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date,start=1,end=7) %in% str_sub(Severe_Bull,start=1,end=7)

###
daily_market_vol = as.vector(rolling_variance(matrix(BUYBACK_DATA_NETWORK$Market_daily,ncol=1),20))
names(daily_market_vol) <- names(BUYBACK_DATA_NETWORK$Market_daily)
monthly_vol = sapply(unique(str_sub(names(daily_market_vol), start=1,end=7)), function(i){
  mean(daily_market_vol[str_sub(names(daily_market_vol), start=1,end=7) == i])
})
Vol_high =names(monthly_vol)[which(monthly_vol > quantile(monthly_vol[monthly_vol!=0],0.7))]
Vol_low =names(monthly_vol)[which(monthly_vol < quantile(monthly_vol[monthly_vol!=0],0.3))]
BUYBACK_DATA_NETWORK$Vol_high= str_sub(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date,start=1,end=7) %in% Vol_high
BUYBACK_DATA_NETWORK$Vol_low= str_sub(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date,start=1,end=7) %in% Vol_low


############################################################################

company_subset_undervalued_bb = BUYBACK_DATA_NETWORK$Valuation_Index > 10
company_subset_overvalued_bb = BUYBACK_DATA_NETWORK$Valuation_Index < 6
High_Idiosyncr_eventsBB = BUYBACK_DATA_NETWORK$DATASET$CRSP$Rsq_score < quantile(BUYBACK_DATA_NETWORK$DATASET$CRSP$Rsq_score, quantile_R2)
Low_Idiosyncr_eventsBB  = BUYBACK_DATA_NETWORK$DATASET$CRSP$Rsq_score > quantile(BUYBACK_DATA_NETWORK$DATASET$CRSP$Rsq_score, 1-quantile_R2)
High_IVOL_eventsBB = BUYBACK_DATA_NETWORK$DATASET$CRSP$IVOL_score > quantile(BUYBACK_DATA_NETWORK$DATASET$CRSP$IVOL_score, 1-quantile_VOL)
Low_IVOL_eventsBB  = BUYBACK_DATA_NETWORK$DATASET$CRSP$IVOL_score < quantile(BUYBACK_DATA_NETWORK$DATASET$CRSP$IVOL_score, quantile_VOL)
High_VOL_eventsBB = BUYBACK_DATA_NETWORK$DATASET$CRSP$pre_vol_Score > quantile(BUYBACK_DATA_NETWORK$DATASET$CRSP$pre_vol_Score, 1-quantile_VOL)
Low_VOL_eventsBB  = BUYBACK_DATA_NETWORK$DATASET$CRSP$pre_vol_Score < quantile(BUYBACK_DATA_NETWORK$DATASET$CRSP$pre_vol_Score, quantile_VOL)

BUYBACK_DATA_NETWORK$EU_index = sapply(1:length(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date), function(i){
  ifelse(High_Idiosyncr_eventsBB[i], 2, ifelse(Low_Idiosyncr_eventsBB[i], 0, 1)) +
    ifelse(High_VOL_eventsBB[i], 2, ifelse(Low_VOL_eventsBB[i], 0, 1)) +
    ifelse(company_subset_undervalued_bb[i], 2, ifelse(company_subset_overvalued_bb[i], 0, 1))
})


#Recommendation score: 1. Strong Buy, 2. Buy, 3. Hold, 4. Underperform, 5. Sell
downgraded_events = !is.na(BUYBACK_DATA_NETWORK$DATASET$ibes$month_minus1$mean_rec) & !is.na(BUYBACK_DATA_NETWORK$DATASET$ibes$month_minus2$mean_rec) & scrub(BUYBACK_DATA_NETWORK$DATASET$ibes$month_minus1$mean_rec) > scrub(BUYBACK_DATA_NETWORK$DATASET$ibes$month_minus2$mean_rec)
not_downgraded_events = !is.na(BUYBACK_DATA_NETWORK$DATASET$ibes$month_minus1$mean_rec) & !is.na(BUYBACK_DATA_NETWORK$DATASET$ibes$month_minus2$mean_rec) & scrub(BUYBACK_DATA_NETWORK$DATASET$ibes$month_minus1$mean_rec) <= scrub(BUYBACK_DATA_NETWORK$DATASET$ibes$month_minus2$mean_rec)
upgraded_events = !is.na(BUYBACK_DATA_NETWORK$DATASET$ibes$month_minus1$mean_rec) & !is.na(BUYBACK_DATA_NETWORK$DATASET$ibes$month_minus2$mean_rec) & scrub(BUYBACK_DATA_NETWORK$DATASET$ibes$month_minus1$mean_rec) < scrub(BUYBACK_DATA_NETWORK$DATASET$ibes$month_minus2$mean_rec)
post_RegFD = (BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date >= "2003-01-01")
crisis_0709 = (BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date >= "2007-07-01" & BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date < "2010-01-01")

market_cap = BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap
Firm_size = BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap_score
Prior_R = BUYBACK_DATA_NETWORK$DATASET$CRSP$recent_performance_score
BEME = BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME_score
U_index = BUYBACK_DATA_NETWORK$Valuation_Index
EU_index = BUYBACK_DATA_NETWORK$EU_index
Vol_raw = BUYBACK_DATA_NETWORK$DATASET$CRSP$pre_vol_Score
Idiosyncratic = BUYBACK_DATA_NETWORK$DATASET$CRSP$IVOL_score
One_m_Rsqr = 1-BUYBACK_DATA_NETWORK$DATASET$CRSP$Rsq_score
market_beta = BUYBACK_DATA_NETWORK$DATASET$CRSP$market_beta_score
Analyst_disagreement = (BUYBACK_DATA_NETWORK$DATASET$ibes$month_minus1$analyst_disagreement_score)
Analyst_coverage = (BUYBACK_DATA_NETWORK$DATASET$ibes$month_minus1$analyst_coverage_score)
Eigenvector = BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores$supplier_eigenvector2_score 
Degree = BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores$supplier_degree1_score

NdirectPercent_score = BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts$NdirectPercent_score
Ndirect_followPercent_score = BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts$Ndirect_followPercent_score
Ngeneral_over_Nanalyst_score = BUYBACK_DATA_NETWORK$DATASET$ibes_supplyanalysts$Ngeneral_over_Nanalyst_score


##Centrality & Subset to be used -- TO PLAY WITH !! 
centrality_quantile_used = 0.2 # Define it here for the top and bottom thresholds
cquantile1 = 0.8 
cquantile2 = 0.6 
cquantile3 = 0.4 
cquantile4 = 0.2 

########################################################################################################
# a helper function
centrality_sort_function <- function(subset_used, factor_model, name_added = "", quantile_used = QUANTILE_COMPARED){
  useonly = which(subset_used)
  total = car_table(BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used = factor_model)
  useonly = which(centrality_used > quantile(centrality_used,(cquantile1)) & subset_used )
  All_top1 = car_table(BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used = factor_model)
  useonly = which(centrality_used > quantile(centrality_used,(cquantile2)) & centrality_used <= quantile(centrality_used,(cquantile1)) & subset_used )
  All_top2 = car_table(BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used = factor_model)
  useonly = which(centrality_used > quantile(centrality_used,(cquantile3)) & centrality_used <= quantile(centrality_used,(cquantile2)) & subset_used )
  All_top3 = car_table(BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used = factor_model)
  useonly = which(centrality_used > quantile(centrality_used,(cquantile4)) & centrality_used <= quantile(centrality_used,(cquantile3)) & subset_used )
  All_top4 = car_table(BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used = factor_model)
  useonly = which(centrality_used <= quantile(centrality_used,(cquantile4)) & subset_used )
  All_bottom = car_table(BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used = factor_model)
  irats = cbind(total$results[reported_times,],All_bottom$results[reported_times,],All_top4$results[reported_times,],All_top3$results[reported_times,],All_top2$results[reported_times,],All_top1$results[reported_times,])
  colnames(irats)[1] <- name_added
  colnames(irats)[4] <- "Q1 (Low) CAR"
  colnames(irats)[7] <- "Q2 CAR"
  colnames(irats)[10] <- "Q3 CAR"
  colnames(irats)[13] <- "Q4 CAR"
  colnames(irats)[16] <- "Q5 (High) CAR"
  
  if (quantile_used == 4){ to_compare = All_top2; to_compare_name = "Q4" }
  if (quantile_used == 3){ to_compare = All_top3; to_compare_name = "Q3" }
  if (quantile_used == 2){ to_compare = All_top4; to_compare_name = "Q2" }
  s1 = All_bottom$results[,1]/All_bottom$results[,2]
  s5 = All_top1$results[,1]/All_top1$results[,2]
  s3 = to_compare$results[,1]/to_compare$results[,2]
  tval13 = (All_bottom$results[,1] - to_compare$results[,1])/sqrt(s1*s1 + s3*s3)
  pval13 = 1-pt(tval13, df = pmin(All_bottom$dfs-1, to_compare$dfs -1))
  Q1Q3 = cbind(irats[,"Q1 (Low) CAR"] - irats[, paste(to_compare_name,"CAR",sep=" ")], tval13[reported_times],pval13[reported_times])
  colnames(Q1Q3) <- c(paste("Q1-",to_compare_name,sep=""),"t-stat","p-value") 
  Q1Q3[nrow(Q1Q3),] <- 0
  
  tval53 = (All_top1$results[,1] - to_compare$results[,1])/sqrt(s5*s5 + s3*s3)
  pval53 = 1-pt(tval53, df = pmin(All_top1$dfs-1, to_compare$dfs -1))
  Q5Q3 = cbind(irats[,"Q5 (High) CAR"] - irats[, paste(to_compare_name,"CAR",sep=" ")], tval53[reported_times],pval53[reported_times])
  colnames(Q5Q3) <- c(paste("Q5-",to_compare_name,sep=""),"t-stat","p-value") 
  Q5Q3[nrow(Q5Q3),] <- 0
  irats = cbind(irats, Q1Q3,Q5Q3)
  ###
  useonly = which(subset_used)
  total = calendar_table(BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used = factor_model,value.weights=value.weights[useonly])
  useonly = which(centrality_used > quantile(centrality_used,(cquantile1)) & subset_used )
  All_top1 = calendar_table(BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used = factor_model,value.weights=value.weights[useonly])
  useonly = which(centrality_used > quantile(centrality_used,(cquantile2)) & centrality_used <= quantile(centrality_used,(cquantile1)) & subset_used )
  All_top2 = calendar_table(BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used = factor_model,value.weights=value.weights[useonly])
  useonly = which(centrality_used > quantile(centrality_used,(cquantile3)) & centrality_used <= quantile(centrality_used,(cquantile2)) & subset_used )
  All_top3 = calendar_table(BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used = factor_model,value.weights=value.weights[useonly]) 
  useonly = which(centrality_used > quantile(centrality_used,(cquantile4)) & centrality_used <= quantile(centrality_used,(cquantile3)) & subset_used )
  All_top4 = calendar_table(BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used = factor_model,value.weights=value.weights[useonly])
  useonly = which(centrality_used <= quantile(centrality_used,(cquantile4)) & subset_used )
  All_bottom = calendar_table(BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used = factor_model,value.weights=value.weights[useonly])
  calendar = round(cbind(total$results[reported_times,],All_bottom$results[reported_times,],All_top4$results[reported_times,],All_top3$results[reported_times,],All_top2$results[reported_times,],All_top1$results[reported_times,]),2)
  colnames(calendar)[1] <- name_added
  colnames(calendar)[4] <- "Q1 (Low) CAL"
  colnames(calendar)[7] <- "Q2 CAL"
  colnames(calendar)[10] <- "Q3 CAL"
  colnames(calendar)[13] <- "Q4 CAL"
  colnames(calendar)[16] <- "Q5 (High) CAL"
  
  if (quantile_used == 4){ to_compare = All_top2; to_compare_name = "Q4" }
  if (quantile_used == 3){ to_compare = All_top3; to_compare_name = "Q3" }
  if (quantile_used == 2){ to_compare = All_top4; to_compare_name = "Q2" }
  s1 = All_bottom$results[,1]/All_bottom$results[,2]
  s5 = All_top1$results[,1]/All_top1$results[,2]
  s3 = to_compare$results[,1]/to_compare$results[,2]
  tval13 = (All_bottom$results[,1] - to_compare$results[,1])/sqrt(s1*s1 + s3*s3)
  pval13 = 1-pt(tval13, df = pmin(All_bottom$dfs-1, to_compare$dfs -1))
  Q1Q3 = cbind(calendar[,"Q1 (Low) CAL"] - calendar[,paste(to_compare_name,"CAL", sep=" ")], tval13[reported_times],pval13[reported_times])
  colnames(Q1Q3) <- c(paste("Q1-",to_compare_name,sep=""),"t-stat","p-value")
  Q1Q3[nrow(Q1Q3),] <- 0
  tval53 = (All_top1$results[,1] - to_compare$results[,1])/sqrt(s5*s5 + s3*s3)
  pval53 = 1-pt(tval53, df = pmin(All_top1$dfs-1, to_compare$dfs -1))
  Q5Q3 = cbind(calendar[,"Q5 (High) CAL"] - calendar[,paste(to_compare_name,"CAL", sep=" ")], tval53[reported_times],pval53[reported_times])
  colnames(Q5Q3) <- c(paste("Q5-",to_compare_name,sep=""),"t-stat","p-value") 
  Q5Q3[nrow(Q5Q3),] <- 0
  calendar = cbind(calendar, Q1Q3,Q5Q3)
  
  list(irats = irats, calendar = calendar)
}

###############################################################################################
###############################################################################################
# Generate the tables of the appendix
###############################################################################################
###############################################################################################


datasummaryBB = rbind(
  round(c(summary(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Size[!is.na(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Size) & BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Size!=0])[c(1,3,4,6)], sd(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Size[!is.na(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Size) & BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Size!=0]),sum(is.na(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Size) | BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Size==0)),1),
  round(c(summary(BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap[!is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap) & BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap!=0])[c(1,3,4,6)], sd(BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap[!is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap) & BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap!=0]),sum(is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap) | BUYBACK_DATA_NETWORK$DATASET$CRSP$Market.Cap==0)),1),
  round(c(summary(BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME[BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME < 1e20 & !is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME) & BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME!=0])[c(1,3,4,6)], sd(BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME[BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME < 1e20 & !is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME) & BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME!=0]),sum(is.na(BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME >= 1e20) | BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME ==0 | BUYBACK_DATA_NETWORK$DATASET$CRSP$BE.ME==0)),1))
rownames(datasummaryBB) <- c("Percent authorized", "Market cap.", "BE/ME")
colnames(datasummaryBB)[ncol(datasummaryBB)-1] <- "std"
colnames(datasummaryBB)[ncol(datasummaryBB)] <- "Missing"


# Table 1b: Industry characteristics

Event_Industries = BUYBACK_DATA_NETWORK$DATASET$SDC$Industry
Event_Industries<- sapply(Event_Industries, function(i){
  x=as.numeric(i)
  tmp = sapply(1:length(FF_industries), function(j) x %in% FF_industries[[j]])
  ifelse(sum(tmp!=0), names(FF_industries)[which(tmp!=0)], "Strange")
})
industry_tableBB = sort(table(Event_Industries), decreasing = TRUE)
industry_tableBB=industry_tableBB[-which(names(industry_tableBB)=="Strange")]
names(industry_tableBB)<- gsub("_"," ",names(industry_tableBB))

company_features_all = cbind(
  market_cap,Firm_size,Prior_R, BEME, 
  Vol_raw,One_m_Rsqr,market_beta, Analyst_coverage,Analyst_disagreement, U_index, EU_index,Eigenvector,  Degree)
colnames(company_features_all) <- c("Market Cap ($)", "Size Score","Prior Returns Scoree","BE/ME Score","Volatility Score","1-R2 Score","Beta Score","Analyst Coverage","Analyst Disagreement","U Index","EU Index",
                                    "Eigenvector","Degree")

non_zero_median <- function(x,n=1)ifelse(sum(scrub(x)!=0)<n,0,median(scrub(x)[scrub(x)!=0]))

industry_BB_features <- Reduce(rbind,lapply(which(industry_tableBB > 100), function(i){
  this_industry = which(Event_Industries == gsub(" ","_", names(industry_tableBB)[i]))
  if (length(this_industry) != industry_tableBB[i])
    stop("\nFunny industry problem\n")
  res = matrix(apply(company_features_all[this_industry,],2,non_zero_mean), nrow=1)
  colnames(res) <- colnames(company_features_all)
  rownames(res)<-names(industry_tableBB)[i]
  res
}))

###############################################################################################
#Table 2: IRATS: three- and five-factor, all events. (+ difference significane: Q1 vs. Q3 vs. Q5) 
#Table 3: Calendar Equal-weight: three- and five-factor, all events. (+ difference significane: Q1 vs. Q3 vs. Q5)

# 3 Factor Model, All
subset_used =  rep(T,length(BUYBACK_DATA_NETWORK$Valuation_Index))
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML", name_added="All 3F")
irats_all_3f = tmp$irats
calendar_all_3f = tmp$calendar
rm("subset_used","tmp")
# 5 factor, All
subset_used =  rep(T,length(BUYBACK_DATA_NETWORK$Size_used))
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_all = tmp$irats
calendar_all = tmp$calendar
rm("subset_used","tmp")

###############################################################################################
# Table 4: Relation between Firm Characteristics and Centrality.  
# -	Columns = firm characteristics score: eigen2, degree1; ME, analyst coverage, (1-R2), volatility, ivol, prior returns, BE/ME, U, EU. 
# -	Rows: 1st row is mean score of all Buyback Events (2nd row is t-stat of mean score of buyback events == 0.5); + next five rows: mean score by centrality group (Q1-Q5). 

BUYBACK_DATA_NETWORK$centrality_1to5 = sapply(centrality_used, function(i)
  ifelse(i <= quantile(centrality_used,0.2), 1, ifelse(i <= quantile(centrality_used,0.4), 2, ifelse(i <= quantile(centrality_used,0.6), 3, ifelse(i <= quantile(centrality_used,0.8), 4,5)))))

centrality_var_relations = apply(cbind(Eigenvector,Degree,Analyst_coverage, NdirectPercent_score,Ndirect_followPercent_score,Ngeneral_over_Nanalyst_score,One_m_Rsqr,Vol_raw, Idiosyncratic, Firm_size, Prior_R,  BEME,market_beta, U_index,EU_index), 2, function(r){
  sapply(-1:7, function(j) {
    ifelse(j==-1, mean(r[!is.na(r)]), 
           ifelse(j==0, t.test(r[!is.na(r)], mu=0.5)$p.value, 
                  ifelse(j==6, t.test(r[BUYBACK_DATA_NETWORK$centrality_1to5 == 1 & !is.na(r)], r[BUYBACK_DATA_NETWORK$centrality_1to5 == 4 & !is.na(r)])$p.value, 
                         ifelse(j==7, t.test(r[BUYBACK_DATA_NETWORK$centrality_1to5 == 5 & !is.na(r)], r[BUYBACK_DATA_NETWORK$centrality_1to5 == 4 & !is.na(r)])$p.value, 
                                mean(r[BUYBACK_DATA_NETWORK$centrality_1to5 == j & !is.na(r)])))))
  })
})
colnames(centrality_var_relations) <- c("Eigenvector","Degree","Analyst-Cov.","NdirectNanalyst","NdirectFolNanalyst","NgeneralNanalyst","(1-R2)","Volatility", "Ivol", "Size", "Prior-Returns","BE/ME", "Market-Beta", "U-index","EU-index" )
rownames(centrality_var_relations) <- c("All", "p-value diff. 0.5",paste("Centrality",1:5, sep=": "), "Q1-Q4 p-value", "Q5-Q4 p-value")

###############################################################################################
# Table 5.1 ??? 5.6: Five-factor IRATS: 2-sort: (+ difference significane: Q1 vs. Q3 vs. Q5)
# 1)	centrality x (1-R2), 
# 2)	size, 
# 3)	analyst coverage, 
# 4)	vol, 
# 5)	U, 
# 6)	EU. 

###################
# 5 factor, Low (1-R2) 
subset_used = (One_m_Rsqr< quantile(One_m_Rsqr,0.5))
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_lowMr2 = tmp$irats
calendar_lowMr2 = tmp$calendar
rm("subset_used","tmp")
# 5 factor, High (1-R2) 
subset_used = (One_m_Rsqr>= quantile(One_m_Rsqr,0.5))
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_highMr2 = tmp$irats
calendar_highMr2 = tmp$calendar
rm("subset_used","tmp")

###################
# 5 factor, Small size>3  
subset_used = (Firm_size <= median(Firm_size))
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_small = tmp$irats
calendar_small = tmp$calendar
rm("subset_used","tmp")
# 5 factor, Large size<=3 
subset_used = (Firm_size > median(Firm_size))
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_large = tmp$irats
calendar_large = tmp$calendar
rm("subset_used","tmp")

###################
# 5 factor, Low analyst coverage  -- include missing(Analyst_coverage) or not??
Analyst_coverage_ini = Analyst_coverage

available_analyst_coverage = !is.na(Analyst_coverage)
Analyst_coverage = scrub(Analyst_coverage)
subset_used = (Analyst_coverage < median(Analyst_coverage)) & available_analyst_coverage 
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_Lcover = tmp$irats
calendar_Lcover = tmp$calendar
rm("subset_used","tmp")
# 5 factor, High analyst coverage
subset_used = (Analyst_coverage >= median(Analyst_coverage)) & available_analyst_coverage
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_Hcover = tmp$irats
calendar_Hcover = tmp$calendar
rm("subset_used","tmp")

Analyst_coverage = Analyst_coverage_ini

###################
# 5 factor, Low analyst disagreement  -- include missing(Analyst_coverage) or not??
Analyst_disagreement_ini = Analyst_disagreement

available_Analyst_disagreement = !is.na(Analyst_disagreement)
Analyst_disagreement = scrub(Analyst_disagreement)
subset_used = (Analyst_disagreement < median(Analyst_disagreement)) & available_Analyst_disagreement 
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_Ldisagree = tmp$irats
calendar_Ldisagree = tmp$calendar
rm("subset_used","tmp")
# 5 factor, High analyst coverage
subset_used = (Analyst_disagreement >= median(Analyst_disagreement)) & available_Analyst_disagreement
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_Hdisagree = tmp$irats
calendar_Hdisagree = tmp$calendar
rm("subset_used","tmp")

Analyst_disagreement = Analyst_disagreement_ini

###################
# 5 factor, High vol 
subset_used = (Vol_raw > median(Vol_raw))
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_Hvol = tmp$irats
calendar_Hvol = tmp$calendar
rm("subset_used","tmp")
# 5 factor, Low vol 
subset_used = (Vol_raw <= median(Vol_raw))
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_Lvol = tmp$irats
calendar_Lvol = tmp$calendar
rm("subset_used","tmp")

###################
# 5 factor, High U
subset_used = company_subset_undervalued_bb
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_Huindex = tmp$irats
calendar_Huindex = tmp$calendar
rm("subset_used","tmp")
# 5 factor, Low U
subset_used = company_subset_overvalued_bb
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_Luindex = tmp$irats
calendar_Luindex = tmp$calendar
rm("subset_used","tmp")

###################
# 5 factor, High EU: EU> 3 -> gives similar obs. in two groups. 
subset_used = (EU_index > 3)
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_Heu = tmp$irats
calendar_Heu = tmp$calendar
rm("subset_used","tmp")
# 5 factor, Low EU
subset_used = (EU_index < 2)
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_Leu = tmp$irats
calendar_Leu = tmp$calendar
rm("subset_used","tmp")



###################
# 5 factor, Low supply chain specialists
NdirectPercent_score_ini = NdirectPercent_score

available_NdirectPercent_score = !is.na(NdirectPercent_score)
NdirectPercent_score = scrub(NdirectPercent_score)
subset_used = (NdirectPercent_score <= quantile(NdirectPercent_score[available_NdirectPercent_score],supplier_quantile)) & available_NdirectPercent_score 
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_lowNdirectPercent = tmp$irats
calendar_lowNdirectPercent = tmp$calendar
rm("subset_used","tmp")
# 5 factor, High (1-R2) 
subset_used = (NdirectPercent_score > quantile(NdirectPercent_score[available_NdirectPercent_score],supplier_quantile)) & available_NdirectPercent_score 
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_highNdirectPercent = tmp$irats
calendar_highNdirectPercent = tmp$calendar
rm("subset_used","tmp")

NdirectPercent_score = NdirectPercent_score_ini

###################
# 5 factor, Low supply chain specialists
Ndirect_followPercent_score_ini = Ndirect_followPercent_score

available_Ndirect_followPercent_score = !is.na(Ndirect_followPercent_score)
Ndirect_followPercent_score = scrub(Ndirect_followPercent_score)
subset_used = (Ndirect_followPercent_score <= quantile(Ndirect_followPercent_score[available_Ndirect_followPercent_score],supplier_quantile)) & available_Ndirect_followPercent_score 
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_lowNdirect_followPercent = tmp$irats
calendar_lowNdirect_followPercent = tmp$calendar
rm("subset_used","tmp")
# 5 factor, High (1-R2) 
subset_used = (Ndirect_followPercent_score > quantile(Ndirect_followPercent_score[available_Ndirect_followPercent_score],supplier_quantile)) & available_Ndirect_followPercent_score 
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_highNdirect_followPercent = tmp$irats
calendar_highNdirect_followPercent = tmp$calendar
rm("subset_used","tmp")

Ndirect_followPercent_score = Ndirect_followPercent_score_ini


###################
# 5 factor, Low supply chain specialists
Ngeneral_over_Nanalyst_score_ini = Ngeneral_over_Nanalyst_score

available_Ngeneral_over_Nanalyst_score = !is.na(Ngeneral_over_Nanalyst_score) & scrub(Ngeneral_over_Nanalyst_score) !=1
Ngeneral_over_Nanalyst_score = scrub(Ngeneral_over_Nanalyst_score)
subset_used = (Ngeneral_over_Nanalyst_score <= quantile(Ngeneral_over_Nanalyst_score[available_Ngeneral_over_Nanalyst_score],supplier_quantile)) & available_Ngeneral_over_Nanalyst_score 
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_lowNgeneral_over_Nanalyst = tmp$irats
calendar_lowNgeneral_over_Nanalyst = tmp$calendar
rm("subset_used","tmp")
# 5 factor, High (1-R2) 
subset_used = (Ngeneral_over_Nanalyst_score > quantile(Ngeneral_over_Nanalyst_score[available_Ngeneral_over_Nanalyst_score],supplier_quantile)) & available_Ngeneral_over_Nanalyst_score 
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="All")
irats_highNgeneral_over_Nanalyst = tmp$irats
calendar_highNgeneral_over_Nanalyst = tmp$calendar
rm("subset_used","tmp")

Ngeneral_over_Nanalyst_score = Ngeneral_over_Nanalyst_score_ini

########################################################################################################
# Super-EU index

CEU_centrality_score = ifelse(BUYBACK_DATA_NETWORK$centrality_1to5 %in% c(1,5), 2, ifelse(BUYBACK_DATA_NETWORK$centrality_1to5 == QUANTILE_COMPARED, 0, 1))
BUYBACK_DATA_NETWORK$CEU_index = BUYBACK_DATA_NETWORK$EU_index + CEU_centrality_score

#BUYBACK_DATA_NETWORK$CEU_index_2 = ifelse(BUYBACK_DATA_NETWORK$CEU_index %in% 0:2, 2, 
#                                          ifelse(BUYBACK_DATA_NETWORK$CEU_index %in% 7:8, 7, BUYBACK_DATA_NETWORK$CEU_index))
BUYBACK_DATA_NETWORK$CEU_index_2 = BUYBACK_DATA_NETWORK$CEU_index
CEU_IRATStable_bb = NULL
CEU_CALtable_bb = NULL
for (i in sort(unique(BUYBACK_DATA_NETWORK$CEU_index_2))){
  CEU_events_now = which(BUYBACK_DATA_NETWORK$CEU_index_2 == i)
  CEU_IRATStable_bb = cbind(CEU_IRATStable_bb,car_table(BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,CEU_events_now], BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date[CEU_events_now], Risk_Factors_Monthly)$results)
  CEU_CALtable_bb = cbind(CEU_CALtable_bb,calendar_table(BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,CEU_events_now], BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date[CEU_events_now], Risk_Factors_Monthly,value.weights = value.weights[CEU_events_now])$results)
}
rm("i")


#Table 9: portfolio returns from SEU vs. EU. 

DatesMonth <- create_dates_month(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date, rownames(BUYBACK_DATA_NETWORK$Risk_Factors_Monthly)) # We don't need this any more, can simplify to only get the dates needed... it's ok for now, as it is now slow
colnames(DatesMonth) <- BUYBACK_DATA_NETWORK$DATASET$SDC$permno

# CEU first
used_events = which(BUYBACK_DATA_NETWORK$CEU_index <= LOW_CEU_THRESHOLD)
short_events_CEU_portfolio = PNL_matrix_BB(start_date_event,HOLDING_PERIOD, used_events,  DatesMonth, BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly,event=1)  
short_pnl_events_CEU_portfolio <- apply(short_events_CEU_portfolio,1,function(r) non_zero_mean(scrub(r)))
##
used_events = which(BUYBACK_DATA_NETWORK$CEU_index >= HIGH_CEU_THRESHOLD)
long_events_CEU_portfolio = PNL_matrix_BB(start_date_event,HOLDING_PERIOD, used_events,  DatesMonth, BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly,event=1)  
long_pnl_events_CEU_portfolio <- apply(long_events_CEU_portfolio,1,function(r) non_zero_mean(scrub(r)))
##
pnl_events_CEU_portfolio = 0.5*(long_pnl_events_CEU_portfolio - short_pnl_events_CEU_portfolio)
pnl_events_CEU_portfolio_hedged = suppressWarnings(scrub(alpha_lm(pnl_events_CEU_portfolio,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))

## EU now
used_events = which(BUYBACK_DATA_NETWORK$EU_index <= LOW_EU_THRESHOLD)
short_events_EU_portfolio = PNL_matrix_BB(start_date_event,HOLDING_PERIOD, used_events,  DatesMonth, BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly,event=1)  
short_pnl_events_EU_portfolio <- apply(short_events_EU_portfolio,1,function(r) non_zero_mean(scrub(r)))
##
used_events = which(BUYBACK_DATA_NETWORK$EU_index >= HIGH_EU_THRESHOLD)
long_events_EU_portfolio = PNL_matrix_BB(start_date_event,HOLDING_PERIOD, used_events,  DatesMonth, BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly,event=1)  
long_pnl_events_EU_portfolio <- apply(long_events_EU_portfolio,1,function(r) non_zero_mean(scrub(r)))
##
pnl_events_EU_portfolio = 0.5*(long_pnl_events_EU_portfolio - short_pnl_events_EU_portfolio)
pnl_events_EU_portfolio_hedged = suppressWarnings(scrub(alpha_lm(pnl_events_EU_portfolio,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))

### Different way
if (1){
  
  used_events = which(BUYBACK_DATA_NETWORK$CEU_index <= LOW_CEU_THRESHOLD | BUYBACK_DATA_NETWORK$CEU_index >= HIGH_CEU_THRESHOLD)
  long_short_events_returns = 0*BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly
  long_short_events_returns[,BUYBACK_DATA_NETWORK$CEU_index <= LOW_CEU_THRESHOLD] <- -BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,BUYBACK_DATA_NETWORK$CEU_index <= LOW_CEU_THRESHOLD]
  long_short_events_returns[,BUYBACK_DATA_NETWORK$CEU_index >= HIGH_CEU_THRESHOLD] <- BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,BUYBACK_DATA_NETWORK$CEU_index >= HIGH_CEU_THRESHOLD]
  events_CEU_portfolio = PNL_matrix_BB(start_date_event,HOLDING_PERIOD, used_events,  DatesMonth, long_short_events_returns,event=1)  
  pnl_events_CEU_portfolio <- apply(events_CEU_portfolio,1,function(r) non_zero_mean(scrub(r)))
  pnl_events_CEU_portfolio_hedged = suppressWarnings(scrub(alpha_lm(pnl_events_CEU_portfolio,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  
  used_events = which(BUYBACK_DATA_NETWORK$EU_index <= LOW_EU_THRESHOLD | BUYBACK_DATA_NETWORK$EU_index >= HIGH_EU_THRESHOLD)
  long_short_events_returns = 0*BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly
  long_short_events_returns[,BUYBACK_DATA_NETWORK$EU_index <= LOW_EU_THRESHOLD] <- -BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,BUYBACK_DATA_NETWORK$EU_index <= LOW_EU_THRESHOLD]
  long_short_events_returns[,BUYBACK_DATA_NETWORK$EU_index >= HIGH_EU_THRESHOLD] <- BUYBACK_DATA_NETWORK$DATASET$returns_by_event_monthly[,BUYBACK_DATA_NETWORK$EU_index >= HIGH_EU_THRESHOLD]
  events_EU_portfolio = PNL_matrix_BB(start_date_event,HOLDING_PERIOD, used_events,  DatesMonth, long_short_events_returns,event=1)  
  pnl_events_EU_portfolio <- apply(events_EU_portfolio,1,function(r) non_zero_mean(scrub(r)))
  pnl_events_EU_portfolio_hedged = suppressWarnings(scrub(alpha_lm(pnl_events_EU_portfolio,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  
}

pnl_events_CEU_portfolio = remove_initialization_time(pnl_events_CEU_portfolio)
pnl_events_CEU_portfolio_hedged = remove_initialization_time(pnl_events_CEU_portfolio_hedged)

pnl_events_EU_portfolio = remove_initialization_time(pnl_events_EU_portfolio)
pnl_events_EU_portfolio_hedged = remove_initialization_time(pnl_events_EU_portfolio_hedged)

rm("used_events")


#Figure 2: Distribution of SEU-index. 

# 5 factor, SEU
#subset_used = BUYBACK_DATA_NETWORK$CEU_index >= 6
#tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="Downgraded")
#irats_lowSEU = tmp$irats
#calendar_lowSEU = tmp$calendar
#rm("subset_used","tmp")


###################
# 5 factor, Recession 
subset_used =  BUYBACK_DATA_NETWORK$NBER_Recession_YEARS_flag == 1
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="Bear")
irats_bear = tmp$irats
calendar_bear = tmp$calendar
rm("subset_used","tmp")
# 5 factor, Not Recession
subset_used =  !BUYBACK_DATA_NETWORK$NBER_Recession_YEARS_flag 
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="Not Bear")
irats_notbear = tmp$irats
calendar_notbear = tmp$calendar
rm("subset_used","tmp")

###################
# 5 factor, High market vol but not sever market bull (we care about "bad volatility") OR severe bear
subset_used =  (BUYBACK_DATA_NETWORK$Vol_high & !BUYBACK_DATA_NETWORK$Severe_Bull) | BUYBACK_DATA_NETWORK$Severe_Bear
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="High Market Vol")
irats_tensemarket = tmp$irats
calendar_tensemarket = tmp$calendar
rm("subset_used","tmp")
# 5 factor, Low market vol but not sever market bear (we care about "good volatility") OR severe bull
subset_used =  (BUYBACK_DATA_NETWORK$Vol_low & !BUYBACK_DATA_NETWORK$Severe_Bear) | BUYBACK_DATA_NETWORK$Severe_Bull
tmp = centrality_sort_function(subset_used,factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA", name_added="Low Market Vol")
irats_quietmarket = tmp$irats
calendar_quietmarket = tmp$calendar
rm("subset_used","tmp")


########################################################################################################
# Cross sectional analyses
########################################################################################################

# Get the predicted post-announce abnormal returns (as developed by "../buybackissuers/bb_issuers_new.R")
load("../FinanceData/created_projects_datasets/BUYBACKSnew_BSC1998_event_study_factor_coeffs.Rdata")
events_used = paste(BUYBACK_DATA_NETWORK$DATASET$SDC$permno, BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date, sep=" ")
Estimated_returns = 100*Estimated_returns[events_used,]
BUYBACK_PreEvent_Factor_coeffs = BUYBACK_PreEvent_Factor_coeffs[events_used]
rm("events_used","BUYBACK_PreEvent_Factor_coeffs") # we don't need for now BUYBACK_PreEvent_Factor_coeffs

company_features_all = cbind(Firm_size,BEME,Prior_R,U_index,EU_index, Vol_raw,One_m_Rsqr,Analyst_coverage,centrality_used,centrality_used)
colnames(company_features_all) <- c("SizeScore","BEMEScore","PriorReturnsScore","UIndex","EUIndex","Volatility","OneMRsq","AnalystCoverage","CentralityLinear","Centrality")
nomissing_allowed = c("alphaT","PriorReturnsScore","UIndex","EUIndex","Volatility","OneMRsq","AnalystCoverage","Centrality")

year_dummies_cross = sapply(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date, function(i) str_sub(i, start=1,end=4))
company_features_all = as.data.frame(company_features_all)
company_features_all$year_dummies_cross = year_dummies_cross

########################################################################################################
#Table 6: Cross-section Regressions: 
#	Panel A (all events): 1-by-1 variable. 
#	Panel B (all events): control all. 

useonly = 1:length(BUYBACK_DATA_NETWORK$DATASET$SDC$CUSIP)

BSC1998_individual_regression = Reduce(rbind,lapply(setdiff(colnames(company_features_all),"year_dummies_cross"), function(i){
  i = c(i,"year_dummies_cross")
  BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,,drop=F],company_features_all[useonly,i,drop=F], timeperiods_requested = 1:48, square_features = "Centrality",nomissing_allowed)
  BSC1998_completemodel =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
  BSC1998_completemodel[2:nrow(BSC1998_completemodel),]
}))
rownames(BSC1998_individual_regression) <- c(setdiff(colnames(company_features_all),"year_dummies_cross"),"Centrality.Square")
BSC1998_individual_regression = rbind(BSC1998_individual_regression, c(rep(12,3), rep(24,3),rep(36,3),rep(48,3)))
rownames(BSC1998_individual_regression)[nrow(BSC1998_individual_regression)] <- "Observations"
rownames(BSC1998_individual_regression)[which(rownames(BSC1998_individual_regression) == "Centrality")]<-"Centrality (One regr.)"
rownames(BSC1998_individual_regression)[which(rownames(BSC1998_individual_regression) == "Centrality.Square")]<-"Centrality.Square (One regr.)"

company_features = company_features_all[,c("UIndex","Volatility","OneMRsq","AnalystCoverage","Centrality","year_dummies_cross")]
BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = c("Centrality"),nomissing_allowed)
BSC1998_completemodel =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
BSC1998_completemodel = rbind(BSC1998_completemodel, c(rep(12,3), rep(24,3),rep(36,3),rep(48,3)))
rownames(BSC1998_completemodel)[nrow(BSC1998_completemodel)] <- "Observations"

company_features = company_features_all[,c("SizeScore","BEMEScore","PriorReturnsScore","Volatility","OneMRsq","AnalystCoverage","Centrality","year_dummies_cross")]
BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = c("Centrality"),nomissing_allowed)
BSC1998_completemodel_decompose =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
BSC1998_completemodel_decompose = rbind(BSC1998_completemodel_decompose, c(rep(12,3), rep(24,3),rep(36,3),rep(48,3)))
rownames(BSC1998_completemodel_decompose)[nrow(BSC1998_completemodel_decompose)] <- "Observations"

########################################################################################################
#Table 7: Cross-section Regressions: control all vars: ??? coefficients comparison test
#	Panel A (high- vs. low-vol events). 
#	Panel B (downgraded vs. other events).

useonly = which(Vol_raw > median(Vol_raw))
company_features = company_features_all[,c("UIndex","Volatility","OneMRsq","AnalystCoverage","Centrality","year_dummies_cross")]
BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = c("Centrality"),nomissing_allowed)
BSC1998_completemodel_highvol =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
BSC1998_completemodel_highvol = rbind(BSC1998_completemodel_highvol, c(rep(12,3), rep(24,3),rep(36,3),rep(48,3)))
rownames(BSC1998_completemodel_highvol)[nrow(BSC1998_completemodel_highvol)] <- "Observations"

useonly = which(Vol_raw <= median(Vol_raw))
company_features = company_features_all[,c("UIndex","Volatility","OneMRsq","AnalystCoverage","Centrality","year_dummies_cross")]
BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = c("Centrality"),nomissing_allowed)
BSC1998_completemodel_lowvol =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
BSC1998_completemodel_lowvol = rbind(BSC1998_completemodel_lowvol, c(rep(12,3), rep(24,3),rep(36,3),rep(48,3)))
rownames(BSC1998_completemodel_lowvol)[nrow(BSC1998_completemodel_lowvol)] <- "Observations"

###

useonly = which(downgraded_events)
company_features = company_features_all[,c("UIndex","Volatility","OneMRsq","AnalystCoverage","Centrality","year_dummies_cross")]
BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = c("Centrality"),nomissing_allowed)
BSC1998_completemodel_downgraded =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
BSC1998_completemodel_downgraded = rbind(BSC1998_completemodel_downgraded, c(rep(12,3), rep(24,3),rep(36,3),rep(48,3)))
rownames(BSC1998_completemodel_downgraded)[nrow(BSC1998_completemodel_downgraded)] <- "Observations"

useonly = which(upgraded_events)
company_features = company_features_all[,c("UIndex","Volatility","OneMRsq","AnalystCoverage","Centrality","year_dummies_cross")]
BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = c("Centrality"),nomissing_allowed)
BSC1998_completemodel_upgraded =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
BSC1998_completemodel_upgraded = rbind(BSC1998_completemodel_upgraded, c(rep(12,3), rep(24,3),rep(36,3),rep(48,3)))
rownames(BSC1998_completemodel_upgraded)[nrow(BSC1998_completemodel_upgraded)] <- "Observations"

########################################################################################################
#Table 8: Cross-section Regressions: control all: ??? coefficients comparison test
#	Panel A ??? more supply-chain analysts; 
#	Panel B ??? fewer supply-chain analysts. 

Analyst_coverage  = Analyst_coverage_ini

available_analyst_coverage = !is.na(Analyst_coverage)
Analyst_coverage = scrub(Analyst_coverage)

useonly = (Analyst_coverage < median(Analyst_coverage)) & available_analyst_coverage 
company_features = company_features_all[,c("UIndex","Volatility","OneMRsq","AnalystCoverage","Centrality","year_dummies_cross")]
BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = c("Centrality"),nomissing_allowed)
BSC1998_completemodel_highanalyst =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
BSC1998_completemodel_highanalyst = rbind(BSC1998_completemodel_highanalyst, c(rep(12,3), rep(24,3),rep(36,3),rep(48,3)))
rownames(BSC1998_completemodel_highanalyst)[nrow(BSC1998_completemodel_highanalyst)] <- "Observations"

useonly = (Analyst_coverage >= median(Analyst_coverage)) & available_analyst_coverage 
company_features = company_features_all[,c("UIndex","Volatility","OneMRsq","AnalystCoverage","Centrality","year_dummies_cross")]
BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = c("Centrality"),nomissing_allowed)
BSC1998_completemodel_lowanalyst =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
BSC1998_completemodel_lowanalyst = rbind(BSC1998_completemodel_lowanalyst, c(rep(12,3), rep(24,3),rep(36,3),rep(48,3)))
rownames(BSC1998_completemodel_lowanalyst)[nrow(BSC1998_completemodel_lowanalyst)] <- "Observations"

########################################################################################################
#Table 9: Cross-section Regressions: control all: ??? coefficients comparison test
#	Panel A ??? high EU; 
#	Panel B ??? low EU. 

useonly = (EU_index > 3)
company_features = company_features_all[,c("UIndex","Volatility","OneMRsq","AnalystCoverage","Centrality","year_dummies_cross")]
BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = c("Centrality"),nomissing_allowed)
BSC1998_completemodel_highEU =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
BSC1998_completemodel_highEU = rbind(BSC1998_completemodel_highEU, c(rep(12,3), rep(24,3),rep(36,3),rep(48,3)))
rownames(BSC1998_completemodel_highEU)[nrow(BSC1998_completemodel_highEU)] <- "Observations"

useonly = (EU_index < 2)
company_features = company_features_all[,c("UIndex","Volatility","OneMRsq","AnalystCoverage","Centrality","year_dummies_cross")]
BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = c("Centrality"),nomissing_allowed)
BSC1998_completemodel_lowEU =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
BSC1998_completemodel_lowEU = rbind(BSC1998_completemodel_lowEU, c(rep(12,3), rep(24,3),rep(36,3),rep(48,3)))
rownames(BSC1998_completemodel_lowEU)[nrow(BSC1998_completemodel_lowEU)] <- "Observations"

########################################################################################################
# Cross-sectional: robustness - use all centralities
all_centralities = cbind(
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores$supplier_degree1_score, 
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores$supplier_degree2_score, 
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores$supplier_eigenvector2_score,
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores$supplier_kbcntrl2_score,
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores$supplier_betweenness1_score,
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores$dgr1cust_subs_score,
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores$dgr1sc_subs_score,
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores$dgr1supp_subs_score,
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores$dgr2cust_subs_score,
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores$dgr2sc_subs_score,
  BUYBACK_DATA_NETWORK$DATASET$customer_supplier_network_scores$dgr2supp_subs_score
)
colnames(all_centralities) <- c("Degree", "Strength","Eigenvector", "KB", "Betweenness","dgr1custsubs","dgr1scsubs",
                                "dgr1suppsubs","dgr2custsubs","dgr2scsubs","dgr2suppsubs")

robust_results = list()

useonly = 1:length(BUYBACK_DATA_NETWORK$DATASET$SDC$CUSIP)
for (iter in 1:ncol(all_centralities)){
  company_features = company_features_all[,c("UIndex","Volatility","OneMRsq","AnalystCoverage","year_dummies_cross")]
  company_features$Centrality = all_centralities[,iter]
  BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = c("Centrality"),nomissing_allowed)
  BSC1998_completemodel_robust =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
  BSC1998_completemodel_robust = rbind(BSC1998_completemodel_robust, c(rep(12,3), rep(24,3),rep(36,3),rep(48,3)))
  rownames(BSC1998_completemodel_robust)[nrow(BSC1998_completemodel_robust)] <- "Observations"
  robust_results = c(robust_results,list(BSC1998_completemodel_robust))
}
names(robust_results) <- colnames(all_centralities)

rm("company_features","useonly","BSC1998_coefficients","iter","BSC1998_completemodel_robust")

# JUST ADD THE MEAN CENTRALITY OF ALL CRSP... used once in the paper...
load("../FinanceData/created_supplier_customer_networkdata/GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE.Rdata")
tmp = GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE[[centrality_name]]
tmp = tmp[as.Date(rownames(tmp)) >= min(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date) & as.Date(rownames(tmp)) <= max(BUYBACK_DATA_NETWORK$DATASET$SDC$Event.Date), ]
tmp <- get_cross_section_score(tmp)
meanCRSPdegree = mean(tmp[!is.na(tmp)])
rm("GLOBAL_SUPPLIER_CUSTOMER_NETWORK_DATABASE","tmp")

########################################################################################################
########################################################################################################
# Keep all results ONLY IN THIS FILE just in case we need to rerun or add something... this is temporary
save(list = setdiff(ls(all = TRUE),initial_vars), file = "bb_network.Rdata")

########################################################################################################
# Generate the report
library(knitr); knit2pdf('bb_network.Rnw', texi2dvi='texi2dvi'); file.remove(paste("bb_network",c(".aux",".tex",".log",".out"),sep=""))
file.rename("bb_network.pdf", paste("bb_network_", str_replace(centrality_name,"supplier_",""), ".pdf", sep=""))
#unlink("bb_network.Rdata")

