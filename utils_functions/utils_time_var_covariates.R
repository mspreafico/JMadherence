library(data.table)
library(tidyr)

# Auxiliary functions for 01_time_varying_covariates.R

cumulative_days<-function(data, anagraphic, therapy = c('ACE', 'ARB'), idVar = 'ID'){
  
  # Compute events' starting/ending dates and coverage days
  data[event_type=='hospitalization', start_date := hosp_adm_date]
  data[event_type=='hospitalization', quant := LOS]
  data[event_type=='hospitalization', end_date := hosp_adm_date + (LOS-1)]
  
  data[event_type=='drug purchase', start_date := purchase_date]
  data[event_type=='drug purchase', quant:= days_pharma]
  data[event_type=='drug purchase', end_date := purchase_date + (days_pharma-1)]
  data <- data[start_date >= index_date]
  
  if(dim(data[is.na(quant)])[1]>0){
    warning('Data with no quant values deleted.')
    data <- data[!is.na(quant)]
  }
  
  IDs <- c(unique(data[ATC_class %in% therapy, ..idVar]))
  temp_data <- NULL
  for (i in 1:length(IDs[[1]])){
    
    currentID <- IDs[[1]][i]
    
    # Set reference date and stop date
    ref_date <- data[get(idVar)==currentID, unique(index_date)]
    stop_date <-(ref_date+365)
    
    # Set vectors of events' starting/ending dates and coverage days
    start_vec <- data[get(idVar)==currentID & (ATC_class %in% therapy | event_type=='hospitalization'), start_date]
    end_vec <- data[get(idVar)==currentID & (ATC_class %in% therapy | event_type=='hospitalization'), end_date]
    quant_vec <- data[get(idVar)==currentID & (ATC_class %in% therapy |  event_type=='hospitalization'), quant]
    
    # Order events and exclude overlaps
    order_vec<-order(start_vec)
    start_vec <- start_vec[order_vec]
    end_vec <- end_vec[order_vec]
    quant_vec <- quant_vec[order_vec]
    start_vec_start_date<-start_vec
    end_vec_start_date<-end_vec
    if(length(start_vec)>1){
      for (j in 2:length(start_vec))
      {
        if (start_vec[j]<=end_vec[j-1])
        {
          start_vec[j] <- end_vec[j-1]+1
          if(start_vec[j]>end_vec[j]){
            end_vec[j]<-start_vec[j]-1
          }
        }
      }
    }
    
    for (h in 1:length(start_vec)){
      start_vec[h]<-min(start_vec[h],stop_date-1)
      end_vec[h]<-min(end_vec[h],stop_date-1)
      start_vec[h]<-max(start_vec[h],ref_date)
      end_vec[h]<-max(end_vec[h],ref_date)
      
    }
    
    # Compute covered days
    start_index<- difftime(start_vec, ref_date, units='days') 
    end_index<-difftime(end_vec, ref_date, units='days')
    
    day<-as.matrix(t(rep(0,365)))
    last<-0
    for(l in 1:length(start_vec)){
      inizio <- start_index[l]
      fine <- end_index[l]
      if(inizio<=365 & inizio<=fine){
        for(m in inizio:fine){
          day[m] <- 1
        }
      }
    } 
    
    # Compute cumulative coverage days
    day<-as.matrix(t(cumsum(day)))
    colnames(day)<-c(1:365)
    day<-gather(as.data.frame(day), t_days, cum_days, 1:365, factor_key=F)
    day<-as.data.table(day)
    day[, c(idVar) := currentID]
    temp_data<-rbind(temp_data,day)
    
    if(i%%100==0 | i==length(IDs[[1]]))
      print(paste('Iteration: patient',i,'of',length(IDs[[1]]),sep=' '))
    
  }
  
  temp_data<-merge(anagraphic[get(idVar) %in% IDs[[1]]], temp_data, by=idVar, all.y=T)
  temp_data[, t_days := as.numeric(t_days)]
  temp_data[, cum_days := as.numeric(cum_days)]
  temp_data<-temp_data[t_days<=fup*30.4375]
  
  return(temp_data)
  
}


cumulative_months<-function(cumulative_days){
  
  cumulative_days<-as.data.table(cumulative_days)
  # Select days of each month
  time<-c(1,30,60,91,121,152,182,213,243,273,304,334,365)
  temp_data<-as.data.table(cumulative_days[t_days %in% time])
  # Convert days into months
  temp_data[, time := t_days/30.4375]
  temp_data[, cum_months := cum_days/30.4375]
  # Delete day-variables
  temp_data[, t_days:=NULL]
  temp_data[, cum_days:=NULL]
  return(temp_data)
  
}

adherence<-function(cumulative_months, tau = 0.8){
  
  # Dichotomize cum_months according to threshold tau,
  # as explained in the manuscript
  temp_data<-as.data.table(cumulative_months)
  temp_data[, adherence := ifelse((cum_months/time)>=tau,1,0) ]
  return(temp_data)
  
}


