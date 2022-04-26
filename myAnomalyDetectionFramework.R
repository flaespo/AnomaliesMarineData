myAnomalyDetection <- function(time_series_values, Na_value, start, frequency,type,time_series_new_value,confidence_levels, num_predicted_values) {
  
  if ((frequency != 12) && (frequency != 52)){
    stop(" Error: Frequency value not allowed")
  }
  
  # Create time series objects
  time_series <- ts(time_series_values, start = start, frequency = frequency)
  
  # Na_value is a missing value: replace them with NA
  time_series[time_series == Na_value]=NA
  
  # Preprocessing time series: management of missing values and outliers
  print("Data pre-processing")
  clean_time_series <- tsclean(time_series, replace.missing = TRUE) 
  
  # Monthly time series
  if (frequency == 12){
    
    #CHL
    if ((type == "CHL") | (type == "chl")){
      
      print("Fitting best ARIMA model")
      arima_model <- auto.arima(clean_time_series, lambda="auto",seasonal = TRUE,  max.P = 5,  max.Q = 5)
      
      print("Residuals diagnostics")
      pvalue_corr <- checkresiduals(arima_model,plot=FALSE)$p.value
      
      ttest <-t.test(residuals(arima_model),mu=0)
      pvalue_mean <- ttest$p.value
      
      if (pvalue_corr < 0.05){
        print('Warning: There are correlations between residuals! The model can still be used for forecasting, but the prediction intervals may not be accurate due to the correlated residuals.')
      }
      if (pvalue_mean < 0.05){
        print('Warning: The residuals have not zero mean') 
      }
      
      print("Forecasting using ARIMA model")
      arima_forecast <-forecast(arima_model,num_predicted_values,confidence_levels)
      
      lower_80 <- as.numeric(arima_forecast$lower[1,1])
      upper_80 <- as.numeric(arima_forecast$upper[1,1])
      
      lower_95 <- as.numeric(arima_forecast$lower[1,2])
      upper_95 <- as.numeric(arima_forecast$upper[1,2])
      
      
      if ((time_series_new_value > lower_80) && (time_series_new_value < upper_80)) {
        
        isanomaly <- FALSE
        level <- "Not anomalous value"
        descr <- " "
        
        print('New data:'); print(time_series_new_value)
        print("No anomaly")
        
        interp_anomaly <- time_series_new_value
        
        
      } else if (time_series_new_value  <= lower_80  ){
        
        isanomaly <- TRUE
        level <- "Medium"
        descr <- " "
        
        print('New data:'); print(time_series_new_value)
        print("Anomaly : the value is less than expected")
        
        clean_time_series[1]=NA
        clean_time_series <- na.omit(clean_time_series)
        time_series_new_value= NA
        clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
        clean_time_series <- na.interp(clean_time_series)
        
        interp_anomaly <- clean_time_series[length(clean_time_series)]
        
        
      }else if ((time_series_new_value  >=upper_80)  && (time_series_new_value  <= upper_95 )) {
        
        isanomaly <- TRUE
        level <- "Medium"
        descr <- " "
        
        print('New data:'); print(time_series_new_value)
        print("Anomaly : the value is higher than expected")
        
        clean_time_series[1]=NA
        clean_time_series <- na.omit(clean_time_series)
        time_series_new_value= NA
        clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
        clean_time_series <- na.interp(clean_time_series)
        
        interp_anomaly <- clean_time_series[length(clean_time_series)]
        
        
      } else {
        
        isanomaly <- TRUE
        level <- "Severe" 
        descr <- " "
        
        print('New data:'); print(time_series_new_value)
        print("Serious anomaly : the value is higher than expected.")
        
        clean_time_series[1]=NA
        clean_time_series <- na.omit(clean_time_series)
        time_series_new_value= NA
        clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
        clean_time_series <- na.interp(clean_time_series)
        
        interp_anomaly <- clean_time_series[length(clean_time_series)]
        
        
        
      }} else if((type == "DOX") | (type == "dox")){ #DOX
        
        print("Fitting best ARIMA model")
        arima_model <- auto.arima(clean_time_series, lambda="auto",seasonal = TRUE,  max.P = 5,  max.Q = 5)
        
        print("Residuals diagnostics")
        pvalue_corr <- checkresiduals(arima_model,plot=FALSE)$p.value
        
        ttest <-t.test(residuals(arima_model),mu=0)
        pvalue_mean <- ttest$p.value
        
        if (pvalue_corr < 0.05){
          print('Warning: There are correlations between residuals! The model can still be used for forecasting, but the prediction intervals may not be accurate due to the correlated residuals.')
        }    
        if (pvalue_mean < 0.05){
          print('Warning: The residuals have not zero mean') 
        }
        
        print("Forecasting using ARIMA model")
        arima_forecast <-forecast(arima_model,num_predicted_values,confidence_levels)
        
        lower_80 <- as.numeric(arima_forecast$lower[1,1])
        upper_80 <- as.numeric(arima_forecast$upper[1,1])
        
        lower_95 <- as.numeric(arima_forecast$lower[1,2])
        upper_95 <- as.numeric(arima_forecast$upper[1,2])
        
        
        if ((time_series_new_value > lower_80) && (time_series_new_value < upper_80)) {
          
          isanomaly <- FALSE
          level <- "Not anomalous value"
          descr <- " "
          
          print('New data:'); print(time_series_new_value)
          print("No anomaly")
          
          interp_anomaly <- time_series_new_value 
          
          
        } else if (time_series_new_value  >= upper_80  ){
          
          isanomaly <- TRUE
          level <- "Medium"
          descr <- " "
          
          print('New data:'); print(time_series_new_value)
          print("Anomaly : the value is higher than expected")
          
          clean_time_series[1]=NA
          clean_time_series <- na.omit(clean_time_series)
          time_series_new_value= NA
          clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
          clean_time_series <- na.interp(clean_time_series)
          
          interp_anomaly <- clean_time_series[length(clean_time_series)]
          
          
        }else if ((time_series_new_value  >=lower_95)  && (time_series_new_value  <= lower_80 )) {
          
          isanomaly <- TRUE
          level <- "Medium"
          descr <- " " 
          
          print('New data:'); print(time_series_new_value)
          print("Anomaly : the value is less than expected")
          
          clean_time_series[1]=NA
          clean_time_series <- na.omit(clean_time_series)
          time_series_new_value= NA
          clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
          clean_time_series <- na.interp(clean_time_series)
          
          interp_anomaly <- clean_time_series[length(clean_time_series)]
          
          
        } else {
          
          isanomaly <- TRUE
          level <- "Severe"
          descr <- " "
          
          print('New data:'); print(time_series_new_value)
          print("Serious anomaly : the value is higher than expected.")
          
          clean_time_series[1]=NA
          clean_time_series <- na.omit(clean_time_series)
          time_series_new_value= NA
          clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
          clean_time_series <- na.interp(clean_time_series)
          
          interp_anomaly <- clean_time_series[length(clean_time_series)]
          
          
        }} else if((type == "SAL") | (type == "sal")|(type == "SST")|(type == "sst")){ # SAL - SST
          
          print("Fitting best ARIMA model")
          arima_model <- auto.arima(clean_time_series, lambda="auto",seasonal = TRUE,  max.P = 5,  max.Q = 5)
          
          print("Residuals diagnostics")
          pvalue_corr <- checkresiduals(arima_model,plot=FALSE)$p.value
          
          ttest <-t.test(residuals(arima_model),mu=0)
          pvalue_mean <- ttest$p.value
          
          if (pvalue_corr < 0.05){
            print('Warning: There are correlations between residuals! The model can still be used for forecasting, but the prediction intervals may not be accurate due to the correlated residuals.')
          }
          if (pvalue_mean < 0.05){
            print('Warning: The residuals have not zero mean') 
          }
          
          print("Forecasting using ARIMA model")
          arima_forecast <-forecast(arima_model,num_predicted_values,confidence_levels)
          
          lower_80 <- as.numeric(arima_forecast$lower[1,1])
          upper_80 <- as.numeric(arima_forecast$upper[1,1])
          
          lower_95 <- as.numeric(arima_forecast$lower[1,2])
          upper_95 <- as.numeric(arima_forecast$upper[1,2])
          
          
          if ((time_series_new_value > lower_80) && (time_series_new_value < upper_80)) {
            
            isanomaly <- FALSE
            level <- "Not anomalous value"
            descr <- " "
            
            print('New data:'); print(time_series_new_value)
            print("No anomaly")
            
            interp_anomaly <- time_series_new_value 
            
            
          } else if ((time_series_new_value  >= upper_80)&&(time_series_new_value  <= upper_95)){
            
            isanomaly <- TRUE
            level <- "Medium"
            descr <- " "
            
            print('New data:'); print(time_series_new_value)
            print("Anomaly : the value is higher than expected")
            
            clean_time_series[1]=NA
            clean_time_series <- na.omit(clean_time_series)
            time_series_new_value= NA
            clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
            clean_time_series <- na.interp(clean_time_series)
            
            interp_anomaly <- clean_time_series[length(clean_time_series)]
            
            
          }else if ((time_series_new_value  >=lower_95)  && (time_series_new_value  <= lower_80 )) {
            
            isanomaly <- TRUE
            level <- "Medium"
            descr <- " "
            
            print('New data:'); print(time_series_new_value)
            print("Anomaly : the value is less than expected")
            
            clean_time_series[1]=NA
            clean_time_series <- na.omit(clean_time_series)
            time_series_new_value= NA
            clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
            clean_time_series <- na.interp(clean_time_series)
            
            interp_anomaly <- clean_time_series[length(clean_time_series)]
            
            
          }else if (time_series_new_value > upper_95) {
            
            isanomaly <- TRUE
            level <- "Severe"
            descr <- " "
            
            print('New data:'); print(time_series_new_value)
            print("Anomaly : the value is higher than expected")
            
            clean_time_series[1]=NA
            clean_time_series <- na.omit(clean_time_series)
            time_series_new_value= NA
            clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
            clean_time_series <- na.interp(clean_time_series)
            
            interp_anomaly <- clean_time_series[length(clean_time_series)]
            
            
          } else {
            
            isanomaly <- TRUE 
            level <- "Severe" 
            descr <- " " 
            
            print('New data:'); print(time_series_new_value)
            print("Serious anomaly : the value is less than expected.")
            
            clean_time_series[1]=NA
            clean_time_series <- na.omit(clean_time_series)
            time_series_new_value= NA
            clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
            clean_time_series <- na.interp(clean_time_series)
            
            interp_anomaly <- clean_time_series[length(clean_time_series)]
            
            
          }} else {
            
            stop('Error: Type of parameter not found')
            
            
            
            
          }} else if (frequency == 52){# Weekly time series
            
            #CHL
            if ((type == "CHL") | (type == "chl")){
              
              print("Fitting best ARIMA model")
              arima_model <- auto.arima(clean_time_series, lambda="auto",seasonal = TRUE,  max.P = 5,  max.Q = 5)
              
              print("Residuals diagnostics")
              pvalue_corr <- checkresiduals(arima_model,plot=FALSE)$p.value
              
              ttest <-t.test(residuals(arima_model),mu=0)
              pvalue_mean <- ttest$p.value
              
              if (pvalue_corr < 0.05){
                print('Warning: There are correlations between residuals! The model can still be used for forecasting, but the prediction intervals may not be accurate due to the correlated residuals.') 
              }
              if (pvalue_mean < 0.05){
                print('Warning: The residuals have not zero mean') 
              }
              
              print("Forecasting using ARIMA model")
              arima_forecast <-forecast(arima_model,num_predicted_values,confidence_levels)
              
              lower_80 <- as.numeric(arima_forecast$lower[1,1])
              upper_80 <- as.numeric(arima_forecast$upper[1,1])
              
              lower_95 <- as.numeric(arima_forecast$lower[1,2])
              upper_95 <- as.numeric(arima_forecast$upper[1,2])
              
              
              if ((time_series_new_value > lower_80) && (time_series_new_value < upper_80)) {
                
                isanomaly <- FALSE
                level <- "Not anomalous value"
                descr <- " "
                
                print('New data:'); print(time_series_new_value)
                print("No anomaly")
                
                interp_anomaly<- time_series_new_value 
                
                
              } else if (time_series_new_value  <= lower_80  ){
                
                isanomaly <- TRUE
                level <- "Medium"
                descr <- " "
                
                print('New data:'); print(time_series_new_value)
                print("Anomaly : the value is less than expected")
                
                clean_time_series[1]=NA
                clean_time_series <- na.omit(clean_time_series)
                time_series_new_value= NA
                start_new <- start(clean_time_series)
                clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
                clean_time_series <-ts(clean_time_series,start_new,frequency=52)
                clean_time_series <- na.interp(clean_time_series)
                
                interp_anomaly <- clean_time_series[length(clean_time_series)]
                
                
                
              }else if ((time_series_new_value  >=upper_80)  && (time_series_new_value  <= upper_95 )) {
                
                isanomaly <- TRUE
                level <- "Medium"
                descr <- " "
                
                print('New data:'); print(time_series_new_value)
                print("Anomaly : the value is higher than expected")
                
                clean_time_series[1]=NA
                clean_time_series <- na.omit(clean_time_series)
                time_series_new_value= NA
                start_new <- start(clean_time_series)
                clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
                clean_time_series <-ts(clean_time_series,start_new,frequency=52)            
                clean_time_series <- na.interp(clean_time_series)
                
                interp_anomaly <- clean_time_series[length(clean_time_series)]
                
                
              } else {
                
                isanomaly <- TRUE
                level <- "Severe"
                descr <- " "
                
                print('New data:'); print(time_series_new_value)
                print("Serious anomaly : the value is higher than expected.")
                
                clean_time_series[1]=NA
                clean_time_series <- na.omit(clean_time_series)
                time_series_new_value= NA
                start_new <- start(clean_time_series)
                clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
                clean_time_series <-ts(clean_time_series,start_new,frequency=52)            
                clean_time_series <- na.interp(clean_time_series)
                
                interp_anomaly <- clean_time_series[length(clean_time_series)]
                
                
              }} else if((type == "DOX") | (type == "dox")){ #SAL
                
                print("Fitting best ARIMA model")
                arima_model <- auto.arima(clean_time_series, lambda="auto",seasonal = TRUE,  max.P = 5,  max.Q = 5)
                
                print("Residuals diagnostics")
                pvalue_corr <- checkresiduals(arima_model,plot=FALSE)$p.value
                
                ttest <-t.test(residuals(arima_model),mu=0)
                pvalue_mean <- ttest$p.value
                
                if (pvalue_corr < 0.05){
                  print('Warning: There are correlations between residuals! The model can still be used for forecasting, but the prediction intervals may not be accurate due to the correlated residuals.') 
                }
                if (pvalue_mean < 0.05){
                  print('Warning: The residuals have not zero mean') 
                }
                
                print("Forecasting using ARIMA model")
                arima_forecast <-forecast(arima_model,num_predicted_values,confidence_levels)
                
                lower_80 <- as.numeric(arima_forecast$lower[1,1])
                upper_80 <- as.numeric(arima_forecast$upper[1,1])
                
                lower_95 <- as.numeric(arima_forecast$lower[1,2])
                upper_95 <- as.numeric(arima_forecast$upper[1,2])
                
                
                if ((time_series_new_value > lower_80) && (time_series_new_value < upper_80)) {
                  
                  isanomaly <- FALSE
                  level <- "Not anomalous value"
                  descr <- " "
                  
                  print('New data:'); print(time_series_new_value)
                  print("No anomaly")
                  
                  interp_anomaly<- time_series_new_value 
                  
                } else if (time_series_new_value  >= upper_80  ){
                  
                  isanomaly <- TRUE
                  level <- "Medium"
                  descr <- " "
                  
                  print('New data:'); print(time_series_new_value)
                  print("Anomaly : the value is higher than expected")
                  
                  clean_time_series[1]=NA
                  clean_time_series <- na.omit(clean_time_series)
                  time_series_new_value= NA
                  start_new <- start(clean_time_series)
                  clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
                  clean_time_series <-ts(clean_time_series,start_new,frequency=52)              
                  clean_time_series <- na.interp(clean_time_series)
                  
                  interp_anomaly <- clean_time_series[length(clean_time_series)]
                  
                  
                  
                }else if ((time_series_new_value  >=lower_95)  && (time_series_new_value  <= lower_80 )) {
                  
                  isanomaly <- TRUE
                  level <- "Medium"
                  descr <- " "
                  
                  print('New data:'); print(time_series_new_value)
                  print("Anomaly : the value is less than expected")
                  
                  clean_time_series[1]=NA
                  clean_time_series <- na.omit(clean_time_series)
                  time_series_new_value= NA
                  start_new <- start(clean_time_series)
                  clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
                  clean_time_series <-ts(clean_time_series,start_new,frequency=52)              
                  clean_time_series <- na.interp(clean_time_series)
                  
                  interp_anomaly <- clean_time_series[length(clean_time_series)]
                  
                  
                  
                } else {
                  
                  isanomaly <- TRUE
                  level <- "Severe"
                  descr <- " "
                  
                  print('New data:'); print(time_series_new_value)
                  print("Serious anomaly : the value is higher than expected.")
                  
                  clean_time_series[1]=NA
                  clean_time_series <- na.omit(clean_time_series)
                  time_series_new_value= NA
                  start_new <- start(clean_time_series)
                  clean_time_series <- ts_bind(clean_time_series,time_series_new_value)
                  clean_time_series <-ts(clean_time_series,start_new,frequency=52)              
                  clean_time_series <- na.interp(clean_time_series)
                  
                  interp_anomaly <- clean_time_series[length(clean_time_series)]
                  
                  
                }} else if((type == "SAL") | (type == "sal")|(type == "SST")|(type == "sst")){ # SAL - SST
                  
                  print("Fitting best ARIMA model")
                  arima_model <- auto.arima(clean_time_series, lambda="auto",seasonal = TRUE,  max.P = 5,  max.Q = 5)
                  
                  print("Residuals diagnostics")
                  pvalue_corr <- checkresiduals(arima_model,plot=FALSE)$p.value
                  
                  ttest <-t.test(residuals(arima_model),mu=0)
                  pvalue_mean <- ttest$p.value
                  
                  if (pvalue_corr < 0.05){
                    print('Warning: There are correlations between residuals! The model can still be used for forecasting, but the prediction intervals may not be accurate due to the correlated residuals.') 
                  }
                  if (pvalue_mean < 0.05){
                    print('Warning: The residuals have not zero mean') 
                  }
                  
                  print("Forecasting using ARIMA model")
                  arima_forecast <-forecast(arima_model,num_predicted_values,confidence_levels)
                  
                  lower_80 <- as.numeric(arima_forecast$lower[1,1])
                  upper_80 <- as.numeric(arima_forecast$upper[1,1])
                  
                  lower_95 <- as.numeric(arima_forecast$lower[1,2])
                  upper_95 <- as.numeric(arima_forecast$upper[1,2])
                  
                  
                  if ((time_series_new_value > lower_80) && (time_series_new_value < upper_80)) {
                    
                    isanomaly <- FALSE
                    level <- "Not anomalous value"
                    descr <- " "
                    
                    print('New data:'); print(time_series_new_value)
                    print("No anomaly")
                    
                    interp_anomaly<- time_series_new_value 
                    
                  } else if((time_series_new_value>=upper_80)&&(time_series_new_value<= upper_95)){
                    
                    isanomaly <- TRUE
                    level <- "Medium"
                    descr <- " "
                    
                    print('New data:'); print(time_series_new_value)
                    print("Anomaly : the value is higher than expected")
                    
                    clean_time_series[1]=NA
                    clean_time_series <- na.omit(clean_time_series)
                    time_series_new_value= NA
                    start_new <- start(clean_time_series)
                    clean_time_series<-ts_bind(clean_time_series,time_series_new_value)
                    clean_time_series <-ts(clean_time_series,start_new,frequency=52)                
                    clean_time_series <- na.interp(clean_time_series)
                    
                    interp_anomaly <- clean_time_series[length(clean_time_series)]
                    
                    
                  }else if((time_series_new_value>=lower_95)&&(time_series_new_value<= lower_80)) {
                    
                    isanomaly <- TRUE
                    level <- "Medium"
                    descr <- " "
                    
                    print('New data:'); print(time_series_new_value)
                    print("Anomaly : the value is less than expected")
                    
                    clean_time_series[1]=NA
                    clean_time_series <- na.omit(clean_time_series)
                    time_series_new_value= NA
                    start_new <- start(clean_time_series)
                    clean_time_series<-ts_bind(clean_time_series,time_series_new_value)
                    clean_time_series <-ts(clean_time_series,start_new,frequency=52)                         
                    clean_time_series <- na.interp(clean_time_series)
                    
                    interp_anomaly <- clean_time_series[length(clean_time_series)]
                    
                    
                  }else if (time_series_new_value > upper_95) {
                    
                    isanomaly <- TRUE
                    level <- "Severe"
                    descr <- " "
                    
                    print('New data:'); print(time_series_new_value)
                    print("Anomaly : the value is higher than expected")
                    
                    clean_time_series[1]=NA
                    clean_time_series <- na.omit(clean_time_series)
                    time_series_new_value= NA
                    start_new <- start(clean_time_series)
                    clean_time_series<-ts_bind(clean_time_series,time_series_new_value)
                    clean_time_series <-ts(clean_time_series,start_new,frequency=52)                
                    clean_time_series <- na.interp(clean_time_series)
                    
                    interp_anomaly <- clean_time_series[length(clean_time_series)]
                    
                    
                  } else {
                    
                    isanomaly <- TRUE
                    level <- "Severe"
                    descr <- " "
                    
                    
                    print('New data:'); print(time_series_new_value)
                    print("Serious anomaly : the value is less than expected.")
                    
                    clean_time_series[1]=NA
                    clean_time_series <- na.omit(clean_time_series)
                    time_series_new_value= NA
                    start_new <- start(clean_time_series)
                    clean_time_series<-ts_bind(clean_time_series,time_series_new_value)
                    clean_time_series <-ts(clean_time_series,start_new,frequency=52)                
                    clean_time_series <- na.interp(clean_time_series)
                    
                    interp_anomaly <- clean_time_series[length(clean_time_series)]
                    
                    
                    
                  }} else {
                    
                    stop('Error: Type of parameter not found')
                    
                  }}
  
  
  results <-list(isanomaly=isanomaly,level=level,interp_anomaly=interp_anomaly,descr=descr)
  return(results)