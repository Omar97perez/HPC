# auxiliary function for column normalization (actualy not included in the base package...)
range01 <- function(x,m=c(),M=c()){
  if (isempty(x)){
    return(c())
  }
  if (isempty(m)){
    m=min(x)
  }
  if (isempty(M)){
    M=max(x)
  }
  if (m==M){
    return(x)
  } else {
    x <- (x-m)/(M-m)
    return(x)
  }
}


f_prediction_with_RF_api_new <- function(forecastData, histData, pred_model){
  # This function returns 15min based predictions for given customer and and given day based on historical clusterings and future weather prediction.
  # output: vector of 15-min  predictions based on RF (Random Forest predictions)
  # input: 
  #  forecastData - data of weather forecsats for the day we want to predict
  #  histData - historical data about consumption for last few days
  #  pred_model - NN model to be used for prediction
  # this function replaces f_prediction_with_clustering_api for NN (new version incorporating historic data)
  # created 31.10.2020: 
  # 5 11 2020: range01 improved
  
  lengths<-sapply(histData$consumptions$values,FUN="length")
  select_cons=which(lengths==96)
  Consumptions_clean<-matrix(unlist(histData$consumptions$values[select_cons]),byrow=TRUE,ncol=96)
  
  temp_obs<-rev(sapply(histData$observations$temperature[select_cons], "mean"))
  temp_var<-rev(sapply(histData$observations$temperature[select_cons], "var"))
  prec_obs<-rev(sapply(histData$observations$precipitation[select_cons], "mean"))
  prec_var<-rev(sapply(histData$observations$precipitation[select_cons], "var"))
  wind_obs<-rev(sapply(histData$observations$wind_speed[select_cons], "mean"))
  wind_var<-rev(sapply(histData$observations$wind_speed[select_cons], "var"))
  humid_obs<-rev(sapply(histData$observations$humidity[select_cons], "mean"))
  press_obs<-rev(sapply(histData$observations$pressure[select_cons], "mean"))
  
  month_obs<-rev(as.numeric(substr(histData$dateInfo$date[select_cons],6,7)))
  day_obs<-rev(as.numeric(substr(histData$dateInfo$date[select_cons],9,10)))
  free_day_obs<-rev(as.numeric(histData$dateInfo$freeDay[select_cons]))
  
  days_ind=c(1:7)
  names(days_ind)=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  week_day_obs=rev(as.numeric(days_ind[histData$dateInfo$dayName[select_cons]]))
  
  # consumption manipulation (we use the previous values for future predictions)
  consump_mean <- rev(apply(Consumptions_clean, 1, mean))
  consump_var <- rev(apply(Consumptions_clean, 1, var))
  
  # prediction for prediction day
  temp_forcast<-mean(as.double(forecastData$forecast$temperature))
  temp_var_forecast<-var(as.double(forecastData$forecast$temperature))
  prec_forcast<-mean(as.double(forecastData$forecast$precipitation))
  prec_var_forcast<-var(as.double(forecastData$forecast$precipitation))
  wind_forcast<-mean(as.double(forecastData$forecast$wind_speed))
  wind_var_forcast<-var(as.double(forecastData$forecast$wind_speed))
  press_forcast<-mean(as.double(forecastData$forecast$pressure))
  humid_forcast<-mean(as.double(forecastData$forecast$humidity))
  
  month_forcast<-as.numeric(substr(forecastData$dateInfo$date,6,7))
  day_forcast<-as.numeric(forecastData$dateInfo$dayNum)
  free_day_forcast<-as.numeric(forecastData$dateInfo$freeDay)
  
  days_ind=c(1:7)
  names(days_ind)=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  week_day_forcast=as.numeric(days_ind[forecastData$dateInfo$dayName])
  
  data_pred<-data.frame(temp=c(temp_forcast,temp_obs),
                        temp_var=c(temp_var_forecast,temp_var),
                        prec=c(prec_forcast,prec_obs),
                        prec_var=c(prec_var_forcast,prec_var),
                        press=c(press_forcast,press_obs),
                        wind=c(wind_forcast,wind_obs),
                        wind_var=c(wind_var_forcast,wind_var),
                        hum=c(humid_forcast,humid_obs),
                        m=c(month_forcast,month_obs),
                        d=c(day_forcast,day_obs),
                        free=c(free_day_forcast,free_day_obs),
                        dw=c(week_day_forcast,week_day_obs),
                        consump1_mean=c(consump_mean,NA),
                        consump1_var=c(consump_var,NA),
                        consump2_mean=c(consump_mean[2:length(consump_mean)],NA,NA),
                        consump2_var=c(consump_var[2:length(consump_var)],NA,NA),
                        consump3_mean=c(consump_mean[3:length(consump_mean)],NA,NA,NA),
                        consump3_var=c(consump_var[3:length(consump_var)],NA,NA,NA),
                        consump4_mean=c(consump_mean[4:length(consump_mean)],NA,NA,NA,NA),
                        consump4_var=c(consump_var[4:length(consump_var)],NA,NA,NA,NA),
                        consump5_mean=c(consump_mean[5:length(consump_mean)],NA,NA,NA,NA,NA),
                        consump5_var=c(consump_var[5:length(consump_var)],NA,NA,NA,NA,NA),
                        consump6_mean=c(consump_mean[6:length(consump_mean)],NA,NA,NA,NA,NA,NA),
                        consump6_var=c(consump_var[6:length(consump_var)],NA,NA,NA,NA,NA,NA),
                        consump7_mean=c(consump_mean[7:length(consump_mean)],NA,NA,NA,NA,NA,NA,NA),
                        consump7_var=c(consump_var[7:length(consump_var)],NA,NA,NA,NA,NA,NA,NA)
  )
  
  data_pred<-data_pred[complete.cases(data_pred),]
  
  data_pred$d <- factor(data_pred$d, levels = as.character(1:31))
  data_pred$dw <- factor(data_pred$dw, levels = as.character(1:7))
  data_pred$m <- factor(data_pred$m, levels = as.character(1:12))
  data_pred$free <- factor(data_pred$free, levels = as.character(0:1))
  
  data_pred <- data_pred %>% dplyr::mutate(
    temp = range01(temp,pred_model$min_max_weather["min","temp"],pred_model$min_max_weather["max","temp"]),
    temp_var = range01(temp_var,pred_model$min_max_weather["min","temp_var"],pred_model$min_max_weather["max","temp_var"]),
    prec = range01(prec,pred_model$min_max_weather["min","prec"],pred_model$min_max_weather["max","prec"]),
    prec_var = range01(prec_var,pred_model$min_max_weather["min","prec_var"],pred_model$min_max_weather["max","prec_var"]),
    wind = range01(wind,pred_model$min_max_weather["min","wind"],pred_model$min_max_weather["max","wind"]),
    wind_var = range01(wind_var,pred_model$min_max_weather["min","wind_var"],pred_model$min_max_weather["max","wind_var"]),
    press = range01(press,pred_model$min_max_weather["min","press"],pred_model$min_max_weather["max","press"]),
    hum = range01(hum,pred_model$min_max_weather["min","hum"],pred_model$min_max_weather["max","hum"]),
    consump1_mean = range01(consump1_mean,pred_model$min_max_weather["min","consump1_mean"],pred_model$min_max_weather["max","consump1_mean"]),
    consump1_var = range01(consump1_var,pred_model$min_max_weather["min","consump1_var"],pred_model$min_max_weather["max","consump1_var"]),
    consump2_mean = range01(consump2_mean,pred_model$min_max_weather["min","consump2_mean"],pred_model$min_max_weather["max","consump2_mean"]),
    consump2_var = range01(consump2_var,pred_model$min_max_weather["min","consump2_var"],pred_model$min_max_weather["max","consump2_var"]),
    consump3_mean = range01(consump3_mean,pred_model$min_max_weather["min","consump3_mean"],pred_model$min_max_weather["max","consump3_mean"]),
    consump3_var = range01(consump3_var,pred_model$min_max_weather["min","consump3_var"],pred_model$min_max_weather["max","consump3_var"]),
    consump4_mean = range01(consump4_mean,pred_model$min_max_weather["min","consump4_mean"],pred_model$min_max_weather["max","consump4_mean"]),
    consump4_var = range01(consump4_var,pred_model$min_max_weather["min","consump4_var"],pred_model$min_max_weather["max","consump4_var"]),
    consump5_mean = range01(consump5_mean,pred_model$min_max_weather["min","consump5_mean"],pred_model$min_max_weather["max","consump5_mean"]),
    consump5_var = range01(consump5_var,pred_model$min_max_weather["min","consump5_var"],pred_model$min_max_weather["max","consump5_var"]),
    consump6_mean = range01(consump6_mean,pred_model$min_max_weather["min","consump6_mean"],pred_model$min_max_weather["max","consump6_mean"]),
    consump6_var = range01(consump6_var,pred_model$min_max_weather["min","consump6_var"],pred_model$min_max_weather["max","consump6_var"]),
    consump7_mean = range01(consump7_mean,pred_model$min_max_weather["min","consump7_mean"],pred_model$min_max_weather["max","consump7_mean"]),
    consump7_var = range01(consump7_var,pred_model$min_max_weather["min","consump7_var"],pred_model$min_max_weather["max","consump7_var"])
  ) 
  data_pred <- data_pred[1,]
  
  clust_pred = predict(pred_model$class_model, newdata=data_pred)
  
  pred_consumption <- pred_model$centroids[clust_pred,]
  prediction=list(date=forecastData$dateInfo$date,predicted_consumption=pred_consumption)
  return(prediction)
}