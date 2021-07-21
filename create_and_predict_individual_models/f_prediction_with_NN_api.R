f_prediction_with_NN_api <- function(forecastData, histData, pred_model){
  # This function returns 15min  based predictions for given customer and and given day   based on historical clusterings and future weather prediction.
  # output: vector of 15-min  predictions based on NN
  # input: 
  #  forecastData - data of weather forecsats for the day we want to predict
  #  histData - historical data about consumption for last few days
  #  pred_model - NN model to be used for prediction
  # this function replaces f_prediction_with_clustering_api for NN
  # created 14.10.2020: 
  
  if (isempty(pred_model)){
    prediction=c()
    return(prediction)
  }
  
  
  lengths<-sapply(histData$consumptions$values,FUN="length")
  select_cons=which(lengths==96)
  Consumptions_clean<-matrix(unlist(histData$consumptions$values[select_cons]),byrow=TRUE,ncol=96)
  
  temp_obs<-sapply(histData$observations$temperature[select_cons], "mean")
  prec_obs<-sapply(histData$observations$precipitation[select_cons], "mean")
  press_obs<-sapply(histData$observations$pressure[select_cons], "mean")
  wind_obs<-sapply(histData$observations$wind_speed[select_cons], "mean")
  humid_obs<-sapply(histData$observations$humidity[select_cons], "mean")
  
  month_obs<-as.numeric(substr(histData$dateInfo$date[select_cons],6,7))
  day_obs<-as.numeric(substr(histData$dateInfo$date[select_cons],9,10))
  free_day_obs<-as.numeric(histData$dateInfo$freeDay[select_cons])
  
  days_ind=c(1:7)
  names(days_ind)=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  week_day_obs=as.numeric(days_ind[histData$dateInfo$dayName[select_cons]])
  
  train_data<-data.frame(temp=temp_obs,prec=prec_obs,press=press_obs,wind=wind_obs,hum=humid_obs,m=month_obs,d=day_obs,free=free_day_obs,dw=week_day_obs)
  train_data_na_row<-unique(which(is.na(train_data)==TRUE,TRUE)[,1])
  train_data_keep_row=setdiff(c(1:dim(train_data)[1]),train_data_na_row)
  train_data<-train_data[train_data_keep_row,]
  
  # prediction for prediction day
  temp_forcast<-mean(as.double(forecastData$forecast$temperature))
  prec_forcast<-mean(as.double(forecastData$forecast$precipitation))
  press_forcast<-mean(as.double(forecastData$forecast$pressure))
  wind_forcast<-mean(as.double(forecastData$forecast$wind_speed))
  humid_forcast<-mean(as.double(forecastData$forecast$humidity))
  
  month_forcast<-as.numeric(substr(forecastData$dateInfo$date,6,7))
  day_forcast<-as.numeric(forecastData$dateInfo$dayNum)
  free_day_forcast<-as.numeric(forecastData$dateInfo$freeDay)
  
  days_ind=c(1:7)
  names(days_ind)=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  week_day_forcast=as.numeric(days_ind[forecastData$dateInfo$dayName])
  
  data_pred<-data.frame(temp=temp_forcast,prec=prec_forcast,press=press_forcast,wind=wind_forcast,hum=humid_forcast,m=month_forcast,d=day_forcast,free=free_day_forcast,dw=week_day_forcast)

  all_data <- data_pred %>% rbind(train_data)
  all_data$d <- as.factor(all_data$d)
  all_data$dw <- as.factor(all_data$dw)
  all_data$m <- as.factor(all_data$m)
  
  all_data <- all_data %>% dplyr::mutate(
    temp = (temp-min(temp))/(max(temp) - min(temp)),
    prec = (prec-min(prec))/(max(prec) - min(prec)),
    press = (press-min(press))/(max(press) - min(press)),
    wind = (wind-min(wind))/(max(wind) - min(wind)),
    hum = (hum-min(hum))/(max(hum) - min(hum))
  )
  data_pred <- all_data[1,]
  day_hist<-sum(as.vector(rownames(pred_model$MAPE))==forecastData$dateInfo$date)
  if(!day_hist){
    clust_pred = max.col(predict(pred_model$class_model, newdata = data_pred) %>% as.matrix(), 'first')
  } else{
    # presko?imo AI predikcijo klastra
    clust_pred<-pred_model$train_data[day_hist,"clust"]
  }
  pred_consumption <- pred_model$centroids[clust_pred,]
  prediction=list(date=forecastData$dateInfo$date,predicted_consumption=pred_consumption)
  return(prediction)
}