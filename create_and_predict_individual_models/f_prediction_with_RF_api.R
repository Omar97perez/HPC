f_prediction_with_RF_api <- function(forecastData, pred_model){
  # This function returns 15min  based predictions for given customer and and given day   based on historical clusterings and future weather prediction.
  # output: vector of 15-min  predictions
  # input: 
  #  forecastData - data of weather forecsats for the day we want to predict
  #  pred_model - RF model to be used for prediction
  # this function replaces f_prediction_with_clustering_api
  # created 14.10.2020: 
  # last change: 14 10 2020
  
  if (isempty(pred_model)){
    prediction=c()
    return(prediction)
  }
  
  
  
  days_ind=c(1:7)
  names(days_ind)=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

  # prediction for prediction day
  temp_forcast<-mean(as.double(forecastData$forecast$temperature))
  prec_forcast<-mean(as.double(forecastData$forecast$precipitation))
  press_forcast<-mean(as.double(forecastData$forecast$pressure))
  wind_forcast<-mean(as.double(forecastData$forecast$wind_speed))
  humid_forcast<-mean(as.double(forecastData$forecast$humidity))
  
  month_forcast<-as.factor(as.numeric(substr(forecastData$dateInfo$date,6,7)))
  day_forcast<-as.factor(as.numeric(forecastData$dateInfo$dayNum))
  free_day_forcast<-as.numeric(forecastData$dateInfo$freeDay)
  
  days_ind=c(1:7)
  names(days_ind)=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  week_day_forcast=as.factor(as.numeric(days_ind[forecastData$dateInfo$dayName]))
  
  data_pred<-data.frame(temp=temp_forcast,prec=prec_forcast,press=press_forcast,wind=wind_forcast,hum=humid_forcast,m=month_forcast,d=day_forcast,free=free_day_forcast,dw=week_day_forcast)
  #centralising data
  data_pred$temp=(data_pred$temp-pred_model$min_max_weather["min","temp"])/(pred_model$min_max_weather["max","temp"]-pred_model$min_max_weather["min","temp"])
  data_pred$prec=(data_pred$prec-pred_model$min_max_weather["min","prec"])/(pred_model$min_max_weather["max","prec"]-pred_model$min_max_weather["min","prec"])
  data_pred$press=(data_pred$press-pred_model$min_max_weather["min","press"])/(pred_model$min_max_weather["max","press"]-pred_model$min_max_weather["min","press"])
  data_pred$wind=(data_pred$wind-pred_model$min_max_weather["min","wind"])/(pred_model$min_max_weather["max","wind"]-pred_model$min_max_weather["min","wind"])
  data_pred$hum=(data_pred$hum-pred_model$min_max_weather["min","hum"])/(pred_model$min_max_weather["max","hum"]-pred_model$min_max_weather["min","hum"])

  day_hist<-sum(as.vector(rownames(pred_model$MAPE))==forecastData$dateInfo$date)
  data_pred_f=data.frame(clus_km_clusters=as.factor(c(1)),data_pred)
  levels(data_pred_f$clus_km_clusters)=levels(pred_model$train_data$clus_km_clusters)
  levels(data_pred_f$m)=levels(pred_model$train_data$m)
  levels(data_pred_f$d)=levels(pred_model$train_data$d)
  levels(data_pred_f$dw)=levels(pred_model$train_data$dw)
  sapply(data_pred_f, class)
  sapply(pred_model$train_data, class)
  if(!day_hist){
    clust_pred = predict(pred_model$class_model, newdata=data_pred_f)
  } else{
    # presko?imo AI predikcijo klastra
    clust_pred<-pred_model$train_data[day_hist,"clust"]
  }
  pred_consumption <- pred_model$centroids[clust_pred,]
  prediction=list(date=forecastData$dateInfo$date,predicted_consumption=pred_consumption)
  return(prediction)
}