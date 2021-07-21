f_create_model_clust_api <- function(histData, varNOOfClusters = 15, modelType =  'RF'){
  # This function returns model for hourly based predictions for given customer based on clustering of histrorical data
  # output: - a matrix of clusters means
  #         - a prediction model for prediciting the clusters using class_method
  # input:  - varConsumerID . id of customer
  #         - clust_method="KMeans",
  #         - varNOOfClusters (number of clusters used in model)
  #         - method used to predict clustering, default ="random forest RF"
  # created by J Povh, 22.4.2020
  # change: 10 6 2020 by J POvh: added MAPE to the model
  # change 15 7 2020: added full test data set (with full weather data)
  # change 6 10 2020: improved preparation of train data
  # change 13 10 2020: improved NN modeling (M Rogar)
  # change 22 10 2020: removed "train_data_FULL" from the output
  # change 23 10 2020: fixed bug: to prevent empty centroids
  # change 5 11 2020: return empty model if data is not correct
  if (modelType == 'RF'){
    lengths<-sapply(histData$consumptions$values,FUN="length")
    select_cons=which(lengths==96)
    Consumptions_clean<-matrix(unlist(histData$consumptions$values[select_cons]),byrow=TRUE,ncol=96)
    
    temp_obs<-sapply(histData$observations$temperature[select_cons], "mean")
    prec_obs<-sapply(histData$observations$precipitation[select_cons], "mean")
    press_obs<-sapply(histData$observations$pressure[select_cons], "mean")
    wind_obs<-sapply(histData$observations$wind_speed[select_cons], "mean")
    humid_obs<-sapply(histData$observations$humidity[select_cons], "mean")
    
    year_obs<-as.numeric(substr(histData$dateInfo$date[select_cons],1,4))
    month_obs<-as.numeric(substr(histData$dateInfo$date[select_cons],6,7))
    day_obs<-as.numeric(substr(histData$dateInfo$date[select_cons],9,10))
    free_day_obs<-as.numeric(histData$dateInfo$freeDay[select_cons])
    
    days_ind=c(1:7)
    names(days_ind)=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    
    week_day_obs=as.numeric(days_ind[histData$dateInfo$dayName[select_cons]])
    
    #Day_Consumptions=t(as.matrix(sapply(Consumptions_clean, `[[`, "values")))
    #dates_cons=sapply(Consumptions_clean, `[[`, "date")
    
    #we delete days for which weather data is not complete
    train_data<-data.frame(temp=temp_obs,prec=prec_obs,press=press_obs,wind=wind_obs,hum=humid_obs,y=year_obs,m=month_obs,d=day_obs,free=free_day_obs,dw=week_day_obs)
    train_data_na_row<-unique(which(is.na(train_data)==TRUE,TRUE)[,1])
    train_data_keep_row=setdiff(c(1:dim(train_data)[1]),train_data_na_row)
    train_data<-train_data[train_data_keep_row,]
    Consumptions_clean<-Consumptions_clean[train_data_keep_row,]
    select_cons<-select_cons[train_data_keep_row]
    
    if (dim(Consumptions_clean)[1] < varNOOfClusters){
      pred_model<-c()
      return(pred_model)     
    }
    
    clus_km <- kmeans(Consumptions_clean, centers = varNOOfClusters)
    clus_km_clusters <- clus_km$cluster
    train_data<-cbind(clus_km_clusters,train_data)
    
    #transform training data
    train_data$clus_km_clusters <- as.factor(train_data$clus_km_clusters)
    train_data$d <- as.factor(train_data$d)
    train_data$dw <- as.factor(train_data$dw)
    train_data$m <- as.factor(train_data$m)
    train_data$y <- NULL
    
    min_max_weather=matrix(0,nrow = 2,ncol=5)
    colnames(min_max_weather)<-c("temp","prec","press","wind","hum")
    rownames(min_max_weather)<-c("min","max")
    
    min_max_weather["min",]=c(min(train_data[,"temp"]),min(train_data[,"prec"]),min(train_data[,"press"]),min(train_data[,"wind"]),min(train_data[,"hum"]))
    min_max_weather["max",]=c(max(train_data[,"temp"]),max(train_data[,"prec"]),max(train_data[,"press"]),max(train_data[,"wind"]),max(train_data[,"hum"]))
    
    train_data <- train_data %>% dplyr::mutate(
      temp = (temp-min(temp))/(max(temp) - min(temp)),
      prec = (prec-min(prec))/(max(prec) - min(prec)),
      press = (press-min(press))/(max(press) - min(press)),
      wind = (wind-min(wind))/(max(wind) - min(wind)),
      hum = (hum-min(hum))/(max(hum) - min(hum))
    ) 
    
    #build prediction model - random forest
    model_rand_forest <- randomForest(clus_km_clusters ~ ., data=train_data,na.action=na.omit,importance=TRUE,ntree=50)
    
    cent_mat<-matrix(0,varNOOfClusters,96)
    for(i in c(1:varNOOfClusters)){
      clust_points = Consumptions_clean[train_data$clus_km_clusters==i,]
      if (is.matrix(clust_points)){
        cent_mat[i,]<-colMeans(clust_points)
      } else {
        cent_mat[i,]<-clust_points
      }
    }
    pred_clusters<-model_rand_forest$predicted
    MAPE_rand_forest=1/96*rowSums(abs(Consumptions_clean-cent_mat[pred_clusters,])/Consumptions_clean)
    MAPE_clustering=1/96*rowSums(abs(Consumptions_clean-cent_mat[train_data$clus_km_clusters,])/Consumptions_clean)
    MAPE<-data.frame(MAPE_rand_forest,MAPE_clustering,row.names = histData$dateInfo$date[select_cons])
    WAPE_rand_forest=rowSums(abs(Consumptions_clean-cent_mat[pred_clusters,]))/rowSums(Consumptions_clean)
    WAPE_clustering=rowSums(abs(Consumptions_clean-cent_mat[train_data$clus_km_clusters,]))/rowSums(Consumptions_clean)
    WAPE<-data.frame(WAPE_rand_forest,WAPE_clustering,row.names = histData$dateInfo$date[select_cons])
    
    # full training data - to build better models
    lengths_temp<-sapply(histData$observations$temperature,FUN="length")
    lengths_prec<-sapply(histData$observations$precipitation,FUN="length")
    lengths_press<-sapply(histData$observations$pressure,FUN="length")
    lengths_wind<-sapply(histData$observations$wind_speed,FUN="length")
    lengths_hum<-sapply(histData$observations$humidity,FUN="length")
    select_cons_weather=which(lengths==96 & lengths_temp==48 & lengths_prec==48 & lengths_press==48 & lengths_wind==48 & lengths_hum==48)
    
    temp=matrix(unlist(histData$observations$temperature[select_cons_weather]),byrow=TRUE,ncol=48)
    prec=matrix(unlist(histData$observations$precipitation[select_cons_weather]),byrow=TRUE,ncol=48)
    press=matrix(unlist(histData$observations$pressure[select_cons_weather]),byrow=TRUE,ncol=48)
    wind=matrix(unlist(histData$observations$wind_speed[select_cons_weather]),byrow=TRUE,ncol=48)
    hum=matrix(unlist(histData$observations$humidity[select_cons_weather]),byrow=TRUE,ncol=48)
    
    train_data_FULL=data.frame(clust=clus_km_clusters[select_cons_weather], y=year_obs[select_cons_weather],m=month_obs[select_cons_weather],d=day_obs[select_cons_weather],
                               free=free_day_obs[select_cons_weather],dw=week_day_obs[select_cons_weather],
                               temp=temp,prec=prec,press=press,wind=wind,hum=hum)
    
    pred_model<-list("class_model","centroids","MAPE","WAPE","train_data","train_data_FULL","min_max_weather")
    pred_model[["class_model"]]<-model_rand_forest
    pred_model[["centroids"]]<-cent_mat
    pred_model[["MAPE"]]<-MAPE
    pred_model[["WAPE"]]<-WAPE
    pred_model[["train_data"]]<-train_data
    pred_model[["train_data_FULL"]]<-train_data_FULL
    pred_model[["min_max_weather"]]<-min_max_weather
    return(pred_model)
  }else if (modelType == 'NN'){
    lengths<-sapply(histData$consumptions$values,FUN="length")
    select_cons=which(lengths==96)
    Consumptions_clean<-matrix(unlist(histData$consumptions$values[select_cons]),byrow=TRUE,ncol=96)
    
    temp_obs<-sapply(histData$observations$temperature[select_cons], "mean")
    prec_obs<-sapply(histData$observations$precipitation[select_cons], "mean")
    press_obs<-sapply(histData$observations$pressure[select_cons], "mean")
    wind_obs<-sapply(histData$observations$wind_speed[select_cons], "mean")
    humid_obs<-sapply(histData$observations$humidity[select_cons], "mean")
    
    year_obs<-as.numeric(substr(histData$dateInfo$date[select_cons],1,4))
    month_obs<-as.numeric(substr(histData$dateInfo$date[select_cons],6,7))
    day_obs<-as.numeric(substr(histData$dateInfo$date[select_cons],9,10))
    free_day_obs<-as.numeric(histData$dateInfo$freeDay[select_cons])
    
    days_ind=c(1:7)
    names(days_ind)=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    
    week_day_obs=as.numeric(days_ind[histData$dateInfo$dayName[select_cons]])
    
    #we delete days for which weather data is not complete
    train_data<-data.frame(temp=temp_obs,prec=prec_obs,press=press_obs,wind=wind_obs,hum=humid_obs,y=year_obs,m=month_obs,d=day_obs,free=free_day_obs,dw=week_day_obs)
    train_data_na_row<-unique(which(is.na(train_data)==TRUE,TRUE)[,1])
    train_data_keep_row=setdiff(c(1:dim(train_data)[1]),train_data_na_row)
    train_data<-train_data[train_data_keep_row,]
    Consumptions_clean<-Consumptions_clean[train_data_keep_row,]
    select_cons<-select_cons[train_data_keep_row]
    
    if (dim(Consumptions_clean)[1] < varNOOfClusters){
      pred_model<-c()
      return(pred_model)     
    }
    
    clus_km <- kmeans(Consumptions_clean, centers = varNOOfClusters)
    clus_km_clusters <- clus_km$cluster
    train_data<-cbind(clus_km_clusters,train_data)
    
    #transform training data
    train_data$clus_km_clusters <- as.factor(train_data$clus_km_clusters)
    train_data$d <- as.factor(train_data$d)
    train_data$dw <- as.factor(train_data$dw)
    train_data$m <- as.factor(train_data$m)
    train_data$y <- NULL
    
    train_data <- train_data %>% dplyr::mutate(
      temp = (temp-min(temp))/(max(temp) - min(temp)),
      prec = (prec-min(prec))/(max(prec) - min(prec)),
      press = (press-min(press))/(max(press) - min(press)),
      wind = (wind-min(wind))/(max(wind) - min(wind)),
      hum = (hum-min(hum))/(max(hum) - min(hum))
    )
    
    
    min_max_weather=matrix(0,nrow = 2,ncol=5)
    colnames(min_max_weather)<-c("temp","prec","press","wind","hum")
    rownames(min_max_weather)<-c("min","max")
    
    min_max_weather["min",]=c(min(train_data[,"temp"]),min(train_data[,"prec"]),min(train_data[,"press"]),min(train_data[,"wind"]),min(train_data[,"hum"]))
    min_max_weather["max",]=c(max(train_data[,"temp"]),max(train_data[,"prec"]),max(train_data[,"press"]),max(train_data[,"wind"]),max(train_data[,"hum"]))
    
    train_data <- train_data %>% dplyr::mutate(
      temp = (temp-min(temp))/(max(temp) - min(temp)),
      prec = (prec-min(prec))/(max(prec) - min(prec)),
      press = (press-min(press))/(max(press) - min(press)),
      wind = (wind-min(wind))/(max(wind) - min(wind)),
      hum = (hum-min(hum))/(max(hum) - min(hum))
    )  
    
    #build prediction model - neural networks
    #build prediction model - feed-forward neural networks
    model_nnet <- nnet(clus_km_clusters~., data = train_data,
                       size=12, decay=1.0e-5, maxit=10000)
    
    cent_mat<-matrix(0,varNOOfClusters,96)
    for(i in c(1:varNOOfClusters)){
      clust_points = Consumptions_clean[train_data$clus_km_clusters==i,]
      if (is.matrix(clust_points)){
        cent_mat[i,]<-colMeans(clust_points)
      } else {
        cent_mat[i,]<-clust_points
      }
    }
    
    pred_clusters <- max.col(model_nnet$fitted.values, 'first')
    MAPE_nnet=1/96*rowSums(abs(Consumptions_clean-cent_mat[pred_clusters,])/Consumptions_clean)
    MAPE_clustering=1/96*rowSums(abs(Consumptions_clean-cent_mat[train_data$clus_km_clusters,])/Consumptions_clean)
    MAPE<-data.frame(MAPE_nnet,MAPE_clustering,row.names = histData$dateInfo$date[select_cons])
    WAPE_rand_forest=rowSums(abs(Consumptions_clean-cent_mat[pred_clusters,]))/rowSums(Consumptions_clean)
    WAPE_clustering=rowSums(abs(Consumptions_clean-cent_mat[train_data$clus_km_clusters,]))/rowSums(Consumptions_clean)
    WAPE<-data.frame(WAPE_rand_forest,WAPE_clustering,row.names = histData$dateInfo$date[select_cons])
    
    # full training data - to build better models
    
    lengths_temp<-sapply(histData$observations$temperature,FUN="length")
    lengths_prec<-sapply(histData$observations$precipitation,FUN="length")
    lengths_press<-sapply(histData$observations$pressure,FUN="length")
    lengths_wind<-sapply(histData$observations$wind_speed,FUN="length")
    lengths_hum<-sapply(histData$observations$humidity,FUN="length")
    select_cons_weather=which(lengths==96 & lengths_temp==48 & lengths_prec==48 & lengths_press==48 & lengths_wind==48 & lengths_hum==48)
    
    temp=matrix(unlist(histData$observations$temperature[select_cons_weather]),byrow=TRUE,ncol=48)
    prec=matrix(unlist(histData$observations$precipitation[select_cons_weather]),byrow=TRUE,ncol=48)
    press=matrix(unlist(histData$observations$pressure[select_cons_weather]),byrow=TRUE,ncol=48)
    wind=matrix(unlist(histData$observations$wind_speed[select_cons_weather]),byrow=TRUE,ncol=48)
    hum=matrix(unlist(histData$observations$humidity[select_cons_weather]),byrow=TRUE,ncol=48)
    
    train_data_FULL=data.frame(clust=clus_km_clusters[select_cons_weather], y=year_obs[select_cons_weather],m=month_obs[select_cons_weather],d=day_obs[select_cons_weather],
                               free=free_day_obs[select_cons_weather],dw=week_day_obs[select_cons_weather],
                               temp=temp,prec=prec,press=press,wind=wind,hum=hum)
    
    pred_model<-list("class_model","centroids","MAPE","WAPE","train_data")#,"train_data_FULL")
    pred_model[["class_model"]]<-model_nnet
    pred_model[["centroids"]]<-cent_mat
    pred_model[["MAPE"]]<-MAPE
    pred_model[["WAPE"]]<-WAPE
    pred_model[["train_data"]]<-train_data
    #    pred_model[["train_data_FULL"]]<-train_data_FULL
    
    return(pred_model)
  }
}