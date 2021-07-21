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


f_create_model_clust_api_new<- function(histData, varNOOfClusters = 15, modelType =  'RF'){
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
  # change 13 10 2020: improved NN modelling (M Rogar)
  # change 22 10 2020: removed "train_data_FULL" from the output
  # change 23 10 2020: fixed bug: to prevent empty centroids
  # change 5 11 2020: integration with Matic Rogar code					
  # change 15 4 2021: added meta data about the model
  if (modelType == 'RF'){
    
    # data manipulation: using only complete historic observations
    lengths<-sapply(histData$consumptions$values,FUN="length")
    ConSums<-sapply(histData$consumptions$values,FUN="sum")
    select_cons=which(lengths==96 & ConSums)
    Consumptions_clean<-matrix(unlist(histData$consumptions$values[select_cons]),byrow=TRUE,ncol=96)
    
    temp_obs<-sapply(histData$observations$temperature[select_cons], "mean")
    temp_var<-sapply(histData$observations$temperature[select_cons], "var")
    prec_obs<-sapply(histData$observations$precipitation[select_cons], "mean")
    prec_var<-sapply(histData$observations$precipitation[select_cons], "var")
    wind_obs<-sapply(histData$observations$wind_speed[select_cons], "mean")
    wind_var<-sapply(histData$observations$wind_speed[select_cons], "var")
    humid_obs<-sapply(histData$observations$humidity[select_cons], "mean")
    press_obs<-sapply(histData$observations$pressure[select_cons], "mean")
    
    year_obs<-as.numeric(substr(histData$dateInfo$date[select_cons],1,4))
    month_obs<-as.numeric(substr(histData$dateInfo$date[select_cons],6,7))
    day_obs<-as.numeric(substr(histData$dateInfo$date[select_cons],9,10))
    free_day_obs<-as.numeric(histData$dateInfo$freeDay[select_cons])
    
    days_ind=c(1:7)
    names(days_ind)=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    
    week_day_obs=as.numeric(days_ind[histData$dateInfo$dayName[select_cons]])
    
    # consumption manipulation (we use the previous values for future predictions)
    consump_mean <- apply(Consumptions_clean, 1, mean)
    consump_var <- apply(Consumptions_clean, 1, var)
    # if not enough test cases	
    if (dim(Consumptions_clean)[1] < varNOOfClusters){
      pred_model<-c()
      return(pred_model)     
    }
    
    
    
    # we use previous 7 values of mean and var of daily consumption (also delete days for which weather and consumption data is not complete)
    train_data<-data.frame(temp=temp_obs,
                           temp_var=temp_var,
                           prec=prec_obs,
                           prec_var=prec_var,
                           press=press_obs,
                           wind=wind_obs,
                           wind_var=wind_var,
                           hum=humid_obs,
                           y=year_obs,
                           m=month_obs,
                           d=day_obs,
                           free=free_day_obs,
                           dw=week_day_obs,
                           consump1_mean=c(NA,consump_mean[1:length(consump_mean)-1]),
                           consump1_var=c(NA,consump_var[1:length(consump_var)-1]),
                           consump2_mean=c(NA,NA,consump_mean[2:length(consump_mean)-2]),
                           consump2_var=c(NA,NA,consump_var[2:length(consump_var)-2]),
                           consump3_mean=c(NA,NA,NA,consump_mean[3:length(consump_mean)-3]),
                           consump3_var=c(NA,NA,NA,consump_var[3:length(consump_var)-3]),
                           consump4_mean=c(NA,NA,NA,NA,consump_mean[4:length(consump_mean)-4]),
                           consump4_var=c(NA,NA,NA,NA,consump_var[4:length(consump_var)-4]),
                           consump5_mean=c(NA,NA,NA,NA,NA,consump_mean[5:length(consump_mean)-5]),
                           consump5_var=c(NA,NA,NA,NA,NA,consump_var[5:length(consump_var)-5]),
                           consump6_mean=c(NA,NA,NA,NA,NA,NA,consump_mean[6:length(consump_mean)-6]),
                           consump6_var=c(NA,NA,NA,NA,NA,NA,consump_var[6:length(consump_var)-6]),
                           consump7_mean=c(NA,NA,NA,NA,NA,NA,NA,consump_mean[7:length(consump_mean)-7]),
                           consump7_var=c(NA,NA,NA,NA,NA,NA,NA,consump_var[7:length(consump_var)-7])
    )
    
    Consumptions_clean<-Consumptions_clean[complete.cases(train_data),]
    select_cons<-select_cons[complete.cases(train_data)]
    train_data<-train_data[complete.cases(train_data),]
    
    # clustering of daily 15min daily consumption using KMeans on train data
    set.seed(2021)
    clus_km <- kmeans(Consumptions_clean, centers = varNOOfClusters)
    clus_km_clusters <- clus_km$cluster
    train_data$clus_km_clusters<-as.factor(clus_km_clusters)
    
    # transform training data (factors and normalization)
    #train_data$clus_km_clusters <- as.factor(train_data$clus_km_clusters)
    train_data$d <- factor(train_data$d,levels = as.character(1:31))
    train_data$dw <- factor(train_data$dw,levels = as.character(1:7))
    train_data$m <- factor(train_data$m,levels = as.character(1:12))
    train_data$free <- factor(train_data$free,levels = as.character(0:1))
    train_data$y <- NULL
    
    
    # this is needed for output, to keep track of original intervals														
    min_max_weather=matrix(0,nrow = 2,ncol=22)
    colnames(min_max_weather)<-c("temp","temp_var","prec","prec_var","press","wind","wind_var","hum",
                                 "consump1_mean","consump1_var","consump2_mean","consump2_var","consump3_mean","consump3_var",
                                 "consump4_mean","consump4_var","consump5_mean","consump5_var","consump6_mean","consump6_var",
                                 "consump7_mean","consump7_var")
    rownames(min_max_weather)<-c("min","max")
    
    min_max_weather["min",]=c(min(train_data[,"temp"]),min(train_data[,"temp_var"]),min(train_data[,"prec"]),min(train_data[,"prec_var"]),
                              min(train_data[,"press"]),min(train_data[,"wind"]),min(train_data[,"wind_var"]),min(train_data[,"hum"]),
                              min(train_data[,"consump1_mean"]),min(train_data[,"consump1_var"]),min(train_data[,"consump2_mean"]),min(train_data[,"consump2_var"]),
                              min(train_data[,"consump3_mean"]),min(train_data[,"consump3_var"]),min(train_data[,"consump4_mean"]),min(train_data[,"consump4_var"]),
                              min(train_data[,"consump5_mean"]),min(train_data[,"consump5_var"]),min(train_data[,"consump6_mean"]),min(train_data[,"consump6_var"]),
                              min(train_data[,"consump7_mean"]),min(train_data[,"consump7_var"]))
    min_max_weather["max",]=c(max(train_data[,"temp"]),max(train_data[,"temp_var"]),max(train_data[,"prec"]),max(train_data[,"prec_var"]),
                              max(train_data[,"press"]),max(train_data[,"wind"]),max(train_data[,"wind_var"]),max(train_data[,"hum"]),
                              max(train_data[,"consump1_mean"]),max(train_data[,"consump1_var"]),max(train_data[,"consump2_mean"]),max(train_data[,"consump2_var"]),
                              max(train_data[,"consump3_mean"]),max(train_data[,"consump3_var"]),max(train_data[,"consump4_mean"]),max(train_data[,"consump4_var"]),
                              max(train_data[,"consump5_mean"]),max(train_data[,"consump5_var"]),max(train_data[,"consump6_mean"]),max(train_data[,"consump6_var"]),
                              max(train_data[,"consump7_mean"]),max(train_data[,"consump7_var"]))
    
    train_data <- train_data %>% dplyr::mutate(
      temp = range01(temp),
      temp_var = range01(temp_var),
      prec = range01(prec),
      prec_var = range01(prec_var),
      wind = range01(wind),
      wind_var = range01(wind_var),
      press = range01(press),
      hum = range01(hum),
      consump1_mean = range01(consump1_mean),
      consump1_var = range01(consump1_var),
      consump2_mean = range01(consump2_mean),
      consump2_var = range01(consump2_var),
      consump3_mean = range01(consump3_mean),
      consump3_var = range01(consump3_var),
      consump4_mean = range01(consump4_mean),
      consump4_var = range01(consump4_var),
      consump5_mean = range01(consump5_mean),
      consump5_var = range01(consump5_var),
      consump6_mean = range01(consump6_mean),
      consump6_var = range01(consump6_var),
      consump7_mean = range01(consump7_mean),
      consump7_var = range01(consump7_var)
    ) 
    
    # RandomForest
    model_rand_forest <- randomForest(clus_km_clusters ~ ., data=train_data,na.action=na.omit,importance=TRUE,ntree=50)
    
    pred_results = list()
    for (i in 1:varNOOfClusters){
      tmp_train_data_index = train_data$clus_km_clusters == i
      tmp_train_data <- train_data[tmp_train_data_index,]
      tmp_model_pred <- model_rand_forest$predicted[tmp_train_data_index]
      pred_results[[i]] <- list(results = sum(tmp_train_data$clus_km_clusters == tmp_model_pred)/length(tmp_train_data$clus_km_clusters),
                                clust_center = clus_km$centers[i,])
    }
    pred_results[['total']] <- sum(train_data$clus_km_clusters == model_rand_forest$predicted)/length(train_data$clus_km_clusters)
    pred_results[['real_vs_predicted']] <- data.frame(real = train_data$clus_km_clusters, predicted = as.vector(model_rand_forest$predicted))
    
    # Using predictions and real cluster labels to extract cluster centroids for consumption predictions
    cent_mat<-clus_km$centers
    
    pred_clusters<-model_rand_forest$predicted
    #    rowMin=apply(abs(Consumptions_clean), 1, FUN=min)
    #    Mape_rows=which(rowMin>0)
    #    Mape_rows_s=which(rowSums(Consumptions_clean)>0)
    #    MAPE_rand_forest=1/96*rowSums(abs(Consumptions_clean[Mape_rows,]-cent_mat[pred_clusters[Mape_rows],])/Consumptions_clean[Mape_rows,])
    #    MAPE_clustering=1/96*rowSums(abs(Consumptions_clean[Mape_rows,]-cent_mat[train_data$clus_km_clusters[Mape_rows],])/Consumptions_clean[Mape_rows,])
    #    MAPE<-data.frame(MAPE_rand_forest,MAPE_clustering,row.names = histData$dateInfo$date[select_cons[Mape_rows]])
    #    WAPE_rand_forest=rowSums(abs(Consumptions_clean[Mape_rows_s,]-cent_mat[pred_clusters[Mape_rows_s],]))/rowSums(Consumptions_clean[Mape_rows_s,])
    #    WAPE_clustering=rowSums(abs(Consumptions_clean[Mape_rows_s,]-cent_mat[train_data$clus_km_clusters[Mape_rows_s],]))/rowSums(Consumptions_clean[Mape_rows_s,])
    #    WAPE<-data.frame(WAPE_rand_forest,WAPE_clustering,row.names = histData$dateInfo$date[select_cons[Mape_rows_s]])
    
    mm_rf<-mape(cent_mat[pred_clusters,],Consumptions_clean)
    mm_clust<-mape(cent_mat[train_data$clus_km_clusters,],Consumptions_clean)
    if (isempty(mm_rf$MAPE$mape_rows)){
      MAPE<-data.frame("MAPE_rand_forest"=mm_rf$MAPE$MAPE,"MAPE_clustering"=mm_clust$MAPE$MAPE)
    } else {
      MAPE<-data.frame("MAPE_rand_forest"=mm_rf$MAPE$MAPE,"MAPE_clustering"=mm_clust$MAPE$MAPE,row.names = histData$dateInfo$date[select_cons[mm_rf$MAPE$mape_rows]])
    }
    if (isempty(mm_rf$WAPE$wape_rows)){
      WAPE<-data.frame("WAPE_rand_forest"=mm_rf$WAPE$WAPE,"WAPE_clustering"=mm_clust$WAPE$WAPE)
    } else {
      WAPE<-data.frame("WAPE_rand_forest"=mm_rf$WAPE$WAPE,"WAPE_clustering"=mm_clust$WAPE$WAPE,row.names = histData$dateInfo$date[select_cons[mm_rf$WAPE$wape_rows]])
    }
    
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
    
    pred_model<-list("class_model","centroids","MAPE","WAPE","min_max_weather","info")
    model_rand_forest$err.rate <- NULL
    model_rand_forest$confusion <- NULL
    model_rand_forest$votes <- NULL
    
    pred_model[["class_model"]]<-model_rand_forest
    pred_model[["centroids"]]<-cent_mat
    pred_model[["MAPE"]]<-MAPE
    pred_model[["WAPE"]]<-WAPE
    pred_model[["min_max_weather"]]<-min_max_weather
    pred_model[["info"]]<- list(first_train_day = histData$dateInfo$date[1], 
                                last_train_day = histData$dateInfo$date[length(histData$dateInfo$date)], 
                                measuringpoint = histData$observations$title[1])
    
    # returning the pred_model consisting of training data, model and MAPE and WAPE values
    return(pred_model)
    
  }else if (modelType == 'NN'){
    if (varNOOfClusters > 10){
      varNOOfClusters=10
    }
    # data manipulation
    lengths<-sapply(histData$consumptions$values,FUN="length")
    ConSums<-sapply(histData$consumptions$values,FUN="sum")
    select_cons=which(lengths==96 & ConSums)
    Consumptions_clean<-matrix(unlist(histData$consumptions$values[select_cons]),byrow=TRUE,ncol=96)
    
    temp_obs<-sapply(histData$observations$temperature[select_cons], "mean")
    temp_var<-sapply(histData$observations$temperature[select_cons], "var")
    prec_obs<-sapply(histData$observations$precipitation[select_cons], "mean")
    prec_var<-sapply(histData$observations$precipitation[select_cons], "var")
    wind_obs<-sapply(histData$observations$wind_speed[select_cons], "mean")
    wind_var<-sapply(histData$observations$wind_speed[select_cons], "var")
    humid_obs<-sapply(histData$observations$humidity[select_cons], "mean")
    press_obs<-sapply(histData$observations$pressure[select_cons], "mean")
    
    year_obs<-as.numeric(substr(histData$dateInfo$date[select_cons],1,4))
    month_obs<-as.numeric(substr(histData$dateInfo$date[select_cons],6,7))
    day_obs<-as.numeric(substr(histData$dateInfo$date[select_cons],9,10))
    free_day_obs<-as.numeric(histData$dateInfo$freeDay[select_cons])
    
    days_ind=c(1:7)
    names(days_ind)=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    
    week_day_obs=as.numeric(days_ind[histData$dateInfo$dayName[select_cons]])
    
    # consumption manipulation (we use the previous one for predictions)
    consump_mean <- apply(Consumptions_clean, 1, mean)
    consump_var <- apply(Consumptions_clean, 1, var)
    
    #Day_Consumptions=t(as.matrix(sapply(Consumptions_clean, `[[`, "values")))
    
    
    #we delete days for which weather data is not complete
    train_data<-data.frame(temp=temp_obs,
                           temp_var=temp_var,
                           prec=prec_obs,
                           prec_var=prec_var,
                           press=press_obs,
                           wind=wind_obs,
                           wind_var=wind_var,
                           hum=humid_obs,
                           y=year_obs,
                           m=month_obs,
                           d=day_obs,
                           free=free_day_obs,
                           dw=week_day_obs,
                           consump1_mean=c(NA,consump_mean[1:length(consump_mean)-1]),
                           consump1_var=c(NA,consump_var[1:length(consump_var)-1]),
                           consump2_mean=c(NA,NA,consump_mean[2:length(consump_mean)-2]),
                           consump2_var=c(NA,NA,consump_var[2:length(consump_var)-2]),
                           consump3_mean=c(NA,NA,NA,consump_mean[3:length(consump_mean)-3]),
                           consump3_var=c(NA,NA,NA,consump_var[3:length(consump_var)-3]),
                           consump4_mean=c(NA,NA,NA,NA,consump_mean[4:length(consump_mean)-4]),
                           consump4_var=c(NA,NA,NA,NA,consump_var[4:length(consump_var)-4]),
                           consump5_mean=c(NA,NA,NA,NA,NA,consump_mean[5:length(consump_mean)-5]),
                           consump5_var=c(NA,NA,NA,NA,NA,consump_var[5:length(consump_var)-5]),
                           consump6_mean=c(NA,NA,NA,NA,NA,NA,consump_mean[6:length(consump_mean)-6]),
                           consump6_var=c(NA,NA,NA,NA,NA,NA,consump_var[6:length(consump_var)-6]),
                           consump7_mean=c(NA,NA,NA,NA,NA,NA,NA,consump_mean[7:length(consump_mean)-7]),
                           consump7_var=c(NA,NA,NA,NA,NA,NA,NA,consump_var[7:length(consump_var)-7])
    )
    
    Consumptions_clean<-Consumptions_clean[complete.cases(train_data),]
    select_cons<-select_cons[complete.cases(train_data)]
    train_data<-train_data[complete.cases(train_data),]
    
    # clustering of daily 15min daily consumption using KMeans on train data
    set.seed(2021)
    clus_km <- kmeans(Consumptions_clean, centers = varNOOfClusters)
    clus_km_clusters <- clus_km$cluster
    train_data$clus_km_clusters<-as.factor(clus_km_clusters)
    
    # transform training data (factors and normalization)
    train_data$d <- factor(train_data$d,levels = as.character(1:31))
    train_data$dw <- factor(train_data$dw,levels = as.character(1:7))
    train_data$m <- factor(train_data$m,levels = as.character(1:12))
    train_data$free <- factor(train_data$free,levels = as.character(0:1))
    train_data$y <- NULL
    
    
    min_max_weather=matrix(0,nrow = 2,ncol=22)
    colnames(min_max_weather)<-c("temp","temp_var","prec","prec_var","press","wind","wind_var","hum",
                                 "consump1_mean","consump1_var","consump2_mean","consump2_var","consump3_mean","consump3_var",
                                 "consump4_mean","consump4_var","consump5_mean","consump5_var","consump6_mean","consump6_var",
                                 "consump7_mean","consump7_var")
    rownames(min_max_weather)<-c("min","max")
    
    min_max_weather["min",]=c(min(train_data[,"temp"]),min(train_data[,"temp_var"]),min(train_data[,"prec"]),min(train_data[,"prec_var"]),
                              min(train_data[,"press"]),min(train_data[,"wind"]),min(train_data[,"wind_var"]),min(train_data[,"hum"]),
                              min(train_data[,"consump1_mean"]),min(train_data[,"consump1_var"]),min(train_data[,"consump2_mean"]),min(train_data[,"consump2_var"]),
                              min(train_data[,"consump3_mean"]),min(train_data[,"consump3_var"]),min(train_data[,"consump4_mean"]),min(train_data[,"consump4_var"]),
                              min(train_data[,"consump5_mean"]),min(train_data[,"consump5_var"]),min(train_data[,"consump6_mean"]),min(train_data[,"consump6_var"]),
                              min(train_data[,"consump7_mean"]),min(train_data[,"consump7_var"]))
    min_max_weather["max",]=c(max(train_data[,"temp"]),max(train_data[,"temp_var"]),max(train_data[,"prec"]),max(train_data[,"prec_var"]),
                              max(train_data[,"press"]),max(train_data[,"wind"]),max(train_data[,"wind_var"]),max(train_data[,"hum"]),
                              max(train_data[,"consump1_mean"]),max(train_data[,"consump1_var"]),max(train_data[,"consump2_mean"]),max(train_data[,"consump2_var"]),
                              max(train_data[,"consump3_mean"]),max(train_data[,"consump3_var"]),max(train_data[,"consump4_mean"]),max(train_data[,"consump4_var"]),
                              max(train_data[,"consump5_mean"]),max(train_data[,"consump5_var"]),max(train_data[,"consump6_mean"]),max(train_data[,"consump6_var"]),
                              max(train_data[,"consump7_mean"]),max(train_data[,"consump7_var"]))
    
    train_data <- train_data %>% dplyr::mutate(
      temp = range01(temp),
      temp_var = range01(temp_var),
      prec = range01(prec),
      prec_var = range01(prec_var),
      wind = range01(wind),
      wind_var = range01(wind_var),
      press = range01(press),
      hum = range01(hum),
      consump1_mean = range01(consump1_mean),
      consump1_var = range01(consump1_var),
      consump2_mean = range01(consump2_mean),
      consump2_var = range01(consump2_var),
      consump3_mean = range01(consump3_mean),
      consump3_var = range01(consump3_var),
      consump4_mean = range01(consump4_mean),
      consump4_var = range01(consump4_var),
      consump5_mean = range01(consump5_mean),
      consump5_var = range01(consump5_var),
      consump6_mean = range01(consump6_mean),
      consump6_var = range01(consump6_var),
      consump7_mean = range01(consump7_mean),
      consump7_var = range01(consump7_var)
    ) 
    
    # NeuralNetworks
    model_nnet <- nnet(clus_km_clusters~., data = train_data,
                       size=12, decay=1.0e-5, maxit=12000)
    
    # Using predictions and real cluster labels to extract cluster centroids for consumption predictions
    cent_mat<-matrix(0,varNOOfClusters,96)
    for(i in c(1:varNOOfClusters)){
      curr_clusters=which(train_data$clus_km_clusters==i)
      if(!isempty(curr_clusters)){
        if (length(curr_clusters)==1){
          cent_mat[i,]<-Consumptions_clean[curr_clusters,]
        } else {
          cent_mat[i,]<-colMeans(Consumptions_clean[curr_clusters,])
        }
      }
    }
    
    pred_clusters <- max.col(model_nnet$fitted.values, 'first')
    
    #MAPE_nnet=1/96*rowSums(abs(Consumptions_clean-cent_mat[pred_clusters,])/Consumptions_clean)
    #MAPE_clustering=1/96*rowSums(abs(Consumptions_clean-cent_mat[train_data$clus_km_clusters,])/Consumptions_clean)
    #MAPE<-data.frame(MAPE_nnet,MAPE_clustering,row.names = histData$dateInfo$date[select_cons])
    #WAPE_rand_forest=rowSums(abs(Consumptions_clean-cent_mat[pred_clusters,]))/rowSums(Consumptions_clean)
    #WAPE_clustering=rowSums(abs(Consumptions_clean-cent_mat[train_data$clus_km_clusters,]))/rowSums(Consumptions_clean)
    #WAPE<-data.frame(WAPE_rand_forest,WAPE_clustering,row.names = histData$dateInfo$date[select_cons])
    
    
    mm_nnet<-mape(cent_mat[pred_clusters,],Consumptions_clean)
    mm_clust<-mape(cent_mat[train_data$clus_km_clusters,],Consumptions_clean)
    if (isempty(mm_nnet$MAPE$mape_rows)){
      MAPE<-data.frame("MAPE_nnet"=mm_nnet$MAPE$MAPE,"MAPE_clustering"=mm_clust$MAPE$MAPE)
    } else {
      MAPE<-data.frame("MAPE_nnet"=mm_nnet$MAPE$MAPE,"MAPE_clustering"=mm_clust$MAPE$MAPE,row.names = histData$dateInfo$date[select_cons[mm_nnet$MAPE$mape_rows]])
    }
    if (isempty(mm_nnet$WAPE$wape_rows)){
      WAPE<-data.frame("WAPE_rand_forest"=mm_nnet$WAPE$WAPE,"WAPE_clustering"=mm_clust$WAPE$WAPE)
    } else {
      WAPE<-data.frame("WAPE_rand_forest"=mm_nnet$WAPE$WAPE,"WAPE_clustering"=mm_clust$WAPE$WAPE,row.names = histData$dateInfo$date[select_cons[mm_nnet$WAPE$wape_rows]])
    }
    
    
    
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
    
    pred_model<-list("class_model","centroids","MAPE","WAPE","min_max_weather","info")
    # postavimo nepotrebne parametre modela na NULL
    model_nnet$fitted.values <- NULL
    model_nnet$residuals <- NULL
    model_nnet$nunits <- NULL
    model_nnet$value <- NULL
    
    # shranimo rezultate
    pred_model[["class_model"]]<-model_nnet
    pred_model[["centroids"]]<-cent_mat
    pred_model[["MAPE"]]<-MAPE
    pred_model[["WAPE"]]<-WAPE
    pred_model[["min_max_weather"]]<-min_max_weather
    pred_model[["info"]]<- list(first_train_day = histData$dateInfo$date[1], 
                                last_train_day = histData$dateInfo$date[length(histData$dateInfo$date)], 
                                measuringpoint = histData$observations$title[1])
    

    return(pred_model)
  }
}