#load sources
rm(list = ls())

pwd <- "C:/Users/jpovh/Documents/RCNM/CRRI/Projekti/2019/3TAV-Eureka/Khyati/Electricity Consumption Project/GitHubFolder/3_TAV_Eureka/"
models_filepath=paste0(pwd,"Data/Data_Output/Prediction_Models/")
setwd(pwd)
source('Rscripts/Libraries.R')
source('Rscripts/f_create_model_clust_api.R')
source('Rscripts/f_prediction_with_clusterig_api.R')
source('Rscripts/f_getData.R')
source('Rscripts/f_create_model_GLM_api.R')
source('Rscripts/f_prediction_with_GLM_api.R')

library(randomForest)
library(jsonlite)
library(RColorBrewer)
library(caret)
library(psych)
# Zagon preko pravega R strežnika
# http://192.168.190.21:8000/forecastapi/__swagger__/
#* @apiTitle Prediction API

varConsumerIDs = c("2-1603","2-9629","2-183237","3-9641")
first_day="2017-01-01"
last_day="2019-12-31"
first_forecast_day= "2020-06-20" 
last_forecast_day= "2020-07-06" 
NofClusters=15
clustering_method="KMeans"
classification_method="rpart"

predDates<-as.factor(as.Date(as.Date(first_forecast_day):as.Date(last_forecast_day), origin="1970-01-01"))
MAPE=matrix(0,ncol = length(varConsumerIDs),nrow=length(predDates))
row.names(MAPE)<-predDates
colnames(MAPE)<-varConsumerIDs
MAPE_GLM<-MAPE
# MAPE_train_data contains MAPEs for training data set for each consumer
MAPE_train_data<-vector(mode = "list", length = length(varConsumerIDs))
names(MAPE_train_data)<-varConsumerIDs
WAPE_train_data<-vector(mode = "list", length = length(varConsumerIDs))
names(WAPE_train_data)<-varConsumerIDs



ConfM=list()  #list of conf matrices
ACC=c()
varConsumerIDin=0
fromMemory=1;
for (varConsumerID in varConsumerIDs[1]){
  if(fromMemory){
    load(file=paste0(pwd,'/data/Data_Output/consumer_',varConsumerID,'_data.RData'))
  } else {
    histData<-f_getHistoricData(var_custID = varConsumerID, date_from = first_day,date_to = last_day,var_location='Ljubljana')
    save(histData,file=paste0(pwd,'/Data/Data_Output/consumer_',varConsumerID,'_data.RData'))
  }
  #generiramo model s pridobljenmi podatki
  set.seed(1)
  pred_model <- f_create_model_clust_api(histData,  varNOOfClusters = NofClusters)
  pred_model_GLM <- f_create_model_GLM_api(histData)
  MAPE_train_data[[varConsumerID]]<-pred_model$MAPE
  WAPE_train_data[[varConsumerID]]<-pred_model$WAPE
  ObsPred=cbind(pred_model$class_model$y,round(pred_model$class_model$predicted,0))
  varConsumerIDin=varConsumerIDin+1
  ConfM[[varConsumerIDin]]=table(factor(ObsPred[,1],levels=1:NofClusters),
                                           factor(ObsPred[,2],levels=1:NofClusters))
                                                 
  ACC[varConsumerIDin]=tr(as.matrix(ConfM[[varConsumerIDin]]))/sum(sum(ConfM[[varConsumerIDin]]))
  #shranimo model
  save(pred_model, file = paste0(models_filepath,varConsumerID,"_model_",NofClusters,"_centroids_",clustering_method,".RData"))
  #naložimo model
  #model<-load(file=paste0(models_filepath,varConsumerID,"_model_",varNOOfClusters,"_centroids_",clustering_method,".RData"))
  
  for (prediction_date_ind in c(10:length(predDates))){
    #predictionData<-f_getForecastData(day_forecast=predDates[prediction_date_ind])
    # pridobimo napovedi iz ZGODOVINSKIH podatkov za dolocen dan - uporabimo za datume pred 1.4.2020
    predictionData<-f_getForecastEvalData(day_forecast=predDates[prediction_date_ind])
    prediction <- f_prediction_with_clusterig_api(predictionData, pred_model)
    consData<-f_getHistoricData(var_custID = varConsumerID, date_from = predDates[prediction_date_ind-8],date_to=predDates[prediction_date_ind])
    #save(predictionData, consData,pred_model_GLM,file = paste0('C:/Users/jpovh/Documents/RCNM/CRRI/Projekti/2019/3TAV-Eureka/Matic Rogar/',"Input_Data_for_GLM_ap.RData"))
#    prediction1 <- f_prediction_with_GLM_api(predictionData,consData,pred_model_GLM)
    
#    pred_model_GLM["all_models"][1]
    prediction1 <- f_prediction_with_GLM_api(predictionData,consData,pred_model_GLM["all_models"][[1]])
    real_consupmtion<-consData$consumptions$values[[nrow(consData$dateInfo)]]
    MAPE[prediction_date_ind,varConsumerID]= sum(abs(prediction$predicted_consumption-real_consupmtion))/sum(real_consupmtion)
    MAPE_GLM[prediction_date_ind,varConsumerID]= sum(abs(prediction1$predictions-real_consupmtion))/sum(real_consupmtion)  
  }
  outputfile=paste0(pwd,'/Data/Data_Output/Predictions/Evaluations/MAPE_',NofClusters,'_',clustering_method="KMeans",'_',classification_method,".txt")
  write.table(MAPE, file=outputfile, row.names=TRUE, col.names=TRUE)
}

#Summarise Mape, Wape
MAPE_summary<-vector(mode = "list", length = length(varConsumerIDs))
names(MAPE_summary)<-varConsumerIDs
WAPE_summary<-vector(mode = "list", length = length(varConsumerIDs))
names(WAPE_summary)<-varConsumerIDs

for (i in varConsumerIDs){
  MT<-as.matrix(MAPE_train_data[[i]])
  MT_keep <- is.finite(rowSums(MT))
  WT<-as.matrix(WAPE_train_data[[i]])
  WT_keep <- is.finite(rowSums(WT))
  MAPE_summary[[i]]<-colMeans(MT[MT_keep,],na.rm = TRUE)
  WAPE_summary[[i]]<-colMeans(WT[WT_keep,],na.rm = TRUE)
}

#visualise MAPE
col_list=brewer.pal(length(varConsumerIDs), "Paired")
names(col_list)=varConsumerIDs
path_fig=paste0(pwd,'/Data/Data_Output/Predictions/Evaluations/fig_MAPE_',NofClusters,'_',clustering_method="KMeans",'_',classification_method,"_",first_forecast_day,"_",last_forecast_day,".pdf")
pdf(path_fig)
xlabels<-as.factor(predDates)
lgd=c()
par(mar=c(5,6,4,1)+.0001)
title=paste0("MAPE for ",varConsumerIDs[1]," and ", varConsumerIDs[2])
plot(c(0,length(predDates)), c(0,2),yaxt='n',xaxt='n',main=title, type = "n", pch=19,cex.lab=1.6, cex.axis=1.3, cex.main=1.5,xlab = "DAY",ylab = "MAPE")  # setting up coord. system
#ylim=c(0,0.8),cex.axis=1.3,cex.lab=1.6,cex.main=1.8,lwd=2)
#axis(side=1,at=ind_ax,xlim=xlabels,ylim=c(0,0.45))
date_ind=c(1,10,20,30,40,50,60)
axis(side=1,at=date_ind,labels=xlabels[date_ind])
axis(side=2,at=c(1:10)*0.2,labels=as.factor(c(1:10)*0.2))
for (i in varConsumerIDs){
  points(as.numeric(MAPE[,i]), col = col_list[i], cex = 1,pch=16,type = "o")    
  lgd=c(lgd,paste0("cons.",i))
}
legend(3, 1.95, legend=lgd, pch=16*matrix(1,1,length(varConsumerIDs)),cex=1.6*matrix(1,1,length(varConsumerIDs)),col=col_list,ncol=2)
dev.off() 




#save data (eg for master student)
histData
save(histData,file=paste0(pwd,'/Data/Data_Output/consumer_',varConsumerID,'_data.RData'))
write.table(pred_model$train_data,file="C:/Users/jpovh/Documents/RCNM/CRRI/Projekti/2019/3TAV-Eureka/Felix/Data_classif_2-183237.txt")

save(predictionData, file = paste0('C:/Users/jpovh/Documents/RCNM/CRRI/Projekti/2019/3TAV-Eureka/Matic Rogar/',"Data_for_Prediction.RData"))

