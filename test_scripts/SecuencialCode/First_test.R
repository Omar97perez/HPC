#load sources
rm(list = ls())

pwd <- "C:/Users/Omar/Desktop/Nueva carpeta (6)/Prace_Sohpc2021_Shape/"
models_filepath=paste0(pwd,"Data/Data_Output/Prediction_Models/")
setwd(pwd)
source('Rscripts/Libraries.R')
source('Rscripts/f_create_model_clust_api_new.R')
source('Rscripts/f_prediction_with_RF_api_new.R')
source('Rscripts/f_prediction_with_NN_api_new.R')
source('Rscripts/f_getData.R')
source('Rscripts/f_create_model_GLM_api.R')
source('Rscripts/f_prediction_with_GLM_api.R')
source('Rscripts/MAPE.R')

library(randomForest)
library(jsonlite)
library(RColorBrewer)
library(caret)
library(psych)


varConsumerIDs1 = c("1001","1002","1003","1004","1005","1006","1007","1008")
varConsumerIDs = c("2-1603","3-10582","3-10585","3-10921","4-4800","4-5016","6-7600","7-166094")

first_day="2019-01-01"
last_day="2021-05-30"
first_forecast_day= "2021-06-01" 
last_forecast_day= "2021-06-07" 
NofClusters=15
clustering_method="KMeans"
classification_method="rpart"
fromMemory=1

for (varConsumerID in varConsumerIDs){
  if(fromMemory){
    input_file=paste0(pwd,"Data/test_data_2021_06_30/hist_data_",varConsumerID,".json")
    load(file=input_file)
  } else {
    histData<-f_getHistoricData(var_custID = varConsumerID, date_from = first_day,date_to = last_day,var_location='Ljubljana')
    save(histData,file=paste0(pwd,'/Data/Data_Output/consumer_',varConsumerID,'_data.RData'))
  }
  set.seed(1)
  pred_model_clust_RF <- f_create_model_clust_api_new(histData,varNOOfClusters = NofClusters,modelType = 'RF')
  pred_model_clust_NN <- f_create_model_clust_api_new(histData,varNOOfClusters = NofClusters,modelType = 'NN')
  pred_model_GLM <- f_create_model_GLM_api(histData)
  pred_model_GLM_small=pred_model_GLM[["small_model"]]
  
  save(pred_model_clust_RF, file = paste0(models_filepath,varConsumerID,"_model_RF_",NofClusters,"_centroids_",clustering_method,".RData"))
  save(pred_model_clust_NN, file = paste0(models_filepath,varConsumerID,"_model_NN_",NofClusters,"_centroids_",clustering_method,".RData"))
  save(pred_model_GLM_small, file = paste0(models_filepath,varConsumerID,"_model_GLM",".RData"))
}

