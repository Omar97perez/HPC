library(doParallel)
library(foreach)

numCores <- detectCores()
#load sources
rm(list = ls())
   
## Loading required package: iterators
registerDoParallel(numCores)  # use multicore, set to the number of our cores

#pwd <- "/Users/zeynepdundar/Desktop/Prace_Sohpc2021_Shape/"
pwd <- "C:/Users/janez/Documents/RCNM/CRRI/Projekti/2019/3TAV-Eureka/Prace_Shape/Prace_Sohpc2021_Shape/"


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
# diminish consumer id set
varConsumerIDs = c("2-1603")
#varConsumerIDs = c("2-1603","3-10582","3-10585","3-10921","4-4800","4-5016","6-7600","7-166094")
numCores <- detectCores()

first_day="2019-01-01"
last_day="2021-06-20"
first_forecast_day= "2021-06-22" 
last_forecast_day= "2021-07-05" 
NofClusters=15
clustering_method="KMeans"
classification_method="rpart"
fromMemory=1
varConsumerID_rel_ind=1000


fz <- function(varConsumerID){
  start_time = Sys.time()
  #varConsumerID_rel_ind=varConsumerID_rel_ind+1;
  #x=1000+varConsumerID;
  if(fromMemory){
    input_file=paste0(pwd,"Data/test_data_2021_07_06/hist_data_",varConsumerID,".json")
    load(file=input_file)
  } else {
    histData<-f_getHistoricData(var_custID = varConsumerID, date_from = first_day,date_to = last_day,var_location='Ljubljana')
    save(histData,file=paste0(pwd,'/Data/Data_Output/consumer_',varConsumerID,'_data.RData'))
  }
  set.seed(1)
  end_time = Sys.time()
  loadData = as.numeric(end_time, units = "secs") - as.numeric(start_time, units = "secs")
  print("Loading data: ")
  print(loadData)
  print("-------")
  print("...Generation of Models...")
  print("RF:")
  start_time = Sys.time()
  pred_model_clust_RF <- f_create_model_clust_api_new(histData,varNOOfClusters = NofClusters,modelType = 'RF')
  end_time = Sys.time()
  genRF <- as.numeric(end_time, units = "secs") - as.numeric(start_time, units = "secs")
  print(genRF)
  print("-")
  print("NN:")
  start_time = Sys.time()
  pred_model_clust_NN <- f_create_model_clust_api_new(histData,varNOOfClusters = NofClusters,modelType = 'NN',max_IT = 1000)
  end_time = Sys.time()
  genNN <- as.numeric(end_time, units = "secs") - as.numeric(start_time, units = "secs")
  print(genNN)
  print("-")
  
  print("GLM:")
  start_time = Sys.time()
  pred_model_GLM <- f_create_model_GLM_api(histData)
  end_time = Sys.time()
  genGLM <- as.numeric(end_time, units = "secs") - as.numeric(start_time, units = "secs")
  print(genGLM)
  print("-")
  
  print("GLM small:")
  start_time = Sys.time()
  pred_model_GLM_small=pred_model_GLM[["small_model"]]
  end_time = Sys.time()
  genGLMsm <- as.numeric(end_time, units = "secs") - as.numeric(start_time, units = "secs")
  print(genGLMsm)
  
  
  print("-------")
  print("...Saving Models...")
  
  print("RF:")
  start_time = Sys.time()
  save(pred_model_clust_RF, file = paste0(models_filepath,varConsumerID,"_model_RF_",NofClusters,"_centroids_",clustering_method,".RData"))
  end_time = Sys.time()
  savingRF <- as.numeric(end_time, units = "secs") - as.numeric(start_time, units = "secs")
  print(savingRF)
  print("-")
  
  print("NN:")
  start_time = Sys.time()
  save(pred_model_clust_NN, file = paste0(models_filepath,varConsumerID,"_model_NN_",NofClusters,"_centroids_",clustering_method,".RData"))
  end_time = Sys.time()
  savingNN <- as.numeric(end_time, units = "secs") - as.numeric(start_time, units = "secs")
  print(savingNN)
  print("-")
  
  print("GLM sm:")
  start_time = Sys.time()
  save(pred_model_GLM_small, file = paste0(models_filepath,varConsumerID,"_model_GLM",".RData"))
  end_time = Sys.time()
  savingGLM = as.numeric(end_time, units = "secs") - as.numeric(start_time, units = "secs")
  print(savingGLM)
  print("-------")
  
  # reduce into an element
  prediction_date_ind = 0
  pred_date=as.Date(first_forecast_day)+prediction_date_ind
  pred_file=paste0(pwd,"Data/test_data_2021_07_06/forecast_data_",varConsumerID,"_",pred_date,".json")
  
  print("...loading pred file...")
  start_time = Sys.time()
  load(pred_file)
  end_time = Sys.time()
  loadFile = as.numeric(end_time, units = "secs") - as.numeric(start_time, units = "secs")
  print(loadFile)
  
  print("------------------------------") 
  print("...prediction of the models...")
  print("GLM:")
  start_time = Sys.time()
  prediction_GLM <- f_prediction_with_GLM_api(forecastData,histData,pred_model_GLM_small,full_model = FALSE)
  end_time = Sys.time()
  predGLM = as.numeric(end_time, units = "secs") - as.numeric(start_time, units = "secs")
  print(predGLM)
  print("-")
  
  print("RF:")
  start_time = Sys.time()
  prediction_clust_RF <- f_prediction_with_RF_api_new(forecastData,histData,pred_model_clust_RF)
  end_time = Sys.time()
  predRF = as.numeric(end_time, units = "secs") - as.numeric(start_time, units = "secs")
  print(predRF)
  print("-")
  
  print("NN:")
  start_time = Sys.time()
  prediction_clust_NN <- f_prediction_with_NN_api_new(forecastData,histData,pred_model_clust_NN)
  end_time = Sys.time()
  predNN = as.numeric(end_time, units = "secs") - as.numeric(start_time, units = "secs")
  print(predNN)
}  
  
no_cores <- detectCores() - 1  
#cl <- makeCluster(no_cores, type="FORK")  

sys_time_par=system.time(
foreach (i=1:length(varConsumerIDs1)) %do% {
  print(varConsumerIDs1[i])
  fz(varConsumerIDs1[i])
}
)


sys_time_ser = system.time(
  for (i in c(1:length(varConsumerIDs1))){
    print(varConsumerIDs1[i])
    fz(varConsumerIDs1[i])
  }
)



