# packages
require(plyr)
require(dplyr)
require(tidyr)
require(lubridate)

# function calculating a LR model for one time (i.e 00:15, 00:30, etc...)
# iterate over all times to get the complete set of models

LMforOne <- function(data_consumption, 
                     data_dateinfo, 
                     data_observations, ntime = 1){
  
  #Calculate the LR model and predict for 31 days in advance (mongoDB data and consumer3)
  
  # consumptions
  data_consumption <- data_consumption %>% mutate(
    tmp_col = unlist(lapply(values,length))
  ) %>% filter(tmp_col >= 96) %>% select(-tmp_col)
  
  data_consumption <- data_consumption %>% mutate(
    consumption_values = unlist(lapply(values, '[[', ntime)),
    date = as.Date(date),
    datetime = as.POSIXct(datetime)
  ) %>% select(-values)
  
  # dateindex
  data_dateinfo <- data_dateinfo %>% mutate(
    date = as.Date(date),
    dayName = as.factor(dayName),
    holidayName = as.factor(holidayName),
    monthNum = as.factor(monthNum),
    weekOfMonth = as.factor(weekOfMonth),
  )
  data_consumption <- data_consumption %>% left_join(data_dateinfo, by = c('date'='date'))
  
  tmp_index <- as.integer((ntime +1)/2)
  
  data_observations <- data_observations %>% mutate(
    tmp_col = unlist(lapply(humidity,length)) + unlist(lapply(precipitation,length)) + unlist(lapply(temperature,length)) + 
      unlist(lapply(wind_speed,length)) + unlist(lapply(pressure,length)) 
  ) %>% filter(tmp_col >= 48*5) %>% select(-tmp_col)
  
  if (nrow(data_observations)==0){
    return(list(model = c(), MAPE = c()))
  }
  data_observations <- data_observations %>% mutate(
    date = as.Date(date),
    datetime = as.POSIXct(datetime),
    humidity = unlist(lapply(humidity,'[[', tmp_index)),
    precipitation = unlist(lapply(precipitation,'[[', tmp_index)),
    temperature = unlist(lapply(temperature,'[[', tmp_index)),
    wind_speed = unlist(lapply(wind_speed,'[[', tmp_index)),
    pressure = unlist(lapply(pressure,'[[', tmp_index)),
  ) %>% select(-index)
  
  
  data_consumption <- data_consumption %>% left_join(data_observations, by = c('date' = 'date'))
  data_consumption <- data_consumption %>% mutate(
    consumption1 = c(NA, head(consumption_values, length(consumption_values)-1)),
    consumption2 = c(NA,NA, head(consumption_values, length(consumption_values)-2)),
    consumption3 = c(NA,NA,NA, head(consumption_values, length(consumption_values)-3)),
    consumption4 = c(NA,NA,NA,NA, head(consumption_values, length(consumption_values)-4)),
    consumption5 = c(NA,NA,NA,NA,NA, head(consumption_values, length(consumption_values)-5)),
    consumption6 = c(NA,NA,NA,NA,NA,NA, head(consumption_values, length(consumption_values)-6)),
    consumption7 = c(NA,NA,NA,NA,NA,NA,NA, head(consumption_values, length(consumption_values)-7))
  ) %>% ungroup()
  # filter out the new NAs
  data_consumption <- data_consumption %>% filter(!is.na(consumption7))
  
  # select the max of 2 years (most recent) data (in regard to data drift)
  data_consumption <- data_consumption[max(nrow(data_consumption) - 732, 1): nrow(data_consumption),]

  # LR model
  # priprava podatkov
  data_for_pred <- data_consumption %>% mutate(
    dayName = factor(ifelse(dayName %in% c('Monday','Tuesday','Wednesday','Thursday','Friday') & !freeDay, 'Weekday',
                               ifelse(dayName %in% c('Monday','Tuesday','Wednesday','Thursday','Friday') & freeDay, 'FreeWeekDay', 'Weekend')),levels=c("FreeWeekDay","Weekday","Weekend")),
    monthNum = factor(ifelse(monthNum %in% c(12,1,2),'Winter',ifelse(monthNum %in% c(3,4,5),'Spring',
                                                                        ifelse(monthNum %in% c(6,7,8), 'Summer','Autumn'))),levels=c('Winter','Spring','Summer','Autumn'))
  ) %>% select(
    -title,
    -datetime.x,
    -datetime.y,
    -date,
    -dayNum,
    -holidayName,
    -index,
    -weekOfMonth,
    -wind_speed,
    -humidity,
    -yearNum,
    -dayHours,
    -freeDay
  ) 
  
  data_for_pred <- data_for_pred[complete.cases(data_for_pred),]
  
  check_factor_levels <- function(x){
    # checks factor levels and remove potential character columns which should not be there!
    if (is.factor(x)){
      if (length(levels(factor(as.character(x)))) > 1){
        return(TRUE)
      }else{
        return(FALSE)
      }
    } else if (is.character(x)){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  
  data_for_pred <- data_for_pred[,sapply(data_for_pred, check_factor_levels)]
  LRmodel <- glm(consumption_values ~ .+I(temperature^2), data = data_for_pred, family = 'gaussian')
  
  # hack da dobimo se date, ki ga potrebujemo
  data_consumption_fordate <- data_consumption[complete.cases(data_consumption),]
  
  info <- list(first_train_day = min(data_consumption_fordate$date), 
               last_train_day = max(data_consumption_fordate$date), 
               measuringpoint = data_observations$title[1])
  
  data_for_MAPE <- data_for_pred %>% mutate(
    date = data_consumption_fordate$date,
    predicted_consumption_values = ifelse(LRmodel$fitted.values > 0, LRmodel$fitted.values, 0)
  ) %>% select(date, consumption_values,predicted_consumption_values) %>% mutate(
    MAPE = ifelse(consumption_values > 0, abs(consumption_values - predicted_consumption_values)/consumption_values, 0)
  )
  return(list(model = LRmodel, MAPE = data_for_MAPE, info = info))
}

# function that iterates through all models
getAllModels <- function(data_consumption, 
                         data_dateinfo, 
                         data_observations){
  
  # creating a full model from 96 individual models
  all_models <- list()
  small_models <- list()
  all_info <- list()
  
  # returning the values for the first time (00:15)
  i <- 1
  result_forOne <- LMforOne(data_consumption = data_consumption, 
                            data_dateinfo = data_dateinfo, 
                            data_observations = data_observations,
                            ntime = i)
  
  all_models[[i]] <- result_forOne[["model"]]
  data_for_MAPE <- result_forOne[["MAPE"]]
  first_train_day <- result_forOne[["info"]][["first_train_day"]]
  last_train_day <- result_forOne[["info"]][["last_train_day"]]
  measuringpoint <- result_forOne[["info"]][["measuringpoint"]]
  for (i in 2:96){
    result_forOne <- LMforOne(data_consumption = data_consumption, 
                              data_dateinfo = data_dateinfo, 
                              data_observations = data_observations,
                              ntime = i)
    all_models[[i]] <- result_forOne[["model"]]
    data_for_MAPE <- data_for_MAPE %>% rbind(result_forOne[["MAPE"]])
    # change in min-max train day if there is some data discrepancy
    all_info[[i]] <- result_forOne[["info"]]
    if (all_info[[i]][["first_train_day"]] < first_train_day){
      first_train_day <- all_info[[i]][["first_train_day"]]
    }
    if (all_info[[i]][["last_train_day"]] > last_train_day){
      last_train_day <- all_info[[i]][["last_train_day"]]
    }
  }
  if (length(data_for_MAPE$MAPE)==0){
    print("Warning: Incomplete data... Could not create models for all 96 models. Some values are missing in the training dataset.")
    return(c())
  }
  
  # MAPE results
  data_MAPE <- data_for_MAPE %>% dplyr::group_by(date) %>% dplyr::summarise(MAPE = mean(MAPE))
  for (i in 1:96){
    small_models[[i]] <-  all_models[[i]]$coefficients
  }
  # creating info data
  info <- list(first_train_day = first_train_day, 
          last_train_day = last_train_day, 
          measuringpoint = measuringpoint
  )
  return(list(full_model = all_models, small_model = small_models, MAPE = data_MAPE, info = info))
}



f_create_model_GLM_api <- function(histData){
  # This function returns model for 15 min based predictions for the customer specified with histData based on GLM for each 15 min time interval
  # output: - list of 96 GLM models
  #         - a prediction model for prediciting the clusters using class_method
  # input:  - histData, corresponding to particular consumer
  #  source('C:/Users/jpovh/Documents/RCNM/CRRI/Projekti/2019/3TAV-Eureka/Matic Rogar/MagNaloga/code/mongoDB_model_return.r')
  
  historicData <- histData
  # first we split into three different tables (based on the orignial mongoDB split) (NOSQL type)
  historicData_consumptions <- historicData$consumptions
  historicData_dateinfo <- historicData$dateInfo
  historicData_observations <- historicData$observations
  all_LM_models_and_MAPE <- getAllModels(historicData_consumptions,historicData_dateinfo,historicData_observations)
  all_LM_models_and_MAPE$info$created_timestamp<-now()
  return(all_LM_models_and_MAPE)
}


