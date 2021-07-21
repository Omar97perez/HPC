############################################ 
######### VERSION 2 OF GLM MODEL ###########
############################################


# packages
require(plyr)
require(dplyr)
require(tidyr)
require(lubridate)
require(ggplot2)

f_create_model_GLM_api_v2 <- function(histData){
  # This function returns model for 15 min based predictions for the customer specified with histData based on GLM for each 15 min time interval
  # output: - list of 96 GLM models for weekdays and 96 GLM models for free days, MAPE data, info data
  # input:  - histData, corresponding to particular consumer
  
  # split the incoming data into 3 different datasets
  # split into 3 different datasets

  # first we split into three different tables (based on the orignial mongoDB split) (NOSQL type)
  historicData_consumptions <- histData$consumptions
  historicData_dateinfo <- histData$dateInfo
  historicData_observations <- histData$observations
  
  # MAPE data template
  MAPEdata <- data.frame(
    date = c(),
    MAPE = c()
  )
  
  # iterate through each time point
  for (ntime in 1:96){
    
    # change the names to match previous development
    data_consumption <- historicData_consumptions
    data_dateinfo <- historicData_dateinfo
    data_observations <- historicData_observations
    
    # get the right data from the consumption data (include only data with enough datapoints)
    data_consumption <- data_consumption %>% mutate(
      tmp_col = unlist(lapply(values,length))
    ) %>% filter(tmp_col >= 96) %>% select(-tmp_col)
    
    data_consumption <- data_consumption %>% mutate(
      consumption_values = unlist(lapply(values, '[[', ntime)),
      date = as.Date(date),
      datetime = as.POSIXct(datetime)
    ) %>% select(-values)
    
    # dateinfo data 
    data_dateinfo <- data_dateinfo %>% mutate(
      date = as.Date(date),
      dayName = as.factor(dayName),
      holidayName = as.factor(holidayName),
      monthNum = as.factor(monthNum),
      weekOfMonth = as.factor(weekOfMonth),
    )
    
    # join with consumption data
    data_consumption <- data_consumption %>% left_join(data_dateinfo, by = c('date'='date'))
    
    tmp_index <- as.integer((ntime +1)/2)
    
    # observation data (weather parameters)
    data_observations <- data_observations %>% mutate(
      tmp_col = unlist(lapply(humidity,length)) + unlist(lapply(precipitation,length)) + unlist(lapply(temperature,length)) + 
        unlist(lapply(wind_speed,length)) + unlist(lapply(pressure,length)) 
    ) %>% filter(tmp_col >= 48*5) %>% select(-tmp_col)
    
    data_observations <- data_observations %>% mutate(
      date = as.Date(date),
      datetime = as.POSIXct(datetime),
      humidity = unlist(lapply(humidity,'[[', tmp_index)),
      precipitation = unlist(lapply(precipitation,'[[', tmp_index)),
      temperature = unlist(lapply(temperature,'[[', tmp_index)),
      wind_speed = unlist(lapply(wind_speed,'[[', tmp_index)),
      pressure = unlist(lapply(pressure,'[[', tmp_index)),
    ) %>% select(-index)
    
    # join with consumption data
    data_consumption <- data_consumption %>% left_join(data_observations, by = c('date' = 'date'))
    
    # check if previus day is HOLIDAY or if next day is HOLIDAY (not just free day, but an actual holiday) 
    data_consumption$prevDayHoliday <- c(NA, (!data_consumption$holidayName == "")[1:nrow(data_consumption)-1])
    data_consumption$nextDayHoliday <- c((!data_consumption$holidayName == "")[2:nrow(data_consumption)-1], NA)
    
    # reduce the dataset to relevant columns
    data_consumption <- data_consumption %>% select(
      date, consumption_values, dayName, freeDay, holidayName, monthNum, yearNum, precipitation, pressure, temperature, prevDayHoliday, nextDayHoliday
    )
    
    # change the year number to a factor variable
    data_consumption <- data_consumption %>% mutate(
      yearNum = as.factor(yearNum)
    )
    data_consumption_workdays <- data_consumption %>% filter(! (freeDay == TRUE | dayName %in% c("Saturday", "Sunday")) )
    
    # filter out NA rows
    data_consumption_workdays <- data_consumption_workdays %>% drop_na()
    
    # get previus 5 workday consumption values
    data_consumption_workdays <- data_consumption_workdays %>% mutate(
      consumption1 = c(NA,NA, head(consumption_values, length(consumption_values)-2)),
      consumption2 = c(NA,NA,NA, head(consumption_values, length(consumption_values)-3)),
      consumption3 = c(NA,NA,NA,NA, head(consumption_values, length(consumption_values)-4)),
      consumption4 = c(NA,NA,NA,NA,NA, head(consumption_values, length(consumption_values)-5)),
      consumption5 = c(NA,NA,NA,NA,NA,NA, head(consumption_values, length(consumption_values)-6))
    ) %>% ungroup()
    
    # filter out NA rows
    data_consumption_workdays <- data_consumption_workdays %>% drop_na()
    
    # temperature ^2
    data_consumption_workdays$temperature2 <- data_consumption_workdays$temperature^2
    
    data_consumption_workdays <- data_consumption_workdays %>% mutate(
      summerHoliday = ifelse(month(date) == 7 & day(date) %in% c(22,23,24,25,26,27,28), TRUE, FALSE),
      newyearHoliday = ifelse(month(date) == 12 & day(date) %in% c(24,27,28,29,30,31), TRUE, FALSE)
    )
    
    data_forpred <- data_consumption_workdays %>% select(
      consumption_values, precipitation, pressure, temperature, temperature2, dayName, yearNum, prevDayHoliday, nextDayHoliday, 
      summerHoliday, newyearHoliday, consumption1, consumption2, consumption3, consumption4, consumption5
    )
    
    # glm model (use all predictive variables from prediction data)
    glmmodel <- glm(consumption_values ~ ., data = data_forpred)
    
    # generate model predictions
    model_predictions <- ifelse(glmmodel$fitted.values > 0 , glmmodel$fitted.values, 0)
    
    # generate MAPE data
    MAPE <- ifelse(data_forpred$consumption_values > 0 , abs((model_predictions - data_forpred$consumption_values) /data_forpred$consumption_values), 0)

    MAPEdata_tmp <- data.frame(date = data_consumption_workdays$date, 
                               MAPE = MAPE)
    MAPEdata <- MAPEdata %>% rbind(MAPEdata_tmp)
    
  }
  
  # aggregate MAPE data by date
  MAPEdata <- MAPEdata %>% dplyr::group_by(date) %>% dplyr::summarise(MAPE = mean(MAPE))
  
  # find the largest 10% of MAPEs and find patterns
  largeMAPEdata <- MAPEdata %>% filter(MAPE > quantile(MAPEdata$MAPE,prob=1-10/100))
  
  date_occ <- as.Date(paste(ifelse(day(largeMAPEdata$date)/10 < 1, paste0('0',day(largeMAPEdata$date)), day(largeMAPEdata$date)),
                            ifelse(month(largeMAPEdata$date)/10 < 1, paste0('0',month(largeMAPEdata$date)), month(largeMAPEdata$date)), '2010'), format = "%d %m %Y")
  date_occ_unique <- unique(date_occ)
  
  dist_mat <- dist(date_occ, method = 'euclidean')
  a <- hclust(dist_mat)
  
  tmpdf <- data.frame(date = date_occ,
                      label = cutree(a, h = 7)) %>% left_join(table(date_occ) %>% as.data.frame() %>% mutate(
                        date_occ = as.Date(date_occ)
                      ), by = c("date" = "date_occ"))
  
  uniq_labels <- unique(tmpdf$label)
  groups_list <- list()
  i <- 1
  for (u in uniq_labels){
    ttmpdf <- tmpdf %>% filter(label == u)
    if ( (nrow(ttmpdf) > 1) & max(ttmpdf$Freq) > 1){
      groups_list[[i]] <- list(m = month(unique(ttmpdf$date)), d = day(unique(ttmpdf$date)))
      i <- i + 1
    }
    
  }
  
  
  ###########################################
  # iterate again
  
  # list of all models, MAPE data template (start from scratch), info data
  all_models <- list()
  MAPEdata <- data.frame(
    date = c(),
    MAPE = c()
  )
  
  # iterate thorugh timepoints
  for (ntime in 1:96){
    
    data_consumption <- historicData_consumptions
    data_dateinfo <- historicData_dateinfo
    data_observations <- historicData_observations
    data_consumption <- data_consumption %>% mutate(
      tmp_col = unlist(lapply(values,length))
    ) %>% filter(tmp_col >= 96) %>% select(-tmp_col)
    data_consumption <- data_consumption %>% mutate(
      consumption_values = unlist(lapply(values, '[[', ntime)),
      date = as.Date(date),
      datetime = as.POSIXct(datetime)
    ) %>% select(-values)
    
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
    
    # check if previus day is HOLIDAY or if next day is HOLIDAY (not just free day, but an actual holiday) 
    data_consumption$prevDayHoliday <- c(NA, (!data_consumption$holidayName == "")[1:nrow(data_consumption)-1])
    data_consumption$nextDayHoliday <- c((!data_consumption$holidayName == "")[2:nrow(data_consumption)-1], NA)
    
    # reduce the dataset to relevant columns
    data_consumption <- data_consumption %>% select(
      date, consumption_values, dayName, freeDay, holidayName, monthNum, yearNum, precipitation, pressure, temperature, prevDayHoliday, nextDayHoliday
    )
    
    # change the year number to a factor variable
    data_consumption <- data_consumption %>% mutate(
      yearNum = as.factor(yearNum)
    )
    data_consumption_workdays <- data_consumption %>% filter(! (freeDay == TRUE | dayName %in% c("Saturday", "Sunday")) )

    # get previus 5 workday consumption values
    data_consumption_workdays <- data_consumption_workdays %>% mutate(
      consumption1 = c(NA,NA, head(consumption_values, length(consumption_values)-2)),
      consumption2 = c(NA,NA,NA, head(consumption_values, length(consumption_values)-3)),
      consumption3 = c(NA,NA,NA,NA, head(consumption_values, length(consumption_values)-4)),
      consumption4 = c(NA,NA,NA,NA,NA, head(consumption_values, length(consumption_values)-5)),
      consumption5 = c(NA,NA,NA,NA,NA,NA, head(consumption_values, length(consumption_values)-6))
    ) %>% ungroup()
    
    # filter out NA rows
    data_consumption_workdays <- data_consumption_workdays %>% drop_na()
    
    # temperature ^2
    data_consumption_workdays$temperature2 <- data_consumption_workdays$temperature^2
    
    # improve prediction data with special indicators 
    j <- i
    for (g in groups_list){
      monthnum <- g["m"]$m
      daynum <- g["d"]$d
      data_consumption_workdays[paste0("specialGroup",j)] <- ifelse( (month(data_consumption_workdays$date) %in% monthnum) &
                                                                       (day(data_consumption_workdays$date) %in% daynum), T, F)
      j <- j+1
    }
    
    # remove unnecesary params
    data_forpred <- data_consumption_workdays %>% select(
      -c("date", "freeDay", "holidayName", "monthNum")
    )
    
    # glm model (use all predictive variables from prediction data and add extra formulated variables)
    glmmodel <- glm(consumption_values ~ ., data = data_forpred)
    all_models[[ntime]] <- glmmodel
    
    model_predictions <- ifelse(glmmodel$fitted.values > 0 , glmmodel$fitted.values, 0)
    MAPE <- ifelse(data_forpred$consumption_values > 0 , abs((model_predictions - data_forpred$consumption_values) /data_forpred$consumption_values), 0)
    MAPEdata_tmp <- data.frame(date = data_consumption_workdays$date, 
                               MAPE = MAPE)
    MAPEdata <- MAPEdata %>% rbind(MAPEdata_tmp)
  }
  
  # aggregate MAPE data by date
  MAPEdata <- MAPEdata %>% dplyr::group_by(date) %>% dplyr::summarise(MAPE = mean(MAPE))
  
  
  # free days modelling
  MAPEdata_freedays <- data.frame(
    date = c(),
    MAPE = c()
  )
  
  for (ntime in 1:96){
    # change the names to match previous development
    data_consumption <- historicData_consumptions
    data_dateinfo <- historicData_dateinfo
    data_observations <- historicData_observations
    
    data_consumption <- data_consumption %>% mutate(
      tmp_col = unlist(lapply(values,length))
    ) %>% filter(tmp_col >= 96) %>% select(-tmp_col)
    data_consumption <- data_consumption %>% mutate(
      consumption_values = unlist(lapply(values, '[[', ntime)),
      date = as.Date(date),
      datetime = as.POSIXct(datetime)
    ) %>% select(-values)

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
    
    data_observations <- data_observations %>% mutate(
      date = as.Date(date),
      datetime = as.POSIXct(datetime),
      humidity = unlist(lapply(humidity,'[[', tmp_index)),
      precipitation = unlist(lapply(precipitation,'[[', tmp_index)),
      temperature = unlist(lapply(temperature,'[[', tmp_index)),
      wind_speed = unlist(lapply(wind_speed,'[[', tmp_index)),
      pressure = unlist(lapply(pressure,'[[', tmp_index)),
    ) %>% select(-index)
    
    
    # check if previus day is HOLIDAY or if next day is HOLIDAY (not just free day, but an actual holiday) 
    data_consumption$prevDayHoliday <- c(NA, (!data_consumption$holidayName == "")[1:nrow(data_consumption)-1])
    data_consumption$nextDayHoliday <- c((!data_consumption$holidayName == "")[2:nrow(data_consumption)-1], NA)
    
    data_consumption <- data_consumption %>% left_join(data_observations, by = c('date' = 'date'))
    
    # reduce the dataset to relevant columns
    data_consumption <- data_consumption %>% select(
      date, consumption_values, dayName, freeDay, holidayName, monthNum, yearNum, precipitation, pressure, temperature,
      prevDayHoliday, nextDayHoliday
    )
    
    # change the year number to a factor variable
    data_consumption <- data_consumption %>% mutate(
      yearNum = as.factor(yearNum)
    )
    data_consumption_freedays <- data_consumption %>% filter((freeDay == TRUE | dayName %in% c("Saturday", "Sunday")))
    
    # get previus 5 workday consumption values
    data_consumption_freedays <- data_consumption_freedays %>% mutate(
      consumption1 = c(NA,NA, head(consumption_values, length(consumption_values)-2)),
      consumption2 = c(NA,NA,NA, head(consumption_values, length(consumption_values)-3))
    ) %>% ungroup()
    
    # filter out NA rows
    data_consumption_freedays <- data_consumption_freedays %>% drop_na()
    
    # temperature ^2
    data_consumption_freedays$temperature2 <- data_consumption_freedays$temperature^2
    
    data_consumption_freedays <- data_consumption_freedays %>% mutate(
      dayName = ifelse(dayName == "Saturday" | dayName == "Sunday", "Weekend", "Other")
    )
    
    data_forpred <- data_consumption_freedays %>% select(
      -c("date", "freeDay", "holidayName", "monthNum")
    )
    
    # glm model (use all predictive variables from prediction data and add extra formulated variables)
    glmmodel <- glm(consumption_values ~ ., data = data_forpred)
    
    MAPE <- ifelse(data_forpred$consumption_values > 0 , abs((glmmodel$fitted.values - data_forpred$consumption_values) /data_forpred$consumption_values), 0)
    # see where we missed by more than 1
    MAPEdata_freedays_tmp <- data.frame(date = data_consumption_freedays$date, 
                               MAPE = MAPE)
    MAPEdata_freedays <- MAPEdata_freedays %>% rbind(MAPEdata_freedays_tmp)
  }
  
  MAPEdata_freedays <- MAPEdata_freedays %>% dplyr::group_by(date) %>% dplyr::summarise(MAPE = mean(MAPE))
  
  # MAPE data
  MAPEdata <- MAPEdata %>% rbind(MAPEdata_freedays)
  
  # info data
  info <- list(first_train_day = min(historicData_consumptions$date), 
               last_train_day = max(historicData_consumptions$date), 
               measuringpoint = historicData_observations$title[1])
  
  all_models_total <- list(workdays_model = all_models,
                           freeday_model = all_models_freedays)

  return(list(full_model = all_models_total, MAPE = MAPEdata, info = info))
}
