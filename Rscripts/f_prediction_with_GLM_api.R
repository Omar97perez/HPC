# packages
require(plyr)
require(dplyr)
require(tidyr)
require(lubridate)


CheckIf96consumptions <- function(prevConsumption, ntime){
  consumption <- rep(0, nrow(prevConsumption))
  i <- 1
  for (line in prevConsumption$values){
    if (length(line) == 96){
      consumption[i] <- line[[ntime]]
      i <- i+1
    }else{
      consumption[i] <- line[[length(line)]]
      i <- i+1
    }
  }
  return(consumption)
}

predictOneTime <- function(forPredictions,
                           prevConsumption, 
                           LMmodels,
                           ntime,
                           nrow_tmp,
                           full_model = TRUE){
  
  # if we have a full model, use predict, otherwise we just use the coefficients in the right order to do predictions
  
  # first we check the coefficients... if we have a full model and not all seasons were present in training mode we revert to 
  # a small model ( Winter is presumed excluded, since one factor variable always is) 
  # remark: forcing new factor levels into full model only skews the results...
  if (full_model){
    full_LMmodel_1 <- LMmodels[[1]]
    season_missing <- F
    for (season in c("Winter","Spring","Summer",'Autumn')){
      if (! season %in% full_LMmodel_1$xlevels[["monthNum"]]){
        season_missing <- T
      }
    }
    if (season_missing){
      full_model <- F
      LMmodels_tmp <- list()
      i <- 1
      for (model in LMmodels){
        LMmodels_tmp[[i]] <- model$coefficients
        i <- i+1
      }
      LMmodels <- LMmodels_tmp
    }
  }
  
  
  if (full_model){
    # index to get the right weather data
    if (nrow(forPredictions) <= 8){
        index_ = as.integer((ntime-1)/12) + 1
        if (index_ > nrow_tmp){
          print(paste('Ni napovedi za cas',as.integer(ntime/4),'h in', ntime-as.integer(ntime/4)*4, 'min'))
        }else{
          forPredictions <- forPredictions[index_,]
          prev_Consumptions <- prevConsumption %>% mutate(
            consumption =  CheckIf96consumptions(prevConsumption, ntime)
          ) %>% .$consumption
          # manually add previous consumptions
          forPredictions$consumption1 <- prev_Consumptions[1]
          forPredictions$consumption2 <- prev_Consumptions[2]
          forPredictions$consumption3 <- prev_Consumptions[3]
          forPredictions$consumption4 <- prev_Consumptions[4]
          forPredictions$consumption5 <- prev_Consumptions[5]
          forPredictions$consumption6 <- prev_Consumptions[6]
          forPredictions$consumption7 <- prev_Consumptions[7]
  
          return(ifelse(predict(LMmodels[[ntime]], newdata = forPredictions) > 0, predict(LMmodels[[ntime]], newdata = forPredictions), 0))
        }
    }else{
      index_ = ntime
      forPredictions <- forPredictions[index_,]
      
        prev_Consumptions <- prevConsumption %>% mutate(
          consumption =  CheckIf96consumptions(prevConsumption, ntime)
        ) %>% .$consumption
        # manually add previous consumptions
        forPredictions$consumption1 <- prev_Consumptions[1]
        forPredictions$consumption2 <- prev_Consumptions[2]
        forPredictions$consumption3 <- prev_Consumptions[3]
        forPredictions$consumption4 <- prev_Consumptions[4]
        forPredictions$consumption5 <- prev_Consumptions[5]
        forPredictions$consumption6 <- prev_Consumptions[6]
        forPredictions$consumption7 <- prev_Consumptions[7]
      
      return(ifelse(predict(LMmodels[[ntime]], newdata = forPredictions) > 0, predict(LMmodels[[ntime]], newdata = forPredictions), 0))
    }
  }else{
      if (nrow(forPredictions) <= 8){
        index_ = as.integer((ntime-1)/12) + 1
        if (index_ > nrow_tmp){
          print(paste('Ni napovedi za cas',as.integer(ntime/4),'h in', ntime-as.integer(ntime/4)*4, 'min'))
        }else{
          forPredictions <- forPredictions[index_,]
          prev_Consumptions <- prevConsumption %>% mutate(
            consumption =  unlist(lapply(values,'[[', ntime))
          ) %>% .$consumption
          # manually add previous consumptions
          forPredictions$consumption1 <- prev_Consumptions[1]
          forPredictions$consumption2 <- prev_Consumptions[2]
          forPredictions$consumption3 <- prev_Consumptions[3]
          forPredictions$consumption4 <- prev_Consumptions[4]
          forPredictions$consumption5 <- prev_Consumptions[5]
          forPredictions$consumption6 <- prev_Consumptions[6]
          forPredictions$consumption7 <- prev_Consumptions[7]
          
          # tmp values needed to propery evaluate possible missing factor variables
          lm_dayNameWeekend <- is.na(LMmodels[[ntime]]['dayNameWeekend'])
          lm_dayNameWeekday <- is.na(LMmodels[[ntime]]['dayNameWeekday'])
          lm_dayNameFreeWeekDay <- is.na(LMmodels[[ntime]]['dayFreeWeekDay'])
          lm_monthNumSummer <- is.na(LMmodels[[ntime]]['monthNumSummer'])
          lm_monthNumWinter <- is.na(LMmodels[[ntime]]['monthNumWinter'])
          lm_monthNumAutumn <- is.na(LMmodels[[ntime]]['monthNumAutumn'])
          lm_monthNumSpring <- is.na(LMmodels[[ntime]]['monthNumSpring'])
          
          # check the sum of missing values (with regard that Winter is always the norm, i.e. the standard -> i.e one will always be missing and
          # if only one is its Winter
          if (sum(lm_monthNumSummer,lm_monthNumWinter,lm_monthNumAutumn,lm_monthNumSpring) >= 2){
            if (sum(lm_monthNumSummer,lm_monthNumWinter,lm_monthNumAutumn,lm_monthNumSpring) == 4){
              LMmodels[[ntime]]['monthNumSpring'] <- 0
              LMmodels[[ntime]]['monthNumSummer'] <- 0
              LMmodels[[ntime]]['monthNumAutumn'] <- 0
              LMmodels[[ntime]]['monthNumWinter'] <- 0
              lm_monthNumSpring <- F
              lm_monthNumSummer <- F
              lm_monthNumAutumn <- F
              lm_monthNumWinter <- F
            }else{
              if (lm_monthNumSpring){
                LMmodels[[ntime]]['monthNumSpring'] <- mean(c(LMmodels[[ntime]]['monthNumSummer'],
                                                              LMmodels[[ntime]]['monthNumAutumn'],
                                                              LMmodels[[ntime]]['monthNumWinter']),na.rm = T)
                lm_monthNumSpring <- F
              }
              
              if (lm_monthNumSummer) {
                LMmodels[[ntime]]['monthNumSummer'] <- mean(c(LMmodels[[ntime]]['monthNumSpring'],
                                                              LMmodels[[ntime]]['monthNumAutumn'],
                                                              LMmodels[[ntime]]['monthNumWinter']),na.rm = T)
                lm_monthNumSummer <- F
              }
              
              if (lm_monthNumAutumn) {
                LMmodels[[ntime]]['monthNumAutumn'] <- mean(c(LMmodels[[ntime]]['monthNumSpring'],
                                                              LMmodels[[ntime]]['monthNumSummer'],
                                                              LMmodels[[ntime]]['monthNumWinter']),na.rm = T)
                lm_monthNumAutumn <- F
              }
              
              if (lm_monthNumWinter) {
                LMmodels[[ntime]]['monthNumWinter'] <- mean(c(LMmodels[[ntime]]['monthNumSpring'],
                                                              LMmodels[[ntime]]['monthNumSummer'],
                                                              LMmodels[[ntime]]['monthNumAutumn']),na.rm = T)
                lm_monthNumWinter <- F
              }
            }
          }
          
          # create the predictions
          predictions <- LMmodels[[ntime]]['(Intercept)'] + 
            ifelse(lm_dayNameWeekend, 0, LMmodels[[ntime]]['dayNameWeekend'])*(as.integer(forPredictions['dayName'] == 'Weekend')) +
            ifelse(lm_dayNameWeekday, 0, LMmodels[[ntime]]['dayNameWeekday'])*(as.integer(forPredictions['dayName'] == 'Weekday')) +
            ifelse(lm_dayNameFreeWeekDay, 0, LMmodels[[ntime]]['dayNameFreeWeekDay'])*(as.integer(forPredictions['dayName'] == 'dayNameFreeWeekDay')) +
            ifelse(lm_monthNumSpring, 0, LMmodels[[ntime]]['monthNumSpring'])*(as.integer(forPredictions['monthNum'] == 'Spring')) +
            ifelse(lm_monthNumAutumn, 0, LMmodels[[ntime]]['monthNumAutumn'])*(as.integer(forPredictions['monthNum'] == 'Autumn')) +
            ifelse(lm_monthNumSummer, 0, LMmodels[[ntime]]['monthNumSummer'])*(as.integer(forPredictions['monthNum'] == 'Summer')) +
            ifelse(lm_monthNumWinter, 0, LMmodels[[ntime]]['monthNumWinter'])*(as.integer(forPredictions['monthNum'] == 'Winter')) +
            LMmodels[[ntime]]['precipitation']*forPredictions['precipitation'] + LMmodels[[ntime]]['pressure']*forPredictions['pressure'] + 
            LMmodels[[ntime]]['temperature']*forPredictions['temperature'] + 
            LMmodels[[ntime]]['consumption1']*forPredictions['consumption1'] + LMmodels[[ntime]]['consumption2']*forPredictions['consumption2'] +
            LMmodels[[ntime]]['consumption3']*forPredictions['consumption3'] + LMmodels[[ntime]]['consumption4']*forPredictions['consumption4'] +
            LMmodels[[ntime]]['consumption5']*forPredictions['consumption5'] + LMmodels[[ntime]]['consumption6']*forPredictions['consumption6'] +
            LMmodels[[ntime]]['consumption7']*forPredictions['consumption7'] + LMmodels[[ntime]]['I(temperature^2)']*forPredictions['temperature']^2
          colnames(predictions) <- 'Prediction'
        
        return(ifelse(predictions > 0 , predictions, 0))
        }
      }else{
        index_ = ntime
        forPredictions <- forPredictions[index_,]
        prev_Consumptions <- prevConsumption %>% mutate(
          consumption =  unlist(lapply(values,'[[', ntime))
        ) %>% .$consumption
        # manually add previous consumptions

        forPredictions$consumption1 <- prev_Consumptions[1]
        forPredictions$consumption2 <- prev_Consumptions[2]
        forPredictions$consumption3 <- prev_Consumptions[3]
        forPredictions$consumption4 <- prev_Consumptions[4]
        forPredictions$consumption5 <- prev_Consumptions[5]
        forPredictions$consumption6 <- prev_Consumptions[6]
        forPredictions$consumption7 <- prev_Consumptions[7]
        
        # tmp values needed to propery evaluate possible missing factor variables
        lm_dayNameWeekend <- is.na(LMmodels[[ntime]]['dayNameWeekend'])
        lm_dayNameWeekday <- is.na(LMmodels[[ntime]]['dayNameWeekday'])
        lm_dayNameFreeWeekDay <- is.na(LMmodels[[ntime]]['dayFreeWeekDay'])
        lm_monthNumSummer <- is.na(LMmodels[[ntime]]['monthNumSummer'])
        lm_monthNumWinter <- is.na(LMmodels[[ntime]]['monthNumWinter'])
        lm_monthNumAutumn <- is.na(LMmodels[[ntime]]['monthNumAutumn'])
        lm_monthNumSpring <- is.na(LMmodels[[ntime]]['monthNumSpring'])
        
        # check the sum of missing values (with regard that Winter is always the norm, i.e. the standard -> i.e one will always be missing and
        # if only one is its Winter
        if (sum(lm_monthNumSummer,lm_monthNumWinter,lm_monthNumAutumn,lm_monthNumSpring) >= 2){
          if (lm_monthNumSpring){
            LMmodels[[ntime]]['monthNumSpring'] <- mean(c(LMmodels[[ntime]]['monthNumSummer'],
                                                          LMmodels[[ntime]]['monthNumAutumn'],
                                                          LMmodels[[ntime]]['monthNumWinter']),na.rm = T)
            lm_monthNumSpring <- F
          }
          
          if (lm_monthNumSummer) {
            LMmodels[[ntime]]['monthNumSummer'] <- mean(c(LMmodels[[ntime]]['monthNumSpring'],
                                                          LMmodels[[ntime]]['monthNumAutumn'],
                                                          LMmodels[[ntime]]['monthNumWinter']),na.rm = T)
            lm_monthNumSummer <- F
          }
          
          if (lm_monthNumAutumn) {
            LMmodels[[ntime]]['monthNumAutumn'] <- mean(c(LMmodels[[ntime]]['monthNumSpring'],
                                                          LMmodels[[ntime]]['monthNumSummer'],
                                                          LMmodels[[ntime]]['monthNumWinter']),na.rm = T)
            lm_monthNumAutumn <- F
          }
        }
        
        # create the predictions
        predictions <- LMmodels[[ntime]]['(Intercept)'] + 
          ifelse(lm_dayNameWeekend, 0, LMmodels[[ntime]]['dayNameWeekend'])*(as.integer(forPredictions['dayName'] == 'Weekend')) +
          ifelse(lm_dayNameWeekday, 0, LMmodels[[ntime]]['dayNameWeekday'])*(as.integer(forPredictions['dayName'] == 'Weekday')) +
          ifelse(lm_dayNameFreeWeekDay, 0, LMmodels[[ntime]]['dayFreeWeekDay'])*(as.integer(forPredictions['dayName'] == 'dayFreeWeekDay')) +
          ifelse(lm_monthNumSpring, 0, LMmodels[[ntime]]['monthNumSpring'])*(as.integer(forPredictions['monthNum'] == 'Spring')) +
          ifelse(lm_monthNumAutumn, 0, LMmodels[[ntime]]['monthNumAutumn'])*(as.integer(forPredictions['monthNum'] == 'Autumn')) +
          ifelse(lm_monthNumSummer, 0, LMmodels[[ntime]]['monthNumSummer'])*(as.integer(forPredictions['monthNum'] == 'Summer')) +
          ifelse(lm_monthNumWinter, 0, LMmodels[[ntime]]['monthNumWinter'])*(as.integer(forPredictions['monthNum'] == 'Winter')) +
          LMmodels[[ntime]]['precipitation']*forPredictions['precipitation'] + LMmodels[[ntime]]['pressure']*forPredictions['pressure'] + 
          LMmodels[[ntime]]['temperature']*forPredictions['temperature'] + 
          LMmodels[[ntime]]['consumption1']*forPredictions['consumption1'] + LMmodels[[ntime]]['consumption2']*forPredictions['consumption2'] +
          LMmodels[[ntime]]['consumption3']*forPredictions['consumption3'] + LMmodels[[ntime]]['consumption4']*forPredictions['consumption4'] +
          LMmodels[[ntime]]['consumption5']*forPredictions['consumption5'] + LMmodels[[ntime]]['consumption6']*forPredictions['consumption6'] +
          LMmodels[[ntime]]['consumption7']*forPredictions['consumption7'] + LMmodels[[ntime]]['I(temperature^2)']*forPredictions['temperature']^2
        colnames(predictions) <- 'Prediction'
        
        return(ifelse(predictions > 0 , predictions, 0))
    }
  }
  
}

predictOneDay <- function(forPredictions,
                          prevConsumption, 
                          LMmodels,
                          full_model = TRUE){
  
  dateInfo_forPredictions <- forPredictions[[1]] %>% as.data.frame()
  
  nrow_tmp <- 8
  for (el in forPredictions[[2]]){
    if (length(el) > 1 & length(el) < nrow_tmp){
      nrow_tmp <- length(el)
    }
  }
  if (nrow_tmp < 8){
    print('Imamo manjkajoce podatke pri vremenski napovedi!')
    print(paste('Stolpec',el,'ima',nrow_tmp,'vrstic.'))
    
    forPredictions[[2]]$forecastByHours <- forPredictions[[2]]$forecastByHours[1:nrow_tmp]
    forPredictions[[2]]$humidity <- forPredictions[[2]]$humidity[1:nrow_tmp]
    forPredictions[[2]]$precipitation <- forPredictions[[2]]$precipitation[1:nrow_tmp]
    forPredictions[[2]]$pressure <- forPredictions[[2]]$pressure[1:nrow_tmp]
    forPredictions[[2]]$temperature <- forPredictions[[2]]$temperature[1:nrow_tmp]
    forPredictions[[2]]$wind_speed <- forPredictions[[2]]$wind_speed[1:nrow_tmp]
  }
  observations_forPredictions <- forPredictions[[2]]  %>% as.data.frame()
  
  data_forPredictions <- dateInfo_forPredictions %>% mutate(
    date = as.Date(date)
  )%>% left_join(observations_forPredictions %>%
                   mutate(forecastByHours = as.Date(forecastByHours)), by = c('date' = 'forecastByHours'))
  
  data_forPredictions <- data_forPredictions %>% mutate(
    dayName = as.factor(ifelse(dayName %in% c('Monday','Tuesday','Wednesday','Thursday','Friday') & !freeDay, 'Weekday',
                               ifelse(dayName %in% c('Monday','Tuesday','Wednesday','Thursday','Friday') & freeDay, 'FreeWeekDay', 'Weekend'))),
    freeDay = freeDay,
    monthNum =  factor(ifelse(monthNum %in% c(12,1,2),'Winter',ifelse(monthNum %in% c(3,4,5),'Spring',
                                                                         ifelse(monthNum %in% c(6,7,8), 'Summer','Autumn'))),levels=c('Winter','Spring','Summer','Autumn')),
    yearNum = as.numeric(yearNum)
  ) %>% select(
    -wind_speed,
    -humidity
  )

 # prevConsumption1 <- prevConsumption %>% mutate(
#    consumption =  CheckIf96consumptions(prevConsumption, ntime)) %>% .$consumption
  
  
    
  prevConsumption <- prevConsumption$consumptions %>% filter(date < min(data_forPredictions$date)) %>% arrange(desc(date)) %>% head(10)
  
  prevCon_len<-which(unlist(lapply(prevConsumption$values, length))<96) #patch fro days where hours is changed (missing 4 values)
  for(ind in prevCon_len){
    tail= rep(prevConsumption$values[[ind]][length(prevConsumption$values[[ind]])],96-length(prevConsumption$values[[ind]]))
    prevConsumption$values[[ind]]<-c(prevConsumption$values[[ind]],tail)
  }
  
  predictions <- seq(1,96)
  date_forpred <- rep('',96)
  fixed_date <- as.POSIXct(data_forPredictions$date[1], tz = 'CET') -105*60
  for (i in 1:96){
    
    if (min(unlist(lapply(prevConsumption$values, length))) < 96){
      date_forpred[i] <- fixed_date + minutes(15*(i-1))
      print(paste0("Not enough values in previous consumption data. Subscript out of bounds at ",
                   min(unlist(lapply(prevConsumption$values, length))),". We need all 96 to make predictions."))
      return(data.frame(datetime = as.POSIXct(as.numeric(date_forpred), origin='1970-01-01') , predictions = 0))
    }else{
    predictions[i] <- predictOneTime(forPredictions = data_forPredictions,
                                     prevConsumption = prevConsumption, 
                                     LMmodels = LMmodels,
                                     ntime = i,
                                     nrow_tmp = nrow_tmp,
                                     full_model = full_model)
    date_forpred[i] <- fixed_date + minutes(15*(i-1))
    }
  }
  predictions <- predictions %>% as.numeric()
  return(data.frame(date = as.POSIXct(as.numeric(date_forpred), origin='1970-01-01') , predicted_consumptions = predictions))
}

f_prediction_with_GLM_api <- function(predictionData, consumptionData, pred_model, full_model = TRUE){
  prediction_results <- predictOneDay(forPredictions = predictionData,
                                      prevConsumption = consumptionData, 
                                      LMmodels = pred_model,
                                      full_model = full_model)
  
  
  returnValue <- list(date=predictionData$dateInfo$date,predicted_consumption=prediction_results$predicted_consumptions)
  return(returnValue)
}