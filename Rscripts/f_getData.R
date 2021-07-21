library(httr)
library(jsonlite)
f_getHistoricData <- function(var_custID = '2-9629', date_from = '2017-01-01',date_to = '2017-12-31',var_location='Ljubljana'){
  # This function return aggregated data from the data aggregation service
  # output: - a structure of needed data
  # input:  - varConsumerID . id of customer
  #         - date_from first date of data,
  #         - date_edn last date of data.
  
  # primer klica funkcije:
  # res <- f_getData('2-9629','2017-01-01','2017-01-30')
  
  
  r <- GET('http://192.168.190.21:8100/historicData',
           query = list(consumerID = var_custID, consumerLocation = var_location, firstDay = date_from, lastDay = date_to))
  data <- fromJSON(content(r, 'text'))
  
  #Error check
  if(!is.null(data$ERROR)){
    print ("ERROR in Data API")
    print (data$ERROR)
    return (data)
  }
  
  result <- data$historicData
  return(result)
}

f_getConsumptionsGroupDays <- function(date_from = '2017-01-01',date_to = '2017-12-31'){
  # This function return aggregated data from the data aggregation service
  # output: - a structure of needed data
  # input:  - varConsumerID . id of customer
  #         - date_from first date of data,
  #         - date_edn last date of data.
  
  # primer klica funkcije:
  # res <- f_getData('2-9629','2017-01-01','2017-01-30')
  #if (as.Date(date_to)>as.Date(today()-2)){
  #  date_to <- as.Date(today()-2)
  #}
  
  r <- GET('http://192.168.190.21:8100/consumptionsPerDay',
           query = list(firstDay = date_from, lastDay = date_to))
  data <- fromJSON(content(r, 'text'))
  
  #Error check
  if(!is.null(data$ERROR)){
    print ("ERROR in Data API")
    print (data$ERROR)
    return (data)
  }
  
  result <- data$consumptions
  return(result)
}

f_getForecastData <- function(var_location='Ljubljana',day_forecast='2020-05-21'){
  # This function return aggregated data from the data aggregation service
  # output: - a structure of needed data
  # input:  - varConsumerID . id of customer
  #         - date_from first date of data,
  #         - date_edn last date of data.
  
  # primer klica funkcije:
  # res <- f_getData('2-9629','2017-01-01','2017-01-30')
  
  r <- GET('http://192.168.190.21:8100/forecast',
           query = list(location = var_location, day = day_forecast))
  result <- fromJSON(content(r, 'text'))
  
  #Error check
  if(!is.null(result$ERROR)){
    print ("ERROR in Data API")
    print (result$ERROR)
    return (result)
  }
  
  return(result)
}

f_getForecastEvalData <- function(var_location='Ljubljana',day_forecast='2019-05-21'){
  # This function return aggregated data from the data aggregation service
  # output: - a structure of needed data
  # input:  - varConsumerID . id of customer
  #         - date_from first date of data,
  #         - date_edn last date of data.
  
  # primer klica funkcije:
  # res <- f_getData('2-9629','2017-01-01','2017-01-30')
  
  r <- GET('http://192.168.190.21:8100/forecastEval',
           query = list(location = var_location, day = day_forecast))
  if (r$status_code != 200){
    return(c())
  }
  result <- try(fromJSON(content(r, 'text')))
  if (strcmp(substr(result[1],1,5),"Error")){
    return(c())
  } else {
    return(result)
  }
}

f_getCustomers <- function(var_region = "all"){
  if (var_region == 'all'){
    r <- GET('http://192.168.190.21:8100/customers')
  } else {
    r <- GET('http://192.168.190.21:8100/customers',
             query = list(region = var_region))
  }
  result <- try(fromJSON(content(r, 'text')))
  return(result)
  
}

f_getModel <- function(modelname = "kmeans", version = 2, measuringpoint = "2-1603"){
  r <- GET(sprintf('http://192.168.190.21:8100/prediction/models/%s/%s/%s', modelname, version, measuringpoint))
  result <- try(fromJSON(content(r, 'text')))
  model <- unserializeJSON(result$data)
  model$createdtimestamp <- result$createdts
  return(model)
  
}

f_getEvaluation <- function(modelname = "kmeans", version = 2, var_limit = 10, var_firstDay = "2000-01-01", var_lastDay = "2950-12-31"){
  r <- GET(sprintf('http://192.168.190.21:8100/prediction/evaluation/%s/%s', modelname, version),
           query = list(limit = var_limit, firstDay = var_firstDay, lastDay = var_lastDay))
  result <- fromJSON(content(r, 'text'))
  return(result)
  
}

