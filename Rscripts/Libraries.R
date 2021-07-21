####################################### Use Libraries #######################################
list.of.packages <- c(
                      # "car",
                      "jsonlite",
                      "nnet",
                      "purrr",
                      "tictoc",
                      "psych",
                      "aweek",
                      "caTools"
                      ,"caret"
                      # ,"chron"
                      # ,"cluster"
                      # ,"corrplot"
                      # ,"dendextend"
                      ,"devtools"
                      ,"dplyr"
                      ,"unikn"
                      # ,"factoextra"
                      # ,"foreach"
                      # ,"gbm"
                      # ,"ggmap"
                      ,"ggplot2"
                      # ,"ggpubr"
                      # ,"googleway"
                      # ,"gplots"
                      # ,"gridExtra"
                      # ,"h2o"
                      # ,"huxtable"
                      # ,"kableExtra"
                      # ,"LICORS"
                      # ,"lime"
                      ,"lubridate"
                      ,"magrittr"
                      # ,"memisc"
                      # ,"Metrics"
                      ,"mongolite"
                      ,"NbClust"
                      ,"openxlsx"
                      # ,"party"
                      ,"plotly"
                      ,"plyr"
                      ,"randomForest"
                      # ,"ranger"
                      # ,"rattle"
                      ,"RColorBrewer"
                      # ,"reshape"
                      ,"reshape2"
                      # ,"rmongodb"
                      ,"rpart"
                      ,"rpart.plot"
                      ,"rsample"
                      # ,"tidyr"
                      #,"tidyverse"
                      # ,"VIM"
                      # ,"viridis"
                      # ,"wesanderson"
                      # ,"xgboost"
                      # ,"xlsx"
                      ,"zoo"
                      ,"data.table"
                      ,"RColorBrewer"
                      ,"xtable"
                      ,"pracma"
                      ,"forecast"
#                      ,"prophet"
                      ,"plumber"
                      )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

for(p in new.packages){
    install.packages(p,dep=TRUE)
}

for (p in list.of.packages){
    library(p,character.only = TRUE)
}

#rm(list.of.packages,GCtorture,new.packages,p)

# devtools::install_github("dkahle/ggmap", ref = "tidyup") 

####################################### Keys #######################################
# key <-"AIzaSyCdHhEDHNWbeVjgu7SMPcLPkGWFtvQvSJs" 
# register_google(key = key)

####################################### Libraries Details #######################################
# caret        # an aggregator package for performing many machine learning models
# chron        # 
# cluster      # for clustering
# corrplot     # package corrplot
# dendextend   # for get_subdendrograms
# devtools     # Collection of package development tools.
# dplyr        # A Grammar of Data Manipulation, Tools for Splitting, Applying and Combining Data
# foreach      # 
# gbm          # basic implementation
# ggplot2      # model visualization
# gridExtra    # Arranging ggplots in a grid
# h2o          # a java-based platform
# huxtable     # 
# kableExtra   # manipulate table styles
# knitr        # 
# LICORS       # 
# lime         # model visualization
# lubridate    # for dates
# magrittr     # for maintainability of code
# mongolite    # 
# NbClust      # 
# openxlsx     # For read.xlsx
# pdp          # model visualization
# plotly       # 
# plyr         # Tools for Splitting, Applying and Combining Data
# randomForest # basic implementation
# ranger       # a faster implementation of randomForest
# reshape      # for data manipulation
# reshape2     # 
# RMongodb     # 
# rsample      # data splitting 
# scales       # 
# tidyr        # Easily Tidy Data with 'spread' and 'gather' Functions
# tidyverse    # Set of packages that work in harmony because they share common data representations and 'API' design
# VIM          # for aggr function
# xgboost      # a faster implementation of gbm
# zoo          # for na.locf