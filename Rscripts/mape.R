# computes MAPE and WAPE between rows of y and yhat
# or between y and yhat if y and yhat are vectors
# author: J Povh
# y, yhat matrices or vectors of the same size
# little change 2021 03 25
# last inspection: 2021 04 08
mape <- function(y, yhat){
  mm=c("NA")
  ww=c("NA")
  Mape_rows=c("NA")
  Wape_rows=c("NA")
  
  if (is.vector(yhat)){
    row_min=min(abs(yhat))
    row_sum=sum(abs(yhat))
    N=length(yhat)
    if (row_min>0){
      mm=1/N*sum(abs(y-yhat)/yhat)
      Mape_rows=c(1)
    }
    if (row_min>0){
      ww=sum(abs(y-yhat))/sum(abs(yhat))
      Wape_rows=c(1)
    }
    MAPE=list("MAPE"=mm,"mape_rows"=Mape_rows);
    WAPE=list("WAPE"=ww,"wape_rows"=Wape_rows);
    result=list("MAPE"=MAPE,"WAPE"=WAPE)
    return(result)
  }
  
  if (dim(yhat)[1]>1){
    rowMins=apply(abs(yhat), 1, FUN=min)
    Mape_rows=which(rowMins>0)
    Wape_rows=which(rowSums(abs(yhat))>0)
    N=dim(yhat)[2]
    if (length(Mape_rows)>1){
      mm=1/N*rowSums(abs(y[Mape_rows,]-yhat[Mape_rows,])/abs(yhat[Mape_rows,]))
    } else if (length(Mape_rows)==1){
      mm=1/N*sum(abs(y[Mape_rows,]-yhat[Mape_rows,])/abs(yhat[Mape_rows,]))
    } 
    
    if (length(Wape_rows)>1){
      ww=rowSums(abs(y[Wape_rows,]-yhat[Wape_rows,]))/rowSums(abs(yhat[Wape_rows,]))
    } else if (length(Wape_rows)==1){
      ww=sum(abs(y[Wape_rows,]-yhat[Wape_rows,]))/sum(abs(yhat[Wape_rows,]))
    }
  } else {
    row_min=min(abs(yhat))
    row_sum=sum(abs(yhat))
    N=length(yhat)
    if (row_min>0){
      mm=1/N*sum(abs(y-yhat)/yhat)
      Mape_rows=c(1)
    }
    if (row_min>0){
      ww=sum(abs(y-yhat))/sum(abs(yhat))
      Wape_rows=c(1)
    }
  }
  MAPE=list("MAPE"=mm,"mape_rows"=Mape_rows);
  WAPE=list("WAPE"=ww,"wape_rows"=Wape_rows);
  
  result=list("MAPE"=MAPE,"WAPE"=WAPE)
  return(result)
}