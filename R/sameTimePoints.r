#' sameTimePoints
#' sameTimePoints
#' @param df data frame with at least three columns (named as colTime, colDuration and colIntensity)
#' @param colTime name of the column corresponding to time
#' @param colDuration name of the column corresponding to duration
#' @param colIntensity name of the column corresponding to intensity
#' @param breakTimes vector of numerics corresponding to time breaks
#' @param npoints number of points (to fill breakTimes automatically)
#' @export
#' @importFrom stats weighted.mean
sameTimePoints=function(df,npoints=100,colTime="time",colDuration="duration",colIntensity="intensity",breakTimes=NULL)
{
  df=df[order(df[,colTime]),]
  if(sum(!round((df[,colTime]+df[,colDuration])[-length(df[,colDuration])]-df[,colTime][-1],digits=8)==0)!=0)
  {
    stop("time+duration!=time[-1]")
  }
  if(is.null(breakTimes))
  {
    breakTimes= seq(min(as.numeric(df[,colTime])),max(as.numeric(df[,colTime]+as.numeric(df[,colDuration])),na.rm=T),length.out=npoints)
  }

  breakLabs=(breakTimes[-1]+breakTimes[-length(breakTimes)])/2
  df[,"time_all"]=cut(df[,colTime],include.lowest=TRUE,right=FALSE,breaks=breakTimes,labels=breakLabs)

  newdf=data.frame(times=breakLabs,intensity=NA) # on construit la table finale
  for(i in 1:nrow(newdf))
  {
   dfi=df[df[,"time_all"]==newdf[i,"times"],]
    dfi=dfi[order(dfi[,colTime]),]

    if(nrow(dfi)!=0) # s'il existe des temps correspondant à ce break mais qui "dépassent"
    {
      if(dfi[nrow(dfi),colTime]+dfi[nrow(dfi),colDuration]>breakTimes[i+1]) # Si on dépasse
      {

        newLine=data.frame(time=breakTimes[i+1],
                           duration=dfi[nrow(dfi),colDuration]-(breakTimes[i+1]-dfi[nrow(dfi),colTime]),
                           intensity=dfi[nrow(dfi),colIntensity],
                           time_all=breakLabs[i+1])

        names(newLine)=c(colTime,colDuration,colIntensity,"time_all")
        indexToMove=max(which(df[,"time_all"]==newdf[i,"times"]))
        df[indexToMove,colDuration]=breakTimes[i+1]-df[indexToMove,colTime]
        df=rbind(df,newLine)
        df=df[order(df[,colTime]),]
 #       df[,"time_all"]
 #       newdf[i,"times"]
      }
      dfToUseForFill=df[df[,"time_all"]==newdf[i,"times"],]
 #     print(dfToUseForFill)
      newdf[i,"intensity"]=weighted.mean(dfToUseForFill[,"intensity"],w=dfToUseForFill[,colDuration],na.rm=T)

    }
    else
    {
      newdf[i,"intensity"]=newdf[i-1,"intensity"]
    }
  }
  return(list(new=newdf,transformed=df,breakTimes=breakTimes,breakLabs=breakLabs))
}



