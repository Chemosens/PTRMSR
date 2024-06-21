#' @title ptrvListIntensity
#' @param df a result of ptrvListIntensityByFiles ($res) ie a data.frame whose colnames are  'time ' 'ion' 'intensity' 'duration' 'file' and others
#' @param format "wide" by default, can also be 'long'
#' @param stat "area" by default but can also be "tmax" or "max"
#' @param timePeriod calculates on a specific time period. If not null, can be a vector with two values (start and stop) or a data.frame whose colnames are file, start and stop
#' @inheritParams ptrvIntensity
#' @importFrom utils read.table
#' @export
ptrvListIntensity=function(df,format="wide",stat="area", timePeriod = NULL, negativeAsNull = TRUE,fill=NULL)
{
  listFiles=levels(factor(df[,"file"]))
  for(i in 1:length(listFiles))
  {
    file=listFiles[i]
    result_all=df[df[,"file"]==file,]
    if(format=="wide")
    {
        if(is.data.frame(timePeriod))
        {
          if(any(!c("file","start","stop")%in%colnames(timePeriod))){stop("timePeriod colnames should be file, start and stop")}
          if(!file%in%timePeriod[,"file"]){stop(paste0("a file is not in timePeriod 'file' column: ",file))}
          if(dim(timePeriod[timePeriod[,"file"]==file,])[1]>1){stop(paste0(file, "is in the timePeriod dataset more than once"))}
          timePeriodFile=c(timePeriod[timePeriod[,"file"]==file,"start"],timePeriod[timePeriod[,"file"]==file,"stop"])
        }
        else{ timePeriodFile=timePeriod}

        res_cycle=ptrvIntensity(result_all,timePeriod=timePeriodFile,negativeAsNull=negativeAsNull,fill=fill)[,c("ion",stat)]
        if(i==1) { res=res_cycle;colnames(res)[2]=listFiles[1]}
        if(i>1){ res=merge(res,res_cycle,by="ion");colnames(res)[i+1]=listFiles[i]}
    }
    if(format=="long")
   {
     res_cycle=ptrvIntensity(result_all,timePeriod=timePeriod,negativeAsNull=negativeAsNull,fill=fill)[,c("ion",stat)]
     res_cycle[,"file"]=listFiles[i]
     res=rbind(res,res_cycle)
    }

  }
  return(res)
}