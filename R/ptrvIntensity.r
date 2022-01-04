#' @param  result is a result of ptrvIntensityByTime
#' @param negativeAsNull If TRUE, all negative value are replaced by 0
#' @param  timePeriod vector with two numbers indicating an interval to calculate the intensities
#' @export
#' @title ptrvIntensity
#' @importFrom reshape2 dcast
ptrvIntensity=function(result,timePeriod=NULL,negativeAsNull=TRUE,smoothingParameter=NULL)
{
   if(is.null(timePeriod)){timePeriod=c(min(result[,"time"]),max(result[,"time"]))}
  if(negativeAsNull){result[result<0]=0}

  result=result[result[,"time"]>=timePeriod[1]&result[,"time"]<=timePeriod[2],]

  resultMax0=dcast(result,ion~., value.var="intensity",fun.aggregate=function(x) return(max(x,na.rm=T)))
  resultMin0=dcast(result,ion~., value.var="intensity",fun.aggregate=function(x) return(min(x,na.rm=T)))

  resultMean0=dcast(result,ion~., value.var="intensity",fun.aggregate=function(x) return(mean(x,na.rm=T)))
  resultSd0=dcast(result,ion~., value.var="intensity",fun.aggregate=function(x) return(sd(x,na.rm=T)))
  resultN0=dcast(result,ion~., value.var="intensity",fun.aggregate=function(x) return(sum(!is.na(x),na.rm=T)))

  resultSum0=dcast(result,ion~., value.var="intensity",fun.aggregate=function(x) return(sum(x,na.rm=T)))
  colnames(resultMax0)=colnames(resultSd0)=colnames(resultN0)=colnames(resultMean0)=colnames(resultSum0)=colnames(resultMin0)=c("ion","intensity")
  resultFinal=data.frame(ion=resultSum0[,"ion"],sum=resultSum0[,"intensity"],moy=resultMean0[,"intensity"],max=resultMax0[,"intensity"],min=resultMin0[,"intensity"],n=resultN0[,"intensity"],sd=resultSd0[,"intensity"])

  res_wide=dcast(result[,c("time","ion","intensity")],time~ion,value.var="intensity",fun.aggregate=function(x){return(max(x,na.rm=T))})
  if(dim(res_wide)[2]>2)
  {
    resultTmax= res_wide[apply(res_wide[,-c(1)],2,which.max),"time"]
  }
  else
  {
    resultTmax=res_wide[which.max(res_wide[,2]),"time"]
  }
  names(resultTmax)=colnames(res_wide)[-c(1)]
  df_tmax=data.frame(ion=names(resultTmax),tmax=resultTmax)


  if("duration"%in%colnames(result))
  {
    result[,"area"]=result[,"intensity"]*result[,"duration"]
    resultArea=dcast(result,ion~., value.var="area",fun.aggregate=function(x) return(sum(x,na.rm=T)) )
    colnames(resultArea)=c("ion","intensity")
    resultFinal[,"area"]=resultArea[,"intensity"]
  }

  resultFinal2=merge(resultFinal,df_tmax,by="ion")

  return(resultFinal2)
}
