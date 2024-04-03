#' @param  dataset is a result of ptrvIntensityByTime$res: a dataframe with colnames as 'time','duration','ion' and  'intensity'
#' @param negativeAsNull If TRUE, all negative value are replaced by 0
#' @param  timePeriod vector with two numbers indicating an interval to calculate the intensities
#' @export
#' @title ptrvIntensity
#' @importFrom reshape2 dcast
#' @importFrom stats sd
ptrvIntensity=function(dataset,timePeriod=NULL,negativeAsNull=TRUE,propPeak=FALSE, proportion=0.75,timing="last")
{
  if(is.null(timePeriod)){timePeriod=c(min(dataset[,"time"]),max(dataset[,"time"]))}
  if(negativeAsNull){dataset[dataset<0]=0}

  dataset=dataset[dataset[,"time"]>=timePeriod[1]&dataset[,"time"]<=timePeriod[2],]
  if(dim(dataset)[1]==0){stop("No time in this time period.")}
  datasetMax0=dcast(dataset,ion~., value.var="intensity",fun.aggregate=function(x) return(max(x,na.rm=T)),fill=0)
  datasetMin0=dcast(dataset,ion~., value.var="intensity",fun.aggregate=function(x) return(min(x,na.rm=T)),fill=0)
  datasetMean0=dcast(dataset,ion~., value.var="intensity",fun.aggregate=function(x) return(mean(x,na.rm=T)),fill=0)
  datasetSd0=dcast(dataset,ion~., value.var="intensity",fun.aggregate=function(x) return(sd(x,na.rm=T)),fill=0)
  datasetN0=dcast(dataset,ion~., value.var="intensity",fun.aggregate=function(x) return(sum(!is.na(x),na.rm=T)),fill=0)
  datasetSum0=dcast(dataset,ion~., value.var="intensity",fun.aggregate=function(x) return(sum(x,na.rm=T)),fill=0)
  colnames(datasetMax0)=colnames(datasetSd0)=colnames(datasetN0)=colnames(datasetMean0)=colnames(datasetSum0)=colnames(datasetMin0)=c("ion","intensity")
  datasetFinal=data.frame(ion=datasetSum0[,"ion"],sum=datasetSum0[,"intensity"],moy=datasetMean0[,"intensity"],max=datasetMax0[,"intensity"],min=datasetMin0[,"intensity"],n=datasetN0[,"intensity"],sd=datasetSd0[,"intensity"])
  # tmax
  res_wide=dcast(dataset[,c("time","ion","intensity")],time~ion,value.var="intensity",fun.aggregate=function(x){return(max(x,na.rm=T))},fill=0)
  if(dim(res_wide)[2]>2)
  {
    datasetTmax= res_wide[apply(res_wide[,-c(1)],2,which.max),"time"]
  }
  else
  {
    datasetTmax=res_wide[which.max(res_wide[,2]),"time"]
  }
  names(datasetTmax)=colnames(res_wide)[-c(1)]
  df_tmax=data.frame(ion=names(datasetTmax),tmax=datasetTmax)

  # Area
  if("duration"%in%colnames(dataset))
  {
    dataset[,"area"]=dataset[,"intensity"]*dataset[,"duration"]
    datasetArea=dcast(dataset,ion~., value.var="area",fun.aggregate=function(x) return(sum(x,na.rm=T)) )
    colnames(datasetArea)=c("ion","intensity")
    datasetFinal[,"area"]=datasetArea[,"intensity"]
  }
  datasetFinal2=merge(datasetFinal,df_tmax,by="ion")

  # T percentage
  if(propPeak)
  {
    datasetFinal2[,paste0("t",proportion,timing)]=NA
    for(ion in datasetFinal2[,"ion"])
    {
      time_prop=timeForPropPeak(time=dataset[dataset[,"ion"]==ion,"time"],intensity=dataset[dataset[,"ion"]==ion,"intensity"],proportion=proportion,timing=timing)
      datasetFinal2[datasetFinal2[,"ion"]==ion,paste0("t",proportion,timing)]=time_prop
    }
  }

  return(datasetFinal2)
}
