#' @param dataset dataset whose colnames are intensity, time, ion (result of ptrvIntensityByTime$res)
#' @export
ptrvSmooth=function(dataset,spar=NULL,sameTime=TRUE,time_x=NULL,method="Spline",negativeValuesToZero=TRUE)
{
  ions=unique(dataset[,"ion"])
  df=NULL
  for(ion in ions)
  {
    dataset_ion0=dataset[dataset[,"ion"]==ion,]
    dataset_ion=dataset_ion0[order(dataset_ion0[,"time"]),]
    res_smooth=smoothIntensityByTime(time=dataset_ion[,"time"],intensity=dataset_ion[,"intensity"],method=method,spar=spar,sameTime=sameTime,time_x=time_x,negativeValuesToZero=negativeValuesToZero)
    df_i=data.frame(time=res_smooth$time,intensity=res_smooth$intensity,ion=ion)
    df_i[,"duration"]=c(df_i[-1,"time"]-df_i[-length(dataset_ion),"time"],0)
    df=rbind(df,df_i)
  }
  return(df)
}