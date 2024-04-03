#'@param time vector of times
#'@param intensity vector of intensities with same length as time (with only positive value if method = "SavitzkyGolay")
#' @param spar smoothing parameter corresponding to spar for method="Spline", span for method="Loess", and halfWindowSize for MovingAverage and SavitzkyGolay. Default to 0.5 for Loess and Spline, and to 5 for SavitzkyGolay and MovingAverage
#'@export
smoothIntensityByTime=function(time,intensity,spar=NULL,sameTime=TRUE,time_x=NULL,method="Spline",negativeValuesToZero=TRUE)
{
  #param <- match.call()[-1]
  match.arg(method,c("SavitzkyGolay","MovingAverage","Spline","Loess"))
  time_without_NA=time[!is.na(time)&!is.na(intensity)]
  intensity_without_NA=intensity[!is.na(time)&!is.na(intensity)]
  if(any(order(time_without_NA)!=1:length(time_without_NA)))
  {
    warning("time in smoothing intensity was not increasing")
  }
  if(is.null(spar))
  {
    if(method%in%c("Loess","Spline")){spar=0.5}
    if(method%in%c("SavitzkyGolay","MovingAverage")){spar=5}
  }

  if(negativeValuesToZero){intensity_without_NA[intensity_without_NA<0]=0}
  if(method=="Loess" &sameTime){sameTime=FALSE; warning("sameTime not available for method Loess. sameTime was put at FALSE")}
  if(method=="SavitzkyGolay" &sameTime){
    if(sameTime)
    {
      sameTime=FALSE; warning("sameTime not available for method SavitzkyGolay. sameTime was put at FALSE")
    }
    if(any(intensity_without_NA<0)){warning("negative values in intensities were put at zero");intensity[intensity<0]=0}
  }

  if(method=="MovingAverage" &sameTime)
  {
    if(sameTime)
    {
      sameTime=FALSE; warning("sameTime not available for method MovingAverage. sameTime was put at FALSE")

    }
    if(any(intensity_without_NA<0)){warning("negative values in intensities were put at zero");intensity[intensity<0]=0}
  }


  if(method=="Spline")
  {
    spline=smooth.spline(x=time_without_NA,y=intensity_without_NA,spar=spar)
    if(!sameTime & is.null(time_x))
    {
      time_res=spline$x
      intensity_res=spline$y
    }
    if(!sameTime & !is.null(time_x))
    {
      res_pred=predict(spline, time_x)
      time_res=res_pred$x
      intensity_res=res_pred$y
    }
    if(sameTime)
    {
      res_pred=predict(spline, time)
      time_res=res_pred$x
      intensity_res=res_pred$y
    }
  }
  if(method=="Loess")
  {
    res_loess=loess(intensity_without_NA~time_without_NA,span=spar)
    intensity_res=res_loess$fitted
    time_res=res_loess$x
  }
  if(method=="SavitzkyGolay"|method=="MovingAverage")
  {
    sp1=new("Spectrum1",mz=time_without_NA,intensity=intensity_without_NA,centroided=F)
    spSmoothed=MSnbase::smooth(sp1,method=method,halfWindowSize = spar)
    time_res=MSnbase::mz(sp1)
    intensity_res=MSnbase::intensity(spSmoothed)
  }
  param=list(spar=spar,sameTime=sameTime,time_x=time_x)
 # plot(time_res,intensity_res)
  return(res=list(time=time_res,intensity=intensity_res,param=param))
}

#
# data(ptrv)
# time=ptrv$RelTime
# intensity=ptrv$m47.04914..Ethanol...Conc.
#
# res05=smoothIntensityByTime(time,intensity,spar=0.5)
# res01=smoothIntensityByTime(time,intensity,spar=0.1)
# res09=smoothIntensityByTime(time,intensity,spar=0.9)
#
# par(mfrow=c(2,2))
# plot(time,intensity,type="l",main="no smooth")
# time==res$time
# plot(res01$time,res01$intensity,type="l",main="spar = 0.1")
# plot(res05$time,res05$intensity,type="l",main="spar = 0.5")
# plot(res09$time,res09$intensity,type="l",main="spar = 0.9")
#
