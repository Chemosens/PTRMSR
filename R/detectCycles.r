#' detectCycle
#'
#' Detects breathing cycles (negative peaks with smoothing methods). A breaking cycle begins half of an inspiration, then a full expiration, then an last half of inspiration
#'
#' @param minExpi minimal value for an expiry intensity to be detected as an expiry (blue dotted line in the plot).
#' @param smoothMethod smoothing method. Among "MovingAverage" or "SavitzkyGolay". See MSnbase::smooth for more details.
#' @param method method to estimate the noise "MAD" or "" in the spectra
#' @param maxInspi maximal intensity of the original curve required to be considered as a breathing peak (purple dotted line in the plot)
#' @param SNR signal noise ratio (0 by default) required to be considered as a peak
#' @param minimalDuration minimalDuration of a breathing cycle (2 by default - as a accelerated breathing frequency corresponds to 20 cycles and more by minut and a low frequency is 12 and less -)
#' @param halfWindowSize required for the smoothing method. Integer corresponding to the half window size to consider.
#' @param timePeriod vector containing the first and last value to attribue to the breathing cycle (start and end of tasting for example). Default to min(time) and max(time)
#' @param df dataframe with two columns whose colnames are 'intensity','time'
#' @param forMinExpiDivideMaxIntBy default to 5. When minExpi is null, it is evaluated as the ratio of the maximal intensity and forMinIntensityDivideMaxIntBy
#' @param forMaxInspiDivideMaxIntBy default to 5. When minExpi is null, it is evaluated as the ratio of the maximal intensity and forMinIntensityDivideMaxIntBy
#' @param mobileMinExpi default to NULL, ifelse an integer higher than 3. If not NULL, the minimal intensity required for an expiration is a mobile one, defined by an mobile average (on the number of points defined in mobileMinExpi) + mobileK * mobile standard deviations (on the same number of points)
#' @param mobileMaxInspi  default to NULL, ifelsea integer higher than 3. If not NULL, the maximal intensity required for an inspiration is a mobile one, defined by an mobile average (on the number of points defined in mobileMinExpi) - mobileK * mobile standard deviations (on the same number of points)
#' @param mobileK default to 1. a number to be used in mobileMinExpi or mobileMaxInspi if not NULL
#' @export
#' @return a list containing the times corresponding to cycle limits (cycles), some plots the reversed smoothed data used for detecting peaks (gg$p2) and raw data with the cycle limits (gg$p3), a table containing information about the selected peaks: finalPeakTable and a table containing all the peaks detected before any filter (peakTable)
#' @importFrom stats embed
detectCycle=function(df,minExpi=NULL,maxInspi=NULL,smoothMethod="MovingAverage",method="MAD",
                     halfWindowSize=5,timePeriod=NULL,SNR=0,minimalDuration=2,forMinExpiDivideMaxIntBy=4,
                     forMaxInspiDivideMaxIntBy=5,mobileMinExpi=NULL,mobileMaxInspi=NULL,mobileK=1)
{
  x=NULL;y=NULL
  if(is.null(timePeriod)){timePeriod[1]=min(df[,"time"],na.rm=T);timePeriod[2]=max(df[,"time"],na.rm=T)}
  if(is.na(timePeriod[1])){timePeriod[1]=min(df[,"time"],na.rm=T)}
  if(is.na(timePeriod[2])){timePeriod[2]=max(df[,"time"],na.rm=T)}

 # print("in detectCycle")
  time=intensity=NULL
  maxInt=max(df[!is.na(df[,"intensity"]),"intensity"])
  if(!is.null(mobileMinExpi)){if(mobileMinExpi<4){stop("Please enter mobileMinExpi>4")}else{mobileMinExpi=round(mobileMinExpi)}}
  if(!is.null(mobileMaxInspi)){if(mobileMaxInspi<4){stop("Please enter mobileMaxInspi>4")}else{mobileMaxInspi=round(mobileMaxInspi)}}

  if(is.null(mobileMinExpi))
  {
    if(is.null(minExpi)){minExpi=maxInt/forMinExpiDivideMaxIntBy}
  }

  if(is.null(mobileMaxInspi))
  {
    if(is.null(maxInspi)){maxInspi=maxInt/forMaxInspiDivideMaxIntBy}
  }



  # The spectra is returned for identifying Inspirations
  sp1=new("Spectrum1",mz=df[!is.na(df[,"intensity"]),"time"],intensity=maxInt-df[!is.na(df[,"intensity"]),"intensity"],centroided=F)
  spSmoothed=MSnbase::smooth(sp1,method=smoothMethod,halfWindowSize = halfWindowSize)
  dfspSmoothed=data.frame(time=MSnbase::mz(sp1),intensity=MSnbase::intensity(spSmoothed))

  p2=ggplot(dfspSmoothed,aes(x=time,y=intensity))+geom_line()+theme_bw()+ggtitle("Smoothed data for breathing")
  if(is.null(mobileMaxInspi))
  {
    p2=p2+geom_hline(yintercept=maxInt-maxInspi,col="purple",linetype = "dotted")
  }
  else
  {
    intensityWithSuppPoints=c(dfspSmoothed[,"intensity"],dfspSmoothed[1:(mobileMaxInspi-1),"intensity"])
    em = embed(intensityWithSuppPoints,mobileMaxInspi) #mobileMaxInspi is the window size
    avgSmoothed= apply(em,1,function(x){return(mean(x,na.rm=T))})
    sdSmoothed= apply(em,1,function(x){return(sd(x,na.rm=T))})
    dfspSmoothed[,"avg"]=avgSmoothed
    dfspSmoothed[,"supLimit"]=avgSmoothed+mobileK*sdSmoothed
    p2=p2+geom_line(data=data.frame(x=as.vector(dfspSmoothed[,"time"]),y=as.vector(dfspSmoothed[,"supLimit"])),aes(x=x,y=y),col="purple",linetype="dotted")
    p2=p2+geom_line(data=data.frame(x=dfspSmoothed[,"time"],y=dfspSmoothed[,"avg"]),aes(x=x,y=y),col="cyan")
    maxInspi=maxInt-dfspSmoothed[,"supLimit"]
  }

  if(is.null(mobileMinExpi))
  {
    p2=p2+geom_hline(yintercept=maxInt-minExpi,col="blue",linetype = "dashed")
  }
  else
  { intensityWithSuppPoints=c(dfspSmoothed[,"intensity"],dfspSmoothed[1:(mobileMinExpi-1),"intensity"])
    em = stats::embed(intensityWithSuppPoints,mobileMinExpi) #mobileMaxInspi is the window size
    avgSmoothed= apply(em,1,function(x){return(mean(x,na.rm=T))})
    sdSmoothed= apply(em,1,function(x){return(sd(x,na.rm=T))})
    dfspSmoothed[,"avg"]=avgSmoothed
    dfspSmoothed=as.data.frame(dfspSmoothed)
    dfspSmoothed[,"infLimit"]=avgSmoothed-mobileK*sdSmoothed
    p2=p2+geom_line(data=data.frame(x=as.vector(dfspSmoothed[,"time"]),y=as.vector(dfspSmoothed[,"infLimit"])),aes(x=x,y=y),col="blue",linetype="dashed")
    p2=p2+geom_line(data=data.frame(x=dfspSmoothed[,"time"],y=dfspSmoothed[,"avg"]),aes(x=x,y=y),col="red",linetype="dashed")
    minExpi=maxInt-dfspSmoothed[,"infLimit"]
  }

  respicks=MSnbase::pickPeaks(spSmoothed,method=method,SNR=SNR)


  # selecting only the peaks that are higher than a given quantity (maxInt-maxInspi on the reversed spectra)
  if(is.null(mobileMaxInspi))
  {
    interestingTimes=MSnbase::mz(respicks)[MSnbase::intensity(respicks)>maxInt-maxInspi]
  }
  else
  {
    interestingTimes=MSnbase::mz(respicks)
  }

  if(length(interestingTimes)==0){warning("no peak detected. Maybe choose maxInspi higher ?(or forMaxInspiDivideMaxIntBy")}


  timeToUseForBreathing=unique(c(timePeriod[1],interestingTimes,timePeriod[2]))

# print(timeToUseForBreathing)
# print(length(timeToUseForBreathing))
  # removing too low picks # Back to real data (and not the reversed one)
  maxIntensityPerCycle=Inspiration=minExpiCycle=maxInspiCycle=rep(NA,length(timeToUseForBreathing)-1)
 # print("before")
  for(i in 1:(length(timeToUseForBreathing)-1))
  {
   # print(i)
    intensityPerCycle=df[df[,"time"]>=timeToUseForBreathing[i]  & df[,"time"]<=timeToUseForBreathing[i+1],"intensity"]
    maxIntensityPerCycle[i]=max(intensityPerCycle,na.rm=T)
    dfInt=df[df[,"time"]==timeToUseForBreathing[i+1] ,"intensity"]
    if(length(dfInt)==1)
    {
      Inspiration[i]=dfInt
    }

    if(!is.null(mobileMinExpi)){minExpiCycle[i]=mean(minExpi[df[,"time"]==timeToUseForBreathing[i+1]],na.rm=T)}
    if(!is.null(mobileMaxInspi)){maxInspiCycle[i]=mean(maxInspi[df[,"time"]==timeToUseForBreathing[i+1]],na.rm=T)}
  }

  durationPerCycle= diff(timeToUseForBreathing)
  peakTable=cbind(start=timeToUseForBreathing[-length(timeToUseForBreathing)],stop=timeToUseForBreathing[-1],duration=durationPerCycle,maxIntensity=maxIntensityPerCycle,Inspiration=Inspiration)
  # Removing time cycles lower than minimalDuration
  peakTable=as.data.frame(peakTable)
  peakTable[,"toRemove"]=FALSE
  if(is.null(mobileMinExpi)&is.null(mobileMaxInspi))
  {
    peakTable[durationPerCycle<minimalDuration|Inspiration>maxInspi|maxIntensityPerCycle<minExpi,"toRemove"]=TRUE
    removingStrangeCycles=which(durationPerCycle<minimalDuration|Inspiration>maxInspi|maxIntensityPerCycle<minExpi)+1
  }
  else
  {
    if(is.null(mobileMinExpi))
    {
      peakTable[durationPerCycle<minimalDuration|Inspiration>maxInspiCycle|maxIntensityPerCycle<minExpi,"toRemove"]=TRUE
      removingStrangeCycles=which(durationPerCycle<minimalDuration|Inspiration>maxInspiCycle|maxIntensityPerCycle<minExpi)+1
    }
    if(is.null(mobileMaxInspi))
    {
      peakTable[durationPerCycle<minimalDuration|Inspiration>maxInspi|maxIntensityPerCycle<minExpiCycle,"toRemove"]=TRUE
      removingStrangeCycles=which(durationPerCycle<minimalDuration|Inspiration>maxInspi|maxIntensityPerCycle<minExpiCycle)+1
    }
    if((!is.null(mobileMinExpi))&(!is.null(mobileMaxInspi)))
    {
      peakTable[durationPerCycle<minimalDuration|Inspiration>maxInspiCycle|maxIntensityPerCycle<minExpiCycle,"toRemove"]=TRUE
      removingStrangeCycles=which(durationPerCycle<minimalDuration|Inspiration>maxInspiCycle|maxIntensityPerCycle<minExpiCycle)+1
    }
  }

  if(length(removingStrangeCycles)!=0)
  {
    timeToUseForBreathing=timeToUseForBreathing[-removingStrangeCycles]
  }
  finalPeakTable=cbind(start=timeToUseForBreathing[-length(timeToUseForBreathing)],stop=timeToUseForBreathing[-1],
                       duration=diff(timeToUseForBreathing),maxIntensity=maxIntensityPerCycle[-c(removingStrangeCycles-1)])

  # p3 is the original data frame with the selected peaks

  p3=ggplot(df,aes(x=time,y=intensity))+geom_line()+theme_bw()+ggtitle("Raw data and breathing cycle limits")
  for(k in 1:length(timeToUseForBreathing))
  {
    p3=p3+geom_vline(xintercept=timeToUseForBreathing[k],col="red")
  }


  if(is.null(mobileMaxInspi))
  {
    p3=p3+geom_hline(yintercept=maxInspi,col="purple",linetype = "dotted")
  }
  else
  {
    p3=p3+geom_line(data=data.frame(x=dfspSmoothed[,"time"],y=maxInspi),aes(x=x,y=y),col="purple",linetype="dotted")
  }
  if(is.null(mobileMinExpi))
  {
    p3=p3+geom_hline(yintercept=minExpi,col="blue",linetype = "dashed")
  }
  else
  {
    p3=p3+geom_line(data=data.frame(x=dfspSmoothed[,"time"],y=minExpi),aes(x=x,y=y),col="blue",linetype="dashed")
  }

  param=c(minExpi=minExpi,maxInspi=maxInspi,smoothMethod=smoothMethod,method=method,
          halfWindowSize=halfWindowSize,timePeriod=timePeriod,SNR=SNR,minimalDuration=minimalDuration,forMinExpiDivideMaxIntBy=forMinExpiDivideMaxIntBy,forMaxInspiDivideMaxIntBy=forMaxInspiDivideMaxIntBy,mobileMinExpi=mobileMinExpi,mobileMaxInspi=mobileMaxInspi,mobileK=mobileK)

  if(length(timeToUseForBreathing)==1){warning("No peak was detected. Please change the parameters: minimalDuration (is it in the proper unity?),
                                               maxInspi or minExpi (is that coherent with the data?). ")}
 return(list(cycles=timeToUseForBreathing,gg=list(p2=p2,p3=p3),finalPeakTable=finalPeakTable,peakTable=peakTable,param=param))
}
