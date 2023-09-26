#' detectCycle
#' Detects breathing cycles (negative peaks with smoothing methods). A breaking cycle begins half of an inspiration, then a full expiration, then an last half of inspiration
#' @param minExpi minimal value for an expiry intensity to be detected as an expiry (blue dotted line in the plot).
#' @param smoothMethod smoothing method. Among "MovingAverage" or "SavitzkyGolay". See MSnbase::smooth for more details.
#' @param method method to estimate the noise "MAD" or ""
#' @param maxInspi maximal intensity of the original curve required to be considered as a breathing peak (purple dotted line in the plot)
#' @param SNR signal noise ratio (0 by default) required to be considered as a peak
#' @param minimalDuration minimalDuration of a breathing cycle (2 by default - as a accelerated breathing frequency corresponds to 20 cycles and more by minut and a low frequency is 12 and less -)
#' @param halfWindowSize required for the smoothing method. Integer corresponding to the half window size to consider.
#' @param maximum last value to attribue to the breathing cycle (end of tasting for example)
#' @param df dataframe with two columns whose colnames are 'intensity','time'
#' @param forMinExpiDivideMaxIntBy default to 5. When minExpi is null, it is evaluated as the ratio of the maximal intensity and forMinIntensityDivideMaxIntBy
#' @param forMaxInspiDivideMaxIntBy default to 5. When minExpi is null, it is evaluated as the ratio of the maximal intensity and forMinIntensityDivideMaxIntBy
#' @export
detectCycle=function(df,minExpi=NULL,maxInspi=NULL,smoothMethod="MovingAverage",method="MAD",halfWindowSize=5,maximum=NULL,SNR=0,minimalDuration=2,forMinExpiDivideMaxIntBy=4,forMaxInspiDivideMaxIntBy=5)
{
 # print("in detectCycle")
  time=intensity=NULL
  maxInt=max(df[!is.na(df[,"intensity"]),"intensity"])

  if(is.null(minExpi)){minExpi=maxInt/forMinExpiDivideMaxIntBy}
  if(is.null(maxInspi)){maxInspi=maxInt/forMaxInspiDivideMaxIntBy}

  # The spectra is returned for identifying Inspirations
  sp1=new("Spectrum1",mz=df[!is.na(df[,"intensity"]),"time"],intensity=maxInt-df[!is.na(df[,"intensity"]),"intensity"],centroided=F)
  sp2=MSnbase::smooth(sp1,method=smoothMethod,halfWindowSize = halfWindowSize)
  dfsp2=data.frame(time=MSnbase::mz(sp1),intensity=MSnbase::intensity(sp2))
  p2=ggplot(dfsp2,aes(x=time,y=intensity))+geom_line()+theme_bw()+ggtitle("Smoothed data for breathing")
  p2=p2+geom_hline(yintercept=maxInt-maxInspi,col="purple",linetype = "dotted")
  p2=p2+geom_hline(yintercept=maxInt-minExpi,col="blue",linetype = "dashed")
  respicks=MSnbase::pickPeaks(sp2,method=method,SNR=SNR)

  # selecting only the peaks that are higher than a given quantity (maxInt-maxInspi on the reversed spectra)
  timeToUseForBreathing=unique(c(0,MSnbase::mz(respicks)[MSnbase::intensity(respicks)>maxInt-maxInspi]))
  if(!is.null(maximum)){ timeToUseForBreathing=unique(c(0,timeToUseForBreathing,maximum))}

  # removing too low picks # Back to real data (and not the reversed one)
  maxIntensityPerCycle=Inspiration=rep(NA,length(timeToUseForBreathing)-1)
 # print("before")
  for(i in 1:(length(timeToUseForBreathing)-1))
  {
    intensityPerCycle=df[df[,"time"]>=timeToUseForBreathing[i]  & df[,"time"]<timeToUseForBreathing[i+1],"intensity"]
    maxIntensityPerCycle[i]=max(intensityPerCycle,na.rm=T)
    Inspiration[i]=df[df[,"time"]==timeToUseForBreathing[i+1] ,"intensity"]
  }
 # print("ok")
  durationPerCycle= diff(timeToUseForBreathing)
  peakTable=cbind(start=timeToUseForBreathing[-length(timeToUseForBreathing)],stop=timeToUseForBreathing[-1],duration=durationPerCycle,maxIntensity=maxIntensityPerCycle,Inspiration=Inspiration)
  # Removing time cycles lower than minimalDuration
  peakTable=as.data.frame(peakTable)
  peakTable[,"toRemove"]=FALSE
  peakTable[durationPerCycle<minimalDuration|Inspiration>maxInspi|maxIntensityPerCycle<minExpi,"toRemove"]=TRUE
  removingStrangeCycles=which(durationPerCycle<minimalDuration|Inspiration>maxInspi|maxIntensityPerCycle<minExpi)+1
 # print("removing")
  if(length(removingStrangeCycles)!=0)
  {
    timeToUseForBreathing=timeToUseForBreathing[-removingStrangeCycles]
  }
#  print("removed")
  finalPeakTable=cbind(start=timeToUseForBreathing[-length(timeToUseForBreathing)],stop=timeToUseForBreathing[-1],
                       duration=diff(timeToUseForBreathing),maxIntensity=maxIntensityPerCycle[-c(removingStrangeCycles-1)])



  # p3 is the original data frame with the selected peaks


  p3=ggplot(df,aes(x=time,y=intensity))+geom_line()+theme_bw()+ggtitle("Raw data and breathing cycle limits")
 # print("p3")
  for(k in 1:length(timeToUseForBreathing))
  {
    p3=p3+geom_vline(xintercept=timeToUseForBreathing[k],col="red")
  }
  p3=p3+geom_hline(yintercept=maxInspi,col="purple",linetype = "dotted")
  p3=p3+geom_hline(yintercept=minExpi,col="blue",linetype = "dashed")

  param=c(minExpi=minExpi,maxInspi=maxInspi,smoothMethod=smoothMethod,method=method,
          halfWindowSize=halfWindowSize,maximum=maximum,SNR=SNR,minimalDuration=minimalDuration,forMinExpiDivideMaxIntBy=forMinExpiDivideMaxIntBy,forMaxInspiDivideMaxIntBy=forMaxInspiDivideMaxIntBy)
 # print("detectCycle ok")
 return(list(cycles=timeToUseForBreathing,gg=list(p2=p2,p3=p3),finalPeakTable=finalPeakTable,peakTable=peakTable,param=param))
}
