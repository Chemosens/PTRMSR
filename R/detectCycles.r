#' @param maxPeaks minimal intensity of the original curve (not the opposite) required to be considered as a peak
#' @param smoothMethod smoothing method. Among "MovingAverage" or "SavitzkyGolay". See MSnbase::smooth for more details.
#' @param method method to estimate the noise "MAD" or ""
#' @param SNR signal noise ratio (0 by default) required to be considered as a peak
#' @param minimalDuration minimalDuration of a breathing cycle (2 by default - as a accelerated breathing frequency corresponds to 20 cycles and more by minut and a low frequency is 12 and less -)
#'
#'
#' @export
detectCycle=function(res1,maxPeaks=NULL,smoothMethod="MovingAverage",method="MAD",halfWindowSize=5,maximum=NULL,SNR=0,minimalDuration=2)
{
  maxInt=max(res1[!is.na(res1[,"intensity"]),"intensity"])
  sp1=new("Spectrum1",mz=res1[!is.na(res1[,"intensity"]),"time"],intensity=maxInt-res1[!is.na(res1[,"intensity"]),"intensity"],centroided=F)
  if(is.null(maxPeaks)){maxPeaks=maxInt/5}
  max2=maxInt-maxPeaks
  sp2=MSnbase::smooth(sp1,method=smoothMethod,halfWindowSize = halfWindowSize)
  dfsp2=data.frame(time=MSnbase::mz(sp1),intensity=maxInt-MSnbase::intensity(sp2))
  p2=ggplot(dfsp2,aes(x=time,y=intensity))+geom_line()+theme_bw()+ggtitle("Smoothed data for breathing")
  respicks=MSnbase::pickPeaks(sp2,method=method,SNR=SNR)
  timeToUseForBreathing=unique(c(0,MSnbase::mz(respicks)[MSnbase::intensity(respicks)>max2]))
  removingTinyCycles=which(diff(timeToUseForBreathing)<minimalDuration)+1
  if(length(removingTinyCycles)!=0)
  {
    timeToUseForBreathing=timeToUseForBreathing[-removingTinyCycles]
  }

  if(!is.null(maximum)){ timeToUseForBreathing=unique(c(0,timeToUseForBreathing,maximum))}
  p3=ggplot(res1,aes(x=time,y=intensity))+geom_line()+theme_bw()+ggtitle("Raw data and breathing cycle limits")
  for(k in 1:length(timeToUseForBreathing))
  {
    p3=p3+geom_vline(xintercept=timeToUseForBreathing[k],col="red")
  }
  return(list(cycles=timeToUseForBreathing,gg=list(p2=p2,p3=p3)))
}
