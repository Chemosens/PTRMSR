#' @title ptrvDetectStart
#' @param starts name of the ions to be used to detect the starting point
#' @param res result of ptrvIntensityByTime$res (data.frame with time, intensity and ion as columns)
#' @param method "startPeakProportion" "higherThanNoise" "tangential" TODO
#' @param proportionOfMax Number between 0 and 1. If method startPeakProportion is chosen and length(starts)==1, the starting time will be the first time that the intensity reaches this proportion of the maximale intensity of the peak. If length(starts) > 1, the starting time will be the minimum value obtained with this method for all starts.
#' @param noisePeriod Vector of two numbers. If method "higherThanNoise" is chosen and length(starts)==1, the starting time will be the first time that the intensity reaches 3*the maximal value obtained during the noise period. If length(starts) > 1, the starting time will be the minimum value obtained with this method for all starts.
#' @param startPeriod Vector of two numbers. The starting time obtained has to be in this interval.
#' @param multiplyNoiseBy Number higher than 1 (nb). If method "higherThanNoise" is chosen, the maximal (or averaged according to noiseStat) value obtained during the noise period is multiplied by this number to give the intensity to reach for returning the starting period.
#' @param statOfNoise "avg" or "max". If method "higherThanNoise" is chosen, the statistic obtained during the noise period to be multiplied by multiplyNoiseBy for returning the starting period.
#' @param timeChoice "interpolation", "lower","upper". The time obtained for reaching a given value can be a not observed time (but in a interval of two observed times). "interpolation" returns the not observed time, "lower" returns the lower limit of this interval, "upper" returns the upper limit of this interval
#' @param smooth If TRUE, the data is previously smoothed with the parameter halfWindowsSize
#' @param nPoints Number of points taken for smoothing
#' @param peakChoice "maxIntensity", "first" or "firstAmongHigh"
#' @param firstAmongHighThreshold percentage of the maximum value to be reached in order to be considered as a peak when peak choice is firstAmongHigh
#' @param order order of the derivative
#' @export
#' @importFrom signal sgolayfilt
#' @examples
#' data(ptrv)
#' res_intensity=ptrvIntensityByTime(ptrv,referenceBreath="m69.06989..isoprene...Conc.",
#' correction = "cycle",removeNoise=FALSE,breathRatio = FALSE)
#' ion="m115.11139..C7H15O....Conc."
#' ds=ptrvDetectStart(res=res_intensity$res,starts=ion, method="startPeakProportion")
#' ds=ptrvDetectStart(res=res_intensity$res,starts=ion, method="higherThanNoise",startPeriod=c(20,150))
#' ds=ptrvDetectStart(res=res_intensity$res,starts=ion, method="higherThanNoise",statOfNoise="avg",
#' multiplyNoiseBy=1.6,timeChoice="lower")
#' ds=ptrvDetectStart(res=res_intensity$res,starts=ion, method="higherThanNoise",statOfNoise="avg",
#' multiplyNoiseBy=1.6,smooth=TRUE,startPeriod=c(20,150))
#' ds=ptrvDetectStart(res=res_intensity$res,starts=ion, method="startPeakProportion",
#' proportionOfMax=0.1,smooth=TRUE,startPeriod=c(20,150))

ptrvDetectStart=function(res,starts,method="startPeakProportion",proportionOfMax=0.1,multiplyNoiseBy=3,peakChoice="maxIntensity",statOfNoise="max",noisePeriod=c(0,20),timeChoice="interpolation",startPeriod=c(20,50),smooth=FALSE,nPoints=7,order=1,firstAmongHighThreshold=50)
{
  order=NULL
 intensity=sg1=sg2=sg3=sg4=intensity=NULL
  closestTime=function(vec,number,option="lower")
  {
    vec=vec[order(vec)]
    diff=vec-number

    if(option=="lower")
    {
      diffneg=which(diff<=0)
      if(length(diffneg)==0){return(vec[length(vec)])}else{      return(vec[diffneg[length(diffneg)]])}

    }
    if(option=="higher")
    {
      diffpos=which(diff>=0)
      if(length(diffpos)==0){return(vec[1])}else
      {
        return(vec[diffpos[1]])
      }

    }
  }


  ion=time=NULL
  tx_vec=rep(NA,length(starts));names(tx_vec)=starts
  diagnosis=tx_vec
  resiongg=data.frame()

  if(method=="startPeakProportion")
  {
     for(start in starts)
    {

      resion=res[res[,"ion"]==start,]
      resion=resion[!is.na(resion[,"intensity"]),]
      resion=resion[order(resion[,"time"]),]

      if(smooth)
      {
       # sp1=new("Spectrum1",mz=resion[,"time"],intensity=resion[,"intensity"])
       # sp2=MSnbase::smooth(sp1,method="MovingAverage",halfWindowSize = halfWindowSize)
      # ion=resion[1,"ion"]
       # resion=data.frame(time=MSnbase::mz(sp2),intensity=MSnbase::intensity(sp2),ion=ion)

        sgolay0=sgolayfilt(x=resion[,"intensity"], p = 3,n=nPoints,m=0)
        resion[,"intensity"]=sgolay0
        resion[resion[,"intensity"]<0,"intensity"]=0

      }

      #plot(smoot$x[-1],diff(smoot$y)-1,col="red",type="l")

      Ix=min(resion[,"intensity"],na.rm=T)+proportionOfMax*(max(resion[,"intensity"],na.rm=T)-min(resion[,"intensity"],na.rm=T))
      tt=resion[,"intensity"]-Ix
      ressign=sign(tt[-length(tt)]*tt[-1])==-1
      minStart=closestTime(resion[-length(tt),"time"],startPeriod[1],option="lower")
      maxStart=closestTime(resion[-1,"time"],startPeriod[2],option="higher")
      resint=(resion[-1,"time"]<=maxStart&resion[-length(tt),"time"]>=minStart)
      indexes=which(ressign&resint)


      if(sum(ressign&resint)!=0)
      {

        if(peakChoice=="maxIntensity")
        {
          resionperiod=resion[resion[,"time"]<=maxStart&resion[,"time"]>=minStart,]
          t_imax=resionperiod[which.max(resionperiod[,"intensity"]),"time"]
          t0=closestTime(resion[indexes,"time"],number=t_imax,option="lower")
          ind=which(resion[,"time"]==t0)

        }
        if(peakChoice=="first")
        {
          ind=which(ressign&resint)[1]
          t0=resion[ind,"time"]
        }
        if(peakChoice=="firstAmongHigh")
        {
          resionperiod=resion[resion[,"time"]<startPeriod[2]&resion[,"time"]>startPeriod[1],]
          respeak=pickingPeaks(resionperiod,col="time",nPoints=nPoints,refineMz="descendPeak",method="MAD")$df
          t_imax= min(respeak[respeak[,"relative"]>firstAmongHighThreshold,"x"])
          t0=closestTime(resion[indexes,"time"],number=t_imax,option="lower")
          if(t0>t_imax){t0=NA}
          if(!is.na(t0))
          {
            ind=which(resion[,"time"]==t0)
          }
        }
        if(!is.na(t0))
        {
          t1=resion[ind+1,"time"]
          I0=resion[ind,"intensity"]
          I1=resion[ind+1,"intensity"]
          a=(I1-I0)/(t1-t0)
          b=I1-a*t1
          if(timeChoice=="interpolation")
          {
            tx_tmp=(1/a)*(Ix-b)
          }
          if(timeChoice=="lower")
          {
            tx_tmp=t0
          }
          if(timeChoice=="upper")
          {
            tx_tmp=t1
          }
        }
        else{tx_tmp=NA}

      }
      else{tx_tmp=NA}

      tx_vec[start]=tx_tmp

    resiongg=rbind(resiongg,resion)




     }


    #if(sum(!is.na(tx_vec))==0){tx=startPeriod[1]}else{  tx=min(tx_vec,na.rm=T)}
    tx=min(tx_vec,na.rm=T)
    gg=ggplot(resiongg,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme_bw()+geom_vline(xintercept=tx)

  }

  if(method=="higherThanNoise")
  {

    for(start in starts)
    {

      resion=res[res[,"ion"]==start,]
      resion=resion[!is.na(resion[,"intensity"]),]
      resion=resion[order(resion[,"time"]),]
      if(smooth)
      {
        #sp1=new("Spectrum1",mz=resion[,"time"],intensity=resion[,"intensity"],centroided = FALSE)
        #sp2=MSnbase::smooth(sp1,method="MovingAverage",halfWindowSize = halfWindowSize)
       # ion=resion[1,"ion"]
       # resion=data.frame(time=MSnbase::mz(sp2),intensity=MSnbase::intensity(sp2),ion=ion)
        sgolay0=sgolayfilt(x=resion[,"intensity"], p = 3,n=nPoints,m=0)
        resion[,"intensity"]=sgolay0
        resion[resion[,"intensity"]<0,"intensity"]=0

      }
      statNoise=ptrvIntensity(resion,timePeriod=noisePeriod)

      if(statOfNoise=="max")
      {
        intensity_threshold=multiplyNoiseBy*statNoise[,"max"]
      }
      if(statOfNoise=="avg")
      {
        intensity_threshold=multiplyNoiseBy*statNoise[,"moy"]
      }
      if(statOfNoise=="bl")
      {
        sp2=new("Spectrum1",mz=resion[,"time"],intensity=resion[,"intensity"],centroided = FALSE)
        intensity_threshold=multiplyNoiseBy*MSnbase::estimateNoise(sp2)[1,"intensity"]
      }
      if(statOfNoise=="blperiod")
      {
        inPeriod=resion[,"time"]<startPeriod[2]&resion[,"time"]>startPeriod[1]
        sp2=new("Spectrum1",mz=resion[inPeriod,"time"],intensity=resion[inPeriod,"intensity"],centroided = FALSE)
        intensity_threshold=multiplyNoiseBy*MSnbase::estimateNoise(sp2)[1,"intensity"]

      }
      Ix=intensity_threshold
      tt=resion[,"intensity"]-Ix
     ressign=sign(tt[-length(tt)]*tt[-1])==-1&(resion[-1,"time"]<=startPeriod[2]&resion[-length(tt),"time"]>=startPeriod[1])

     # minStart=closestTime(resion[-length(tt),"time"],startPeriod[1],option="lower")
     # maxStart=closestTime(resion[-1,"time"],startPeriod[2],option="higher")
     # resint=(resion[-1,"time"]<=maxStart&resion[-length(tt),"time"]>=minStart)

      #  ind=which(ressign)[1]
     indexes=which(ressign)
     if(length(indexes)>=1)
     {
        if(peakChoice=="maxIntensity")
        {
          resionperiod=resion[resion[,"time"]<startPeriod[2]&resion[,"time"]>startPeriod[1],]
          t_imax=resionperiod[which.max(resionperiod[,"intensity"]),"time"]
          t0=closestTime(resion[indexes,"time"],number=t_imax,option="lower")
          ind=which(resion[,"time"]==t0)

        }
        if(peakChoice=="first")
        {
          ind=which(ressign)[1]
          t0=resion[ind,"time"]
        }
        if(peakChoice=="firstAmongHigh")
        {
          resionperiod=resion[resion[,"time"]<startPeriod[2]&resion[,"time"]>startPeriod[1],]
          respeak=pickingPeaks(resionperiod,col="time",nPoints=nPoints,refineMz="descendPeak",method="MAD")$df
          t_imax= min(respeak[respeak[,"relative"]>firstAmongHighThreshold,"x"])
          t0=closestTime(resion[indexes,"time"],number=t_imax,option="lower")

          if(t0>t_imax){t0=NA}
          if(!is.na(t0))
          {
            ind=which(resion[,"time"]==t0)
          }
        }

       if(!is.na(t0))
       {
         t1=resion[ind+1,"time"]
         I0=resion[ind,"intensity"]
         I1=resion[ind+1,"intensity"]
         a=(I1-I0)/(t1-t0)
         b=I1-a*t1
         if(timeChoice=="interpolation")
         {
           tx=(1/a)*(Ix-b)
         }
         if(timeChoice=="lower")
         {
           tx=t0
         }
         if(timeChoice=="upper")
         {
           tx=t1
         }
       }
       else{tx=NA}
      #  t0=resion[ind,"time"]

     }
     else
     {tx=NA}
      resiongg=rbind(resiongg,resion)
      tx_vec[start]=tx
    }

    #if(sum(!is.na(tx_vec))==0){tx=startPeriod[1]}else{  tx=min(tx_vec,na.rm=T)}
    tx=min(tx_vec,na.rm=T)
      #statGlobal=ptrvIntensity(res$res,periodOfInterest=noisePeriod)
  gg=ggplot(resiongg,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme_bw()+geom_vline(xintercept=tx)

  tx=min(tx_vec,na.rm=T)
  }

  if(method=="higherDerivative")
  {

    for(start in starts)
    {

      resion=res[res[,"ion"]==start,]
      resion=resion[!is.na(resion[,"intensity"]),]
      resion=resion[order(resion[,"time"]),]

     # Ix=intensity_threshold
      # minStart=closestTime(resion[-length(tt),"time"],startPeriod[1],option="lower")
      # maxStart=closestTime(resion[-1,"time"],startPeriod[2],option="higher")
      # resint=(resion[-1,"time"]<=maxStart&resion[-length(tt),"time"]>=minStart)


      sgolay2=sgolayfilt(x=resion[,"intensity"], p = 3,n=nPoints,m=2)
      sgolay1=sgolayfilt(x=resion[,"intensity"], p = 3,n=nPoints,m=1)
      sgolay0=sgolayfilt(x=resion[,"intensity"], p = 3,n=nPoints,m=0)
      sgolay3=sgolayfilt(x=resion[,"intensity"], p = 5,n=nPoints,m=3)
      sgolay4=sgolayfilt(x=resion[,"intensity"], p = 6,n=nPoints,m=4)


      inPeriod=resion[,"time"]<startPeriod[2]&resion[,"time"]>startPeriod[1]
      dfsg=data.frame(time=resion[,"time"],sg0=sgolay0,sg1=sgolay1,sg2=sgolay2,sg3=sgolay3,sg4=sgolay4,intensity=resion[,"intensity"])
      colnames(dfsg)=c("time","sg0","sg1","sg2","sg3","sg4","intensity")

      dfsgPeriod=dfsg[inPeriod,]
      tx1=dfsgPeriod[which.max(dfsgPeriod[,"sg1"]),"time"]
      dfsgPeriod2=dfsgPeriod[dfsgPeriod[,"time"]<tx1,]
      tx2=dfsgPeriod2[which.max(dfsgPeriod2[,"sg2"]),"time"]

      dfsgPeriod3=dfsgPeriod2[dfsgPeriod2[,"time"]<tx2,]
      tx3=dfsgPeriod3[which.max(dfsgPeriod3[,"sg3"]),"time"]
      dfsgPeriod4=dfsgPeriod3[dfsgPeriod3[,"time"]<tx3,]
      tx4=dfsgPeriod4[which.max(dfsgPeriod4[,"sg4"]),"time"]
      if(order==1){    tx_vec[start]=tx1}
      if(order==2){    tx_vec[start]=tx2}
      if(order==3){    tx_vec[start]=tx3}
      if(order==4){    tx_vec[start]=tx4}
    }

    if(sum(!is.na(tx_vec))==0){tx=startPeriod[1]}else{  tx=min(tx_vec,na.rm=T)}

  gg=ggplot(dfsg,aes(x=time,y=intensity))+geom_line()+theme_bw()
  gg=gg+geom_line(data=dfsg,aes(x=time,y=sg2,color="green"))
  gg=gg+geom_line(data=dfsg,aes(x=time,y=sg1),color="blue")
  gg=gg+geom_line(data=dfsg,aes(x=time,y=sg3),color="pink")
  gg=gg+geom_line(data=dfsg,aes(x=time,y=sg4),color="yellow")

  }
  return(list(tx=tx,tx_vec=tx_vec,gg=gg))
}
