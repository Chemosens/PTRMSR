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
#' @param detectionThreshold intensity value to be reached to be considered as a peak (default to NULL corresponds to 0)
#' @description This function allows to estimate the beginning of a peak with three different methods. All these methods depends on the selection of a peak in a specific windows (given by "StartPeriod").
#'  Sometimes, this StartPeriod contains several peaks. In this case, we select peaks that are high enough.
#' This quantity is defined as a percent of the maximale peak intensity: a value of 100 indicates that only the maximale peak is considered; a value of 0 indicates that the first detected peak is considered.
#' A value of 50 indicates that the first peak higher than 50% of the maximale intensity is considered.
#'  The user should use cautiously this option.
#'
#'  Then, three methods are available: "startPeakProportion" and "higherThanNoise" are two methods based on the time (t_imax) obtained for the maximale intensity of the peak. It research t0 as close as possible as t_imax, but with a intensity lower than a given intensity threshold.
#'  In startPeakProportion, this threshold is calculated as a proportion of the maximale intensity. This proportion (between 0 and 1) is defined in the proportionOfMax parameter.
#'  In higherThanNoise, this threshold is calculated by multiplying the noise estimated by a number (defined by multiplyNoiseBy)
#'  The noise can be estimated with different methods:
#'  - as the average or maximale intensity (statOfNoise="avg" or "max") of a given period (defined in noisePeriod)
#'  - as the estimation of blank during the total dataframe (statOfNoise="bl")
#'  - as the estimation of blank during the period defined in noisePeriod (statOfNoise="blperiod")
#'
#' A last method is based on derivation of the spectra. It finds out where the derivative of order defined by "order" parameter is maximale
#' @return a list containing:
#' - tx (the obtained starting time);
#' - tx_vec (when several starting ions -parameter starts- are selected, return tx for each starting ion);
#' - gg a ggplot object displaying the intensity along time and the starting time chosen
#' - diagnosis : 'ok' if there was no problem by selecting the start, 'maxPeakTooLow','timeFoundAfterPeak' or 'firstNotMax' if some ambiguity is possible
#' @export
#' @importFrom signal sgolayfilt
#' @examples
#' # Example without breathing correction
#'
#'data(longDf)
#' starts=longDf[1,"ion"]
#' res_higherThanNoise=ptrvDetectStart(res=longDf,starts=starts,startPeriod=c(33.6, 48),method="higherThanNoise",multiplyNoiseBy=1.5,noisePeriod=c(10,30),statOfNoise="avg",peakChoice="firstAmongHigh",nPoints=31,smooth=TRUE,firstAmongHighThreshold=50)
#' res_higherThanNoise$potentialPeaks
#' # Example with breathing correction
#' data(ptrv)
#' res_intensity=ptrvIntensityByTime(ptrv,referenceBreath="m69.06989..isoprene...Conc.",
#' correction = "cycle",removeNoise=FALSE,breathRatio = FALSE)
#' ion="m115.11139..C7H15O....Conc."
#' ds=ptrvDetectStart(res=res_intensity$res,starts=ion, method="startPeakProportion")
#' ds=ptrvDetectStart(res=res_intensity$res,starts=ion, method="higherThanNoise",startPeriod=c(20,150),noisePeriod=c(20,150))
#' ds=ptrvDetectStart(res=res_intensity$res,starts=ion, method="higherThanNoise",statOfNoise="avg",
#' multiplyNoiseBy=1.6,timeChoice="lower",noisePeriod=c(20,150))
#' ds=ptrvDetectStart(res=res_intensity$res,starts=ion, method="higherThanNoise",statOfNoise="avg",
#' multiplyNoiseBy=1.6,smooth=TRUE,startPeriod=c(20,150),noisePeriod=c(20,150),nPoints=5)
#' ds=ptrvDetectStart(res=res_intensity$res,starts=ion, method="startPeakProportion",
#' proportionOfMax=0.1,smooth=TRUE,startPeriod=c(20,150),nPoints=5)

ptrvDetectStart=function(res,starts,method="startPeakProportion",proportionOfMax=0.1,multiplyNoiseBy=3,
                         peakChoice="maxIntensity",statOfNoise="max",noisePeriod=NULL,timeChoice="interpolation",
                         startPeriod=NULL,smooth=FALSE,nPoints=1,order=1,firstAmongHighThreshold=50,
                         detectionThreshold=NULL)
{
  # Internal functions
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
  choosePeak=function(resion,startPeriod,period,peakChoice,smooth,nPoints,firstAmongHighThreshold,detectionThreshold)
  {
  # resion df containing time, intensity as column
  # nPoints : required for smoothing in pickingPeaks
  # returns t_max (to be as close as possible)
    resion[resion[,"intensity"]<0,"intensity"]=0
    resionperiod=resion[resion[,"time"]<startPeriod[2]&resion[,"time"]>startPeriod[1],]
    Imax=max(resionperiod[,"intensity"],na.rm=T)

    if(Imax<detectionThreshold)
    {
      diagnosis="maxPeakTooLow"
      t0=NA
      Ipeak=NA
      df_peaks=NULL
      t_imax=NA
    }
    else
    {
      if(peakChoice=="first")
      {
        firstAmongHighThreshold=0;peakChoice="firstAmongHigh"
        diagnosis="ok"
      }
      if(peakChoice=="maxIntensity")
      {

        Ipeak=max(resionperiod[,"intensity"],na.rm=T)
        t_imax=resionperiod[which.max(resionperiod[,"intensity"]),"time"]
        df_peaks=NA
        diagnosis="ok"
      }
      if(peakChoice=="firstAmongHigh")
      {
       # print(max(resionperiod[,"intensity"]))
        respeak=pickingPeaks(resionperiod,col="time",nPoints=nPoints,refineMz="descendPeak",method="MAD")$df

        if(nrow(respeak)==0){diagnosis="NoPeakInWindow";t_imax=NA;df_peaks=NA;Ipeak=NA}
        else
        {
          df_peaks=respeak[round(respeak[,"relative"],digits=8)>=firstAmongHighThreshold&respeak[,"intensity"]>=detectionThreshold,] # if no round, some value can be higher than 100% due to roundings
          if(nrow(df_peaks)==0)
          {
            diagnosis="maxPeakTooLow"
            df_peaks=NA
            Ipeak=NA
            df_peaks=NULL
            t_imax=NA
          }
          else
          {
            t_imax= min(df_peaks[,"x"],na.rm=T)
            Ipeak=df_peaks[df_peaks[,"x"]==t_imax,"intensity"]
            tmax=df_peaks[which.max(df_peaks[,"intensity"]),"x"]
            if(t_imax!=tmax){ diagnosis="firstNotMax"}else{diagnosis="ok"}
          }

        }

      }
    }

    return(list(t_peak=t_imax,df_peaks=df_peaks,diagnosis=diagnosis,Ipeak=Ipeak,Imax=Imax))
  }
  chooseTime=function(resion,t0,ind,Ix,timeChoice)
  {
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
    return(tx)
  }
  # Working with arguments
  match.arg(method,c("startPeakProportion","higherThanNoise","higherDerivative"))
  if(is.null(detectionThreshold)){detectionThreshold=0}
  if(is.null(startPeriod)){startPeriod=c(min(res[,"time"],na.rm=T),max(res[,"time"],na.rm=T))}
  if(method=="higherThanNoise"&statOfNoise=="blperiod"&is.null(noisePeriod)){stop("Please enter a noise period for statOfNoise='blperiod'")}
  if(method=="higherThanNoise"&statOfNoise=="avg"&is.null(noisePeriod)){stop("Please enter a noise period for statOfNoise='avg'")}
  if(method=="higherThanNoise"&statOfNoise=="max"&is.null(noisePeriod)){stop("Please enter a noise period for statOfNoise='max'")}
  if(smooth==FALSE && nPoints!=0){nPoints=0;message("smooth = FALSE and nPoints!=0; nPoints was replaced by 0")}
  # Initializations
  intensity=sg1=sg2=sg3=sg4=intensity=NULL
  ion=time=NULL
  tx_vec=rep(NA,length(starts));names(tx_vec)=starts
  diagnosis=tx_vec
  resiongg=data.frame()
 #print(startPeriod)

 # if(method=="startPeakProportion")
 # {
    for(start in starts)
    {
      resion=res[res[,"ion"]==start,]
      resion=resion[!is.na(resion[,"intensity"]),]
      resion=resion[order(resion[,"time"]),]
      if(smooth)
      {
        sgolay0=sgolayfilt(x=resion[,"intensity"], p = 3,n=nPoints,m=0)
        resion[,"intensity"]=sgolay0
        resion[resion[,"intensity"]<0,"intensity"]=0
      }
    #  print(detectionThreshold)
      # Choice of the peak
        resPeakChoice=choosePeak(resion,startPeriod=startPeriod,peakChoice=peakChoice,smooth=smooth,nPoints=nPoints,firstAmongHighThreshold=firstAmongHighThreshold,detectionThreshold=detectionThreshold)
        t_peak=resPeakChoice$t_peak
        Ipeak=resPeakChoice$Ipeak
        df_peaks=resPeakChoice$df_peaks
        diagnosis[start]=resPeakChoice$diagnosis
        Imin=max(min(resion[resion[,"time"]<=startPeriod[2]&resion[,"time"]>=startPeriod[1],"intensity"],na.rm=T),0)


      # Choice of the threshold
        if(method=="startPeakProportion")
        {
          Ix=Imin+proportionOfMax*(Ipeak-Imin)
        }
        if(method=="higherThanNoise")
        {
          if(statOfNoise=="max")
          {
            statNoise=ptrvIntensity(resion,timePeriod=noisePeriod)
            intensity_threshold=multiplyNoiseBy*statNoise[,"max"]
          }
          if(statOfNoise=="avg")
          {
            statNoise=ptrvIntensity(resion,timePeriod=noisePeriod)
            intensity_threshold=multiplyNoiseBy*statNoise[,"moy"]
          }
          if(statOfNoise=="bl")
          {
            sp2=new("Spectrum1",mz=resion[,"time"],intensity=resion[,"intensity"],centroided = FALSE)
            intensity_threshold=multiplyNoiseBy*MSnbase::estimateNoise(sp2)[1,"intensity"]
          }
          if(statOfNoise=="blperiod")
          {
            inPeriod=resion[,"time"]<noisePeriod[2]&resion[,"time"]>noisePeriod[1]
            sp2=new("Spectrum1",mz=resion[inPeriod,"time"],intensity=resion[inPeriod,"intensity"],centroided = FALSE)
            intensity_threshold=multiplyNoiseBy*MSnbase::estimateNoise(sp2)[1,"intensity"]
          }
          Ix=intensity_threshold
        }
      # Selection of the time related to the beginning of the chosen peak
        if(method=="higherThanNoise"||method=="startPeakProportion")
        {
          tt=resion[,"intensity"]-Ix
          ressign=sign(tt[-length(tt)]*tt[-1])==-1
          minStart=closestTime(resion[-length(tt),"time"],startPeriod[1],option="lower")
          maxStart=closestTime(resion[-1,"time"],startPeriod[2],option="higher")
          resint=(resion[-1,"time"]<=maxStart&resion[-length(tt),"time"]>=minStart)
          indexes=which(ressign&resint)
          if(diagnosis[start]!="maxPeakTooLow")
          {
            if(length(indexes)==0)
            {
              diagnosis[start]="IntensityNotReached"
            }
            if(length(indexes)!=0)
            {
              if(!is.na(t_peak))
              {
                t0=closestTime(resion[indexes,"time"],number=t_peak,option="lower")
                if(!is.na(t0))
                {
                  if(t0<=t_peak)
                  {
                    ind=which(resion[,"time"]==t0)
                    tx_tmp=chooseTime(resion=resion,t0=t0,ind=ind,Ix=Ix,timeChoice=timeChoice)
                  }
                  if(t0>t_peak){ t0=tx_tmp=NA; diagnosis[start]="IntensityNotReached"}
                }else{tx_tmp=NA}
              }else{tx_tmp=NA}
            }else{tx_tmp=NA}
          }else{tx_tmp=NA}
        }else{tx_tmp=NA}
        if(method=="higherDerivative")
        {
          inPeriod=resion[,"time"]<startPeriod[2]&resion[,"time"]>startPeriod[1]
          sgolay=sgolayfilt(x=resion[,"intensity"], p = 3,n=nPoints,m=order)
          dfsg=data.frame(time=resion[,"time"],sg=sgolay,intensity=resion[,"intensity"])
          colnames(dfsg)=c("time","sg","intensity")
          dfsgPeriod=dfsg[inPeriod,]
          tx_tmp=dfsgPeriod[which.max(dfsgPeriod[,"sg"]),"time"]
        }

      tx_vec[start]=tx_tmp
      resiongg=rbind(resiongg,resion)
     }
    tx=min(tx_vec,na.rm=T)
    gg=ggplot(resiongg,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme_bw()+geom_vline(xintercept=tx)
 # }

  # if(method=="higherThanNoise")
  # {
  #   for(start in starts)
  #   {
  #     resion=res[res[,"ion"]==start,]
  #     resion=resion[!is.na(resion[,"intensity"]),]
  #     resion=resion[order(resion[,"time"]),]
  #     if(smooth)
  #     {
  #       sgolay0=sgolayfilt(x=resion[,"intensity"], p = 3,n=nPoints,m=0)
  #       resion[,"intensity"]=sgolay0
  #       resion[resion[,"intensity"]<0,"intensity"]=0
  #     }
  #
  #
  #     tt=resion[,"intensity"]-Ix # intensity minus the intensity_threshold
  #     ressign=sign(tt[-length(tt)]*tt[-1])==-1&(resion[-1,"time"]<=startPeriod[2]&resion[-length(tt),"time"]>=startPeriod[1])
  #     indexes=which(ressign)
  #     if(length(indexes)>=1)
  #     {
  #       resionperiod=resion[resion[,"time"]<startPeriod[2]&resion[,"time"]>startPeriod[1],]
  #       resPeakChoice=choosePeak(resion,startPeriod=startPeriod,peakChoice=peakChoice,nPoints=nPoints,firstAmongHighThreshold=firstAmongHighThreshold,detectionThreshold=detectionThreshold)
  #       t0=resPeakChoice$t0
  #       ind=resPeakChoice$ind
  #       diagnosis[start]=resPeakChoice$diagnosis
  #
  #      # if(peakChoice=="first")
  #       #{
  #       #  ind=which(ressign)[1]
  #       #  t0=resion[ind,"time"]
  #       #}
  #       if(peakChoice=="firstAmongHigh")
  #       {
  #         print("in")
  #
  #       }
  #      tx=chooseTime(resion,t0,ind,Ix,timeChoice)
  #
  #
  #     #  t0=resion[ind,"time"]
  #
  #    }
  #    else
  #    {tx=NA;diagnosis[start]="ThresholdNotReached"}
  #     resiongg=rbind(resiongg,resion)
  #     tx_vec[start]=tx
  #   }
  #   #if(sum(!is.na(tx_vec))==0){tx=startPeriod[1]}else{  tx=min(tx_vec,na.rm=T)}
  #   tx=min(tx_vec,na.rm=T)
  #     #statGlobal=ptrvIntensity(res$res,periodOfInterest=noisePeriod)
  #   gg=ggplot(resiongg,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme_bw()+geom_vline(xintercept=tx)
  #   tx=min(tx_vec,na.rm=T)
  # }

  # if(method=="higherDerivative")
  # {
  #   for(start in starts)
  #   {
  #     resion=res[res[,"ion"]==start,]
  #     resion=resion[!is.na(resion[,"intensity"]),]
  #     resion=resion[order(resion[,"time"]),]
  #
  #     #sgolay2=sgolayfilt(x=resion[,"intensity"], p = 3,n=nPoints,m=2)
  #     #sgolay1=sgolayfilt(x=resion[,"intensity"], p = 3,n=nPoints,m=1)
  #      #sgolay3=sgolayfilt(x=resion[,"intensity"], p = 5,n=nPoints,m=3)
  #     #sgolay4=sgolayfilt(x=resion[,"intensity"], p = 6,n=nPoints,m=4)
  #
  #
  #     inPeriod=resion[,"time"]<startPeriod[2]&resion[,"time"]>startPeriod[1]
  #     sgolay=sgolayfilt(x=resion[,"intensity"], p = 3,n=nPoints,m=order)
  #     dfsg=data.frame(time=resion[,"time"],sg=sgolay,intensity=resion[,"intensity"])
  #     colnames(dfsg)=c("time","sg","intensity")
  #     dfsgPeriod=dfsg[inPeriod,]
  #     tx=dfsgPeriod[which.max(dfsgPeriod[,"sg"]),"time"]
  #     # dfsgPeriod2=dfsgPeriod[dfsgPeriod[,"time"]<tx1,]
  #     # tx2=dfsgPeriod2[which.max(dfsgPeriod2[,"sg2"]),"time"]
  #     #
  #     # dfsgPeriod3=dfsgPeriod2[dfsgPeriod2[,"time"]<tx2,]
  #     # tx3=dfsgPeriod3[which.max(dfsgPeriod3[,"sg3"]),"time"]
  #     # dfsgPeriod4=dfsgPeriod3[dfsgPeriod3[,"time"]<tx3,]
  #     # tx=dfsgPeriod4[which.max(dfsgPeriod4[,"sg4"]),"time"]
  #      tx_vec[start]=tx
  #
  #   }
  #
  #
  # gg=ggplot(dfsg,aes(x=time,y=intensity))+geom_line()+theme_bw()
  # gg=gg+geom_line(data=dfsg,aes(x=time,y=sg,color="green"))
  #
  # }
  diagnosis[diagnosis==""]="ok"
  listRes=list(tx=tx,tx_vec=tx_vec,gg=gg,diagnosis=diagnosis)
  if(method!="higherDerivative"){listRes[["potentialPeaks"]]=df_peaks;listRes[["intensityThreshold"]]=Ix}
  return(listRes)
}
