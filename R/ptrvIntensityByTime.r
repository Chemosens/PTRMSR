#' @title ptrvIntensityByTime
#' @description Calculates the intensity by time for PTRViewer files. The calculation can be done by breathing cycle or on raw data.Breathing cycles are calculated on a reference ion (referenceBreath) with peak picking algorithms
#' @param dataset whose names are IN THIS ORDER: RelTime, Cycle, Ion1,... ,Ionp.
#' @param referenceBreath name of the ion taken as reference for breathing
#' @param correction "none" or "cycle". See Details.
#' @param timePeriod vector containing starting and stopping point to get the statistics
#' @param timeStart timeStart is a value to be removed of the relTime column
#' @param removeNoise if TRUE, the averaged intensity obtained during the timeBlank period is removed from the signal
#' @param halfWindowSize parameter used to determine the smooth for the peak-picking used in break detection
#' @param total if TRUE, the total intensity is calculated and added as a supplementary column
#' @param breathRatio if TRUE, the ratio between the intensity of ion and the breath is calculated
#' @param method method used for peak picking ("MAD" by default)
#' @param SNR Signal noise ratio parameter used in peak picking
#' @param ions vector contatining all ions to be used in the analysis (by default all ions are used)
#' @param timeBlank vector of two numbers. Indicates the interval to take as a blank period.
#' @param smoothMethod NULL, "SavitzkyGolay" or "MovingAverage"
#' @param funAggregate "mean", "maximal" or "sum"
#' @param timeCol name of the column of dataset containing the time
#' @param colToRemove vector containing the names of the columns of datasets to be removed in the analysis (column that are neither the time nor ions names)
#' @details if 'cycle' is selected in correction option, the breathing cycles are calculated
#' @return a list containing the obtained result (res), ggplot object containing different plots (gg) and, if correction='cycle' the cycle results are also added (detectCycle)
#' @export
#' @inheritParams detectCycle
#' @importFrom stats reshape
#' @importFrom MSnbase smooth
#' @importFrom reshape2 dcast
ptrvIntensityByTime=function(dataset,timeCol="RelTime",colToRemove=c("AbsTime","Cycle"),correction="none",referenceBreath=NULL,timePeriod=NULL,
                             ions=NULL,funAggregate="mean",removeNoise=TRUE,timeBlank=c(0,30),total=FALSE,breathRatio=FALSE,
                             minExpi=NULL,maxInspi=NULL,smoothMethod="MovingAverage",minimalDuration=2,forMinExpiDivideMaxIntBy=4,forMaxInspiDivideMaxIntBy=5,halfWindowSize=5, method="MAD",SNR=0,timeStart=0)
{

  time=intensity=NULL
  p_sc=p_breath=NULL
  match.arg(correction,c("none","cycle"))
  if(is.null(referenceBreath)&correction=="cycle"){correction="none";print("No referenceBreath, the 'none' correction is chosen.")}
  if(is.null(ions)){
    indToRemove=which(colnames(dataset)%in%c(timeCol,colToRemove))
    if(length(indToRemove)>0)
    {
      ions=colnames(dataset)[-indToRemove]
    }

    }else{ions=unique(c(referenceBreath,ions))}
  if(any(colnames(dataset)%in%colToRemove)){dataset=dataset[,-which(colnames(dataset)%in%colToRemove)]}
  if(correction=="cycle")
  {
    if(!referenceBreath%in%colnames(dataset)){
      print(colnames(dataset))
      print("reference")
      print(referenceBreath)
      stop("No breathing ion in the dataset")}
  }
  if(sum(ions%in%colnames(dataset))!=length(ions)){print(ions%in%colnames(dataset));stop("some ions are not in the dataset")}
  dataset=dataset[,c(timeCol,ions)]

  if(!is.null(timePeriod))
  {
      dataset=dataset[dataset[,timeCol]<timePeriod[2]&dataset[,timeCol]>timePeriod[1],]
  }
  dataset[,timeCol]=as.numeric(as.character(dataset[,timeCol]))
  dataset[,timeCol]=dataset[,timeCol]-timeStart
  indexIons=which(colnames(dataset)%in%ions)
  if(length(ions)>1)
  {
    dataset[,ions]=apply(dataset[,ions],2,as.numeric)
  }
  else{dataset[,ions]=as.numeric(dataset[,ions])}

  if(total)
  {
    dataset[,"total"]=apply(dataset[,ions],1,sum)
    indexIons2=c(indexIons,which(colnames(dataset)=="total"))
  }
  else{indexIons2=indexIons}

  if(correction=="none")
  {
    if(breathRatio)
    {
     dataset[,indexIons2]=sweep(dataset[,indexIons2],1,dataset[,referenceBreath],FUN="/")
    }
    dataset=as.data.frame(dataset)
    dataset[,"duration"]=c(dataset[-1,timeCol]-dataset[-c(length(dataset[,timeCol])),timeCol],0)
     res=reshape(dataset,direction="long",timevar="ion",varying=list(indexIons2),times=colnames(dataset)[indexIons2],v.names="intensity")
    colnames(res)=c("time","duration","ion","intensity","id")
    res2=res

    p_sc=ggplot(res,aes(x=time,y=intensity,group=ion,color=ion,name=ion))+geom_line()+theme_bw()+theme(legend.position="none")
    if(is.null(referenceBreath))
    {
      p_breath=ggplot(res[res[,"ion"]==referenceBreath,],aes(x=time,y=intensity))+geom_line()+theme_bw()+theme(legend.position="none")
    }
    else
    { p_breath=NULL}
    return(list(res=res2,gg=list(p_sc,p_breath)))
  }
  if(correction=="cycle")
  {

    dataset=as.data.frame(dataset)
    dataset[,timeCol]=as.numeric(as.character(dataset[,timeCol]))

    res=reshape(dataset,direction="long",varying=list(ions),timevar="ion",times=ions,v.names="intensity")
    colnames(res)=c("time","ion","intensity","id")

    if(!referenceBreath%in%unique(res[,"ion"])){
      print(unique(res[,"ion"]))
      print("reference")
      print(referenceBreath)
      stop("the reference breath is not one column of the dataset")
      }

    res1=res[res[,"ion"]==as.character(referenceBreath),c("time","intensity")]
    colnames(res1)=c("time","intensity")
    #print(res1)
    cycles=detectCycle(df=res1,smoothMethod=smoothMethod,method=method,halfWindowSize=halfWindowSize,timePeriod=timePeriod,SNR=SNR,minimalDuration=minimalDuration, minExpi=minExpi,maxInspi=maxInspi,forMinExpiDivideMaxIntBy=forMinExpiDivideMaxIntBy,forMaxInspiDivideMaxIntBy=forMaxInspiDivideMaxIntBy)
   # cycles=detectCycle(df=res1,maxPeaks=maxPeaks,smoothMethod=smoothMethod,method=method,halfWindowSize=5,maximum=max(dataset[,"RelTime"]),SNR=SNR,minimalDuration=0.5)

    gg_cycles=cycles$gg
    timeToUseForBreathing=cycles$cycles
  #  print(timeToUseForBreathing)
    if(length(timeToUseForBreathing)>1)
    {
      timeLab=1/2*(timeToUseForBreathing[-1]+timeToUseForBreathing[-length(timeToUseForBreathing)])
      timeLab[timeLab<timePeriod[1]]=timePeriod[1]
      timeLab[timeLab>timePeriod[2]]=timePeriod[2]
      duration=diff(timeToUseForBreathing)
      df_duration=data.frame(time=timeLab,duration=duration)
      res[,"cycle"]=cut(res[,"time"],breaks=timeToUseForBreathing,labels=timeLab)
      res=res[,-which(colnames(res)=="id")]
      res=res[!is.na(res[,"intensity"]),]
      res=res[,c("cycle","ion","intensity")]
      # Statistics by cycle
    if(funAggregate=="sum")
    {
      resultMeanT=dcast(res,formula=cycle+ion~.,value.var="intensity",fun.aggregate=function(x){return(sum(x,na.rm=T))})
    }
    if(funAggregate=="mean")
    {
      resultMeanT=dcast(res,formula=as.formula(cycle+ion~.),value.var="intensity",fun.aggregate=function(x){return(mean(x,na.rm=T))})
    }

    if(funAggregate=="max")
    {
      resultMeanT=dcast(res,formula=cycle+ion~.,value.var="intensity",fun.aggregate=function(x){return(max(x,na.rm=T))})
    }

    colnames(resultMeanT)=c("time","ion","intensity")
    resultMeanT=merge(resultMeanT[!is.na(resultMeanT[,"time"]),],df_duration,by="time",all.x=T)
    resultMeanT[,"time"]=as.numeric(as.character(resultMeanT[,"time"]))
    maxNoise=sdNoise=avgNoise=rep(NA,length(unique(res[,"ion"])));
    names(avgNoise)=names(maxNoise)=names(sdNoise)=unique(res[,"ion"])
  #  print("beforeRemoved")
    if(removeNoise)
    {
      if(!is.null(timePeriod))
      {
        if(timeBlank[1]<timePeriod[1]||timeBlank[2]>timePeriod[2]){stop("No noise possible with timeblank not in the chosen interval")}
      }
     resultMeanT2=NULL
      for(ion in unique(res[,"ion"]))
      {
       #average of the noise to be removed
        maxNoise[ion]=max(resultMeanT[resultMeanT[,"time"]<=timeBlank[2]&resultMeanT[,"time"]>=timeBlank[1]&resultMeanT[,"ion"]==ion,"intensity"],na.rm=T)
        avgNoise[ion]=mean(resultMeanT[resultMeanT[,"time"]<=timeBlank[2]&resultMeanT[,"time"]>=timeBlank[1]&resultMeanT[,"ion"]==ion,"intensity"],na.rm=T)
        sdNoise[ion]=sd(resultMeanT[resultMeanT[,"time"]<=timeBlank[2]&resultMeanT[,"time"]>=timeBlank[1]&resultMeanT[,"ion"]==ion,"intensity"],na.rm=T)


        resultMeanT[resultMeanT[,"ion"]==ion,"intensity"]=resultMeanT[resultMeanT[,"ion"]==ion,"intensity"]-avgNoise[ion]

        # if(!is.null(smoothMethod))
        # {
        #   resultMeanT[!is.na(resultMeanT[,"intensity"])&resultMeanT[,"intensity"]<0,"intensity"]=0
        #   spMT=new("Spectrum1",mz=resultMeanT[!is.na(resultMeanT[,"intensity"]),"time"],intensity=resultMeanT[!is.na(resultMeanT[,"intensity"]),"intensity"],centroided=F)
        #   spMT2=MSnbase::smooth(spMT,method=smoothMethod,halfWindowSize = halfWindowSize)
        #   resultMeanT2=rbind(resultMeanT2,data.frame(ion=ion,time=MSnbase::mz(spMT2),intensity=MSnbase::intensity(spMT2)))
        # }

     }
      # if(!is.null(smoothMethod))
      # {
      #   p_sc=ggplot(resultMeanT2[resultMeanT2[,"ion"]!=referenceBreath,],aes(x=time,y=intensity,group=ion,color=ion,name=ion))+geom_line()+theme_bw()+theme(legend.position="none")
      #   p_breath=ggplot(resultMeanT2[resultMeanT2[,"ion"]==referenceBreath,],aes(x=time,y=intensity,group=ion,color=ion,name=ion))+geom_line()+theme_bw()+theme(legend.position="none")
      #
      #
      # }
      # else
      # {
        p_sc=ggplot(resultMeanT[resultMeanT[,"ion"]!=referenceBreath,],aes(x=time,y=intensity,group=ion,color=ion,name=ion))+geom_line()+theme_bw()+theme(legend.position="none")
        p_breath=ggplot(resultMeanT[resultMeanT[,"ion"]==referenceBreath,],aes(x=time,y=intensity,group=ion,color=ion,name=ion))+geom_line()+theme_bw()+theme(legend.position="none")
     # }

    }
    if(!removeNoise)
    {
      p_sc=ggplot(resultMeanT[resultMeanT[,"ion"]!=referenceBreath,],aes(x=time,y=intensity,group=ion,color=ion,name=ion))+geom_line()+theme_bw()+theme(legend.position="none")
      p_breath=ggplot(resultMeanT[resultMeanT[,"ion"]==referenceBreath,],aes(x=time,y=intensity,group=ion,color=ion,name=ion))+geom_line()+theme_bw()+theme(legend.position="none")
    }

    if(breathRatio)
    {
      dcasted=dcast(resultMeanT,formula=time+duration~ion,value.var="intensity",fun.aggregate=mean)
      dcasted2=dcasted
      dcasted2[,ions]=sweep(dcasted[,ions],1,dcasted[,referenceBreath],FUN="/")
      dfres=reshape(dcasted2,direction="long",varying=list(colnames(dcasted2)[-c(1:2)]),times=colnames(dcasted2)[-c(1:2)],timevar="ion")
      colnames(dfres)=c("time","duration","ion","intensity","id")
      resultMeanT=dfres[,c("time","ion","intensity","duration")]
      p_sc=ggplot(resultMeanT[resultMeanT[,"ion"]!=referenceBreath,],aes(x=time,y=intensity,group=ion,color=ion,name=ion))+geom_line()+theme_bw()+theme(legend.position="none")
      p_breath=ggplot(resultMeanT[resultMeanT[,"ion"]==referenceBreath,],aes(x=time,y=intensity,group=ion,color=ion,name=ion))+geom_line()+theme_bw()+theme(legend.position="none")
    }

    res2=resultMeanT
    param=list(correction=correction, maxNoise=maxNoise,sdNoise=sdNoise,avgNoise=avgNoise)
    res=list(res=res2,gg=list(p_smoothbreath=gg_cycles$p2,p_cyclelimits=gg_cycles$p3,p_smoothcycle=p_sc,p_breath=p_breath), detectCycle=cycles,param=param)
    return(res)
    }
    else
    {
      warning("No cycle was detected. The 'none' correction was conducted. If you want to modify the parameters to detect cycles, you can see the graph corresponding to the previous parameters in the results: res$detectCycle$gg$p3 for the graph and the restriction on the peak Table (res$detectCycle$peakTable) in order to understand why no peaks have been detected")
      res=ptrvIntensityByTime(dataset,timeCol=timeCol,colToRemove=colToRemove,referenceBreath=NULL,correction="none",timePeriod=timePeriod,timeStart=timeStart,removeNoise=removeNoise,timeBlank=timeBlank,
                                   halfWindowSize=halfWindowSize, total=total,breathRatio=breathRatio,method=method,SNR=SNR,ions=ions,
                                   funAggregate=funAggregate,smoothMethod=smoothMethod,minimalDuration=minimalDuration,
                                   minExpi=minExpi,maxInspi=maxInspi,forMinExpiDivideMaxIntBy=forMinExpiDivideMaxIntBy,forMaxInspiDivideMaxIntBy=forMaxInspiDivideMaxIntBy)

      return(res)
    }
  }

}