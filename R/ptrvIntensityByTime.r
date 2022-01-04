#' @title ptrvIntensityByTime
#' @description Calculates the intensity by time for PTRViewer files. The calculation can be done by breathing cycle or on raw data.Breathing cycles are calculated on a reference ion (referenceBreath) with peak picking algorithms
#' @param dataset whose names are IN THIS ORDER:AbsTime, RelTime, Cycle, Ion1,... ,Ionp.
#' @param referenceBreath name of the ion taken as reference for breathing
#' @param correction "none" or "cycle". See Details.
#' @param timePeriod vector containing starting and stopping point to get the statistics
#' @param timeStart timeStart is a value to be removed of the relTime column
#' @param removeNoise if TRUE, the averaged intensity obtained during the timeBlank period is removed from the signal
#' @param halfWindowSize parameter used to determine the smooth for the peak-picking used in break detection
#' @param maxPeaks A number. Below this value, a peak is not considered as a peak
#' @param total if TRUE, the total intensity is calculated and added as a supplementary column
#' @param breathRatio if TRUE, the ratio between the intensity of ion and the breath is calculated
#' @param method method used for peak picking ("MAD" by default)
#' @param SNR Signal noise ratio parameter used in peak picking
#' @param ions vector contatining all ions to be used in the analysis (by default all ions are used)
#' @param timeBlank vector of two numbers. Indicates the interval to take as a blank period.
#' @return a list containing the obtained result and ggplot object
#' @export
#' @importFrom stats reshape
#' @importFrom MSnbase smooth
#' @importFrom reshape2 dcast
ptrvIntensityByTime=function(dataset,referenceBreath=NULL,correction="none",timePeriod=NULL,timeStart=0,removeNoise=TRUE,timeBlank=c(0,30),halfWindowSize=10, maxPeaks=NULL,total=FALSE,breathRatio=FALSE,method="MAD",SNR=0,ions=NULL,smoothCycle=NULL,halfWindowSizeCycle=5,funAggregate="mean",smoothMethod="MovingAverage",minimalDuration=2)
{
  time=NULL
  p_sc=p_breath=NULL
   match.arg(correction,c("none","cycle"))
  if(is.null(referenceBreath)&correction=="cycle"){correction="none";print("No referenceBreath, the 'none' correction is chosen.")}
  if(is.null(ions)){ions=colnames(dataset)[-c(1:3)]}else{ions=unique(c(referenceBreath,ions))}
  if(!is.null(ions))
  {
    if(correction=="cycle")
    {
      if(!referenceBreath%in%colnames(dataset)){stop("No breathing ion in the dataset")}
    }
    if(sum(ions%in%colnames(dataset))!=length(ions)){print(ions%in%colnames(dataset));stop("some ions are not in the dataset")}
    dataset=dataset[,c("AbsTime","RelTime","Cycle",ions)]
  }
  if(!is.null(timePeriod))
  {
      dataset=dataset[dataset[,"RelTime"]<timePeriod[2]&dataset[,"RelTime"]>timePeriod[1],]
  }
  dataset[,"RelTime"]=dataset[,"RelTime"]-timeStart
  indexIons=which(colnames(dataset)%in%ions)
  dataset[,ions]=apply(dataset[,ions],2,as.numeric)

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

    dataset[,"duration"]=c(dataset[-1,"RelTime"]-dataset[-c(length(dataset[,"RelTime"])),"RelTime"],0)
    res=reshape(dataset,direction="long",varying=list(indexIons2),times=colnames(dataset)[indexIons2],v.names="intensity")
    colnames(res)=c("abs","time","cycle","duration","ion","intensity","id")
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

    res=reshape(dataset,direction="long",varying=list(ions),times=ions,v.names="intensity")
    colnames(res)=c("abs","time","cycle","ion","intensity","id")
    if(!referenceBreath%in%unique(res[,"ion"])){stop("the reference breath is not one column of the dataset")}

    res1=res[res[,"ion"]==as.character(referenceBreath),]
    cycles=detectCycle(res1=res1,maxPeaks=maxPeaks,smoothMethod=smoothMethod,method=method,halfWindowSize=halfWindowSize,maximum=max(dataset[,"RelTime"]),SNR=SNR,minimalDuration=minimalDuration)
    gg_cycles=cycles$gg
    timeToUseForBreathing=cycles$cycles
    timeLab=1/2*(timeToUseForBreathing[-1]+timeToUseForBreathing[-length(timeToUseForBreathing)])
    duration=diff(timeToUseForBreathing)
    df_duration=data.frame(time=timeLab,duration=duration)

    # Cycle decomposition
    res[,"cycle"]=cut(res[,"time"],breaks=timeToUseForBreathing,labels=timeLab)

    # Statistics by cycle
    if(funAggregate=="sum")
    {
      resultMeanT=dcast(res,cycle+ion~.,value.var="intensity",fun.aggregate=function(x){return(sum(x,na.rm=T))})
    }
    if(funAggregate=="mean")
    {
      resultMeanT=dcast(res,cycle+ion~.,value.var="intensity",fun.aggregate=function(x){return(mean(x,na.rm=T))})
    }
    if(funAggregate=="max")
    {
      resultMeanT=dcast(res,cycle+ion~.,value.var="intensity",fun.aggregate=function(x){return(max(x,na.rm=T))})
    }
    colnames(resultMeanT)=c("time","ion","intensity")

    resultMeanT=merge(resultMeanT[!is.na(resultMeanT[,"time"]),],df_duration,by="time",all.x=T)
    resultMeanT[,"time"]=as.numeric(as.character(resultMeanT[,"time"]))

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

        avgNoise=mean(resultMeanT[resultMeanT[,"time"]<=timeBlank[2]&resultMeanT[,"time"]>=timeBlank[1]&resultMeanT[,"ion"]==ion,"intensity"],na.rm=T)
        resultMeanT[resultMeanT[,"ion"]==ion,"intensity"]=resultMeanT[resultMeanT[,"ion"]==ion,"intensity"]-avgNoise
        if(!is.null(smoothCycle))
        {
          resultMeanT[!is.na(resultMeanT[,"intensity"])&resultMeanT[,"intensity"]<0,"intensity"]=0
          spMT=new("Spectrum1",mz=resultMeanT[!is.na(resultMeanT[,"intensity"]),"time"],intensity=resultMeanT[!is.na(resultMeanT[,"intensity"]),"intensity"],centroided=F)
          spMT2=MSnbase::smooth(spMT,method=smoothCycle,halfWindowSize = halfWindowSizeCycle)
          resultMeanT2=rbind(resultMeanT2,data.frame(ion=ion,time=MSnbase::mz(spMT2),intensity=MSnbase::intensity(spMT2)))
        }

     }
      if(!is.null(smoothCycle))
      {
        p_sc=ggplot(resultMeanT2[resultMeanT2[,"ion"]!=referenceBreath,],aes(x=time,y=intensity,group=ion,color=ion,name=ion))+geom_line()+theme_bw()+theme(legend.position="none")
        p_breath=ggplot(resultMeanT2[resultMeanT2[,"ion"]==referenceBreath,],aes(x=time,y=intensity,group=ion,color=ion,name=ion))+geom_line()+theme_bw()+theme(legend.position="none")


      }
      else
      {

        p_sc=ggplot(resultMeanT[resultMeanT[,"ion"]!=referenceBreath,],aes(x=time,y=intensity,group=ion,color=ion,name=ion))+geom_line()+theme_bw()+theme(legend.position="none")
        p_breath=ggplot(resultMeanT[resultMeanT[,"ion"]==referenceBreath,],aes(x=time,y=intensity,group=ion,color=ion,name=ion))+geom_line()+theme_bw()+theme(legend.position="none")

        }

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
    return(list(res=res2,gg=list(p_smoothbreath=gg_cycles$p2,p_cyclelimits=gg_cycles$p3,p_smoothcycle=p_sc,p_breath=p_breath),correction=correction))
  }

}