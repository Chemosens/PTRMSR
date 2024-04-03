#'Returns a report for a given dataset
#'@inheritParams ptrvIntensityByTime
#'@inheritParams ptrvSignificantSNRIons
#'@inheritParams ptrvDetectStart
#'@param methodDetectStart method to detect start (see \link{ptrvDetectStart}). If numeric, the
#'@param selecIons if "evolving", the selected ions are the ones found as significant in ptrvSignificantSNRIons with noisePeriodSig and max method.If "namely", only the ions present in listIons are selected
#'@param listIons list of ions to be presented in the report
#'@param noisePeriodIBT noise period used during intensity by time calculation
#'@param noisePeriodSig noise period used during significant ions calculation
#'@param noisePeriodDS noise period used during the start detection
#'@param detectingStart boolean specifying wether the detect start procedure should be done (TRUE) or not (FALSE)
#'@importFrom gridExtra grid.arrange
#'@return a list containing: the final data start0 (without breathing correction), start1 (with breathing correction) and a list of plots)
#'@export
ptrvReport=function(dataset,selecIons="evolving",listIons=NULL,referenceBreath,smoothMethod="MovingAverage",methodDetectStart="startPeakProportion",noisePeriodIBT=c(0,30),noisePeriodSig=c(0,30),noisePeriodDS=c(0,30),proportionOfMax=0.3,halfWindowSize=12,startPeriod=c(20,60),detectingStart=FALSE,minimalDuration=2,minExpi=NULL,maxInspi=NULL,forMinExpiDivideMaxIntBy=4,forMaxInspiDivideMaxIntBy=5)
{
  intensity=NULL
  if(!is.null(listIons)){selecIons="namely"}
  ion=snratio=time=NULL
  #dataset=read.table(listFiles[1],sep="\t",dec=",",header=T)
  res_raw=ptrvIntensityByTime(dataset=dataset,referenceBreath=referenceBreath,correction="none",timePeriod=NULL,timeStart=0,halfWindowSize=halfWindowSize,smoothMethod=smoothMethod, method="MAD",total=FALSE,breathRatio=FALSE,minimalDuration=minimalDuration,minExpi=minExpi,maxInspi=maxInspi,forMinExpiDivideMaxIntBy=forMinExpiDivideMaxIntBy,forMaxInspiDivideMaxIntBy=forMaxInspiDivideMaxIntBy)
  res_cyc=ptrvIntensityByTime(dataset=dataset,referenceBreath=referenceBreath,correction="cycle",timePeriod=NULL,timeStart=0,halfWindowSize=halfWindowSize,smoothMethod=smoothMethod, method="MAD",total=FALSE,breathRatio=FALSE,minimalDuration=minimalDuration,minExpi=minExpi,maxInspi=maxInspi,forMinExpiDivideMaxIntBy=forMinExpiDivideMaxIntBy,forMaxInspiDivideMaxIntBy=forMaxInspiDivideMaxIntBy)
  res_bratio=ptrvIntensityByTime(dataset=dataset,referenceBreath=referenceBreath,correction="cycle",timePeriod=NULL,timeStart=0,halfWindowSize=halfWindowSize,smoothMethod=smoothMethod, method="MAD",total=FALSE,breathRatio=TRUE,minimalDuration=minimalDuration,minExpi=minExpi,maxInspi=maxInspi,forMinExpiDivideMaxIntBy=forMinExpiDivideMaxIntBy,forMaxInspiDivideMaxIntBy=forMaxInspiDivideMaxIntBy)
  resBreathRaw=res_raw$res[res_raw$res[,"ion"]==referenceBreath,]
  resBreathCyc=res_cyc$res[res_cyc$res[,"ion"]==referenceBreath,]
  resBreathBratio=res_bratio$res[res_bratio$res[,"ion"]==referenceBreath,]

  # Select significant ions
  if(selecIons=="evolving")
  {
    sigionres=ptrvSignificantSNRIons(dataset,referenceBreath=referenceBreath,noisePeriod=noisePeriodSig,correction="cycle",multiplyNoiseBy = 3,method="max")
    dfrat=data.frame(ion=names(sigionres$snRatio),snratio=sigionres$snRatio)
    dfrat2=dfrat[dfrat[,"snratio"]>3,]
    dfrat3=dfrat2[order(dfrat2[,"snratio"],decreasing=T),]
    dfrat3[,"ion"]=factor(dfrat3[,"ion"],levels=rev(dfrat3[,"ion"]))
    sigion1=sigionres$listIons
  }
  if(selecIons=="namely")
  {
    sigion1=listIons
  }
  if(sum(res_raw$res[,"ion"]%in%sigion1)==0){stop("listIons is not in dataset")}
  resTimeRaw=res_raw$res[res_raw$res[,"ion"]%in%sigion1,]
  resTimeCyc=res_cyc$res[res_cyc$res[,"ion"]%in%sigion1,]
  resTimeBratio=res_bratio$res[res_bratio$res[,"ion"]%in%sigion1,]
  #Wide formats of datasets for standardization
  tmp=dcast(resTimeCyc,time+duration~ion,value.var="intensity",fun.aggregate=mean)
  tmp[tmp<0]=0
  resSum=apply(tmp[,-c(1,2)],1,sum)
  #resTimeCyc[,"time2"]=resTimeCyc[,"time"]-resTimeCyc[,"duration"]/2
  tmp_bis=sweep(tmp[,-c(1,2)],1,resSum,"/")
  tmp_bis[,c("time","duration")]=tmp[,c("time","duration")]
  ind_time=which(colnames(tmp_bis)%in%c("time","duration"))
  res_sum=reshape(tmp_bis,direction="long",varying=list(colnames(tmp_bis)[-ind_time]),times=colnames(tmp_bis)[-ind_time],timevar="ion")
  colnames(res_sum)=c("time","duration","ion","intensity","id")
  #res_sum[,"time2"]=res_sum[,"time"]-res_sum[,"duration"]/2
  if(selecIons=="evolving")
  {
    starts=dfrat3[,"ion"]
  }
  else
  {
    starts=sigion1
  }
  if(detectingStart==TRUE)
  {
    resds2=ptrvDetectStart(res=res_cyc,starts=starts, method=methodDetectStart,noisePeriod=noisePeriodDS,proportionOfMax=proportionOfMax,startPeriod=startPeriod)
    start0=ptrvComparableData(res_sum,into=resds2$tx,timeColumn = "time")
    start1=ptrvComparableData(resTimeCyc,into=resds2$tx,timeColumn = "time")
  }
  if(is.numeric(detectingStart))
  {
    resds2=list()
    resds2$tx=detectingStart
    start0=ptrvComparableData(res_sum,into=resds2$tx,timeColumn = "time")
    start1=ptrvComparableData(resTimeCyc,into=resds2$tx,timeColumn = "time")

  }
  if(!is.numeric(detectingStart)&detectingStart!=TRUE)
  {
    resds2=list()
    resds2$tx=0
    start0=ptrvComparableData(res_sum,into=resds2$tx,timeColumn = "time")
    start1=ptrvComparableData(resTimeCyc,into=resds2$tx,timeColumn = "time")

  }


  # All graphs
  p_breath=res_cyc$gg
  if(selecIons=="evolving")
  {
    p_kineticIons=ggplot(dfrat3,aes(y=ion,x=snratio))+geom_col()+theme_bw()+ggtitle("Signal/Noise ratio")
  }
  else
  {
    p_kineticIons=NULL
  }
  if(detectingStart==TRUE)
  {
    p_detectStart=resds2$gg
  }
  else
  {
    p_detectStart=NULL
  }

  # Vizualization with curves
  #===============
  p1=ggplot(resTimeRaw,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme_bw()+ggtitle("Raw intensity")+geom_vline(xintercept=resds2$tx)
  p2=ggplot(resTimeCyc,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme_bw()+ggtitle("Corrected intensity (by cycle)")+geom_vline(xintercept=resds2$tx)
  p3=ggplot(resTimeBratio,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme_bw()+ggtitle("Breathing ratio of intensity)")+geom_vline(xintercept=resds2$tx)
  p4=ggplot(resBreathRaw,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme_bw()+theme(legend.position="none")+ggtitle("Raw intensity for breathing ion")+geom_vline(xintercept=resds2$tx)
  p5=ggplot(resBreathCyc,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme_bw()+theme(legend.position="none")+ggtitle("Corrected intensity for breathing ion")+geom_vline(xintercept=resds2$tx)
  # Pro# p9=ggplot(tmp,aes(x=time,y=sum))+geom_line()+theme_bw()+ggtitle("Ion sum")+geom_vline(xintercept=resds2$tx)
  p_curves=list(p_raw=p1,p_breath_raw=p4,p_cycle=p2,p_breath_cycle=p5,p_ratio=p3)

  # vizualization with barplots
  #====================
  p10=ggplot(res_sum[res_sum[,"ion"]!="sum",],aes(x=time,y=intensity,group=ion,color=ion,fill=ion))+geom_col(width=res_sum[res_sum[,"ion"]!="sum","duration"])+theme_bw()+theme(legend.position="none")+ggtitle("Divided by sum")+geom_vline(xintercept=resds2$tx)
  p11=ggplot(resTimeCyc[resTimeCyc[,"ion"]%in%listIons,],aes(x=time,y=intensity,group=ion,color=ion,fill=ion))+geom_col(width=resTimeCyc[,"duration"])+theme_bw()+theme(legend.position="none")+ggtitle("Breathing correction")+geom_vline(xintercept=resds2$tx)
  p_barplot=list(p_sum=p10,p_cycle=p11)

  # After removing tx
  #===================

    p13=ggplot(start0,aes(x=time,y=intensity,group=ion,color=ion,fill=ion))+geom_col(width=start0[,"duration"])+theme_bw()+theme(legend.position="none")+ggtitle("Bandplot")
    p12=ggplot(start1,aes(x=time,y=intensity,group=ion,color=ion,fill=ion))+geom_col(width=start1[,"duration"])+theme_bw()+theme(legend.position="none")+ggtitle("Bandplot")
    p_barplot_tx=list(p_before=p12,p_after=p13)



  gg=list(p_breath=p_breath,p_kineticIons=p_kineticIons,p_detectStart=p_detectStart,p_curves=p_curves,p_barplot=p_barplot,p_removeT=p_barplot_tx)
  return(list(start0,start1,gg=gg))
}