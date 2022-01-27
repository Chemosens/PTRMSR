#' Detects several starts in one file
#' Detects several starts in one file (according to a vector of theoretical stimulation times)
#' @param methods Choose among c("higherThanNoise","startPeakProportion","higherDerivative")
#' @param nPoints number of points required in the smoothing function
#' @param multiplyNoiseBy required if method='higherThanNoise' is chosen
#' @param statOfNoise required if method='higherThanNoise'. Statistic to be multiplied by multiplyByNoise ('max' or 'avg'). Can also be 'bl' if corresponding to the estimation of noise during the whole evaluation or 'blper' if
#' @param startPeaks a vector containing the stimulation times (theoretical starts) for all files
#' @param ptrFile_i data frame exported from ptrViewer
#' @param manual manual results obtained (as a vector of the same length as startPeaks)
#' @param endPeaks if NULL, the end of the peak period is given by the next start in startPeaks
#' @param ion name of the column corresponding to the ion to analyse
#' @return a list containing
#' \itemize{
#' \item dfres (dataframe with the obtained times)
#' \item gg (a graphic representing the obtained intensities and the obtained starting times, diagnosis)
#' }
#' @inheritParams ptrvDetectStart
#' @export
detectStartsInFile=function(ptrFile_i,ion,startPeaks,endPeaks=NULL,methods=c("higherThanNoise","startPeakProportion","higherDerivative"),
                            nPoints=31,multiplyNoiseBy=1.5,statOfNoise="bl",smooth=TRUE,
                            proportionOfMax=0.1,peakChoice="maxIntensity",order=2,detectionThreshold=15,
                            firstAmongHighThreshold=50,manual=NULL)
{
  X1=intensity=time=NULL
  nbStimulations=length(startPeaks)
  finalMatrix=rep(NA,nbStimulations);names(finalMatrix)=paste0("pic",1:nbStimulations)
  finalMatrix2=finalMatrix3=diagnosis=diagnosis2=diagnosis3=finalMatrix
  ions=colnames(ptrFile_i)[-1]
  longDf=reshape(ptrFile_i,direction="long",varying=list(ions),times=ions)
  colnames(longDf)=c("time","ion","intensity","id")
  starts=c(ion)
  p=ggplot(longDf[longDf[,"ion"]%in%starts,],aes(x=time,y=intensity))+geom_line(color="grey")+theme_bw()

  startPeaks=as.numeric(as.character(startPeaks))
  stimulation=data.frame(X1=startPeaks)

  p=p+geom_vline(data=stimulation,aes(xintercept=X1))
  # ggplotly(p)
  localMax=rep(NA,nbStimulations)
  minTime=as.numeric(as.character(startPeaks[1]))
  ImaxAllPeriod=max(longDf[longDf[,"time"]>minTime,"intensity"])
#  relativeIntensityThreshold=rit*ImaxAllPeriod

  for(i in 1:nbStimulations)
  {
    startPeak=as.numeric(as.character(startPeaks[i]))
    if(is.null(endPeaks))
    {
      if(i!=nbStimulations){endPeak=as.numeric(as.character(startPeaks[i+1]))}else{endPeak=max(ptrFile_i[,"RelTime"],na.rm=T)}
    }
     localMax[i]=max(longDf[longDf[,"time"]<endPeak&longDf[,"time"]>startPeak&longDf[,"ion"]==ion,"intensity"])

    if("startPeakProportion" %in% methods)
    {
      resds=ptrvDetectStart(res=longDf,starts=ion,startPeriod=c(startPeak, endPeak),method="startPeakProportion",proportionOfMax=proportionOfMax,peakChoice=peakChoice,nPoints=nPoints,smooth=smooth,firstAmongHighThreshold=firstAmongHighThreshold,detectionThreshold=detectionThreshold)
      finalMatrix[i]=resds$tx
      diagnosis[i]=resds$diagnosis
    }
    if("higherThanNoise"%in% methods)
    {
      resds2=ptrvDetectStart(res=longDf,method="higherThanNoise",starts=ion,noisePeriod = c(startPeak,endPeak),startPeriod=c(startPeak, endPeak),multiplyNoiseBy=multiplyNoiseBy,statOfNoise=statOfNoise,smooth=smooth,nPoints=nPoints,peakChoice=peakChoice,firstAmongHighThreshold=firstAmongHighThreshold,detectionThreshold=detectionThreshold)
      finalMatrix2[i]=resds2$tx
      diagnosis2[i]=resds2$diagnosis
    }
    if("higherDerivative"%in% methods)
    {
      resds3=ptrvDetectStart(res=longDf,method="higherDerivative",starts=ion,nPoints=nPoints,smooth=smooth,startPeriod=c(startPeak, endPeak),order=order,detectionThreshold=detectionThreshold)
      finalMatrix3[i]=resds3$tx
      diagnosis3[i]=resds3$diagnosis
    }
  }
  p2=p
  # Visual checking
  if(!is.null(manual))
  {

    colnames(manual)="X1"
    p2=p2+geom_vline(data=manual,color="blue",aes(xintercept=X1),linetype = 3)
    dfres=data.frame(paste0("pic",1:nbStimulations),manual[-1,])
  }
  else
  {
    dfres=data.frame(paste0("pic",1:nbStimulations))
  }
#  manual=data.frame(X1=as.numeric(stat3[stat3[,"Fichier"]==substr(file,1,23),"Latence"])/1000)
#

  p3=p2+ggtitle("Peak detection")+labs(subtitle=element_text("startPeakProportion: red ; higherThanNoise: green ;manual: blue"))
  if("startPeakProportion" %in% methods)
  {
    response=data.frame(X1=(finalMatrix))
    colnames(response)="X1"
    p3=p3+geom_vline(data=response,color="red",aes(xintercept=X1),linetype = 2)
    if(!is.null(manual))
    {
          dfres=cbind(dfres,resp=response,diff_startPeakProportion=response-manual[-1,])
          colnames(dfres)[(ncol(dfres)-1):ncol(dfres)]=c("startPeakProportion","diff_startPeakProportion")
          dfres[is.infinite(dfres[,"diff_startPeakProportion"]),"diff_startPeakProportion"]=NA

    }
    else
    {

      dfres=cbind(dfres,resp=response)
      colnames(dfres)[ncol(dfres)]="startPeakProportion"
    }
  }
  if("higherThanNoise"%in% methods)
  {
    response2=data.frame(X1=(finalMatrix2))
    colnames(response2)="X1"
    p3=p3+geom_vline(data=response2,color="green",aes(xintercept=X1),linetype = 7,size=1)

    if(!is.null(manual))
    {
      dfres=cbind(dfres,response2,diff_higherThanNoise=response2-manual[-1,])
      colnames(dfres)[(ncol(dfres)-1):ncol(dfres)]=c("higherThanNoise","diff_higherThanNoise")
      dfres[is.infinite(dfres[,"diff_higherThanNoise"]),"diff_higherThanNoise"]=NA
    }
    else
    {

      dfres=cbind(dfres,resp=response2)
      colnames(dfres)[ncol(dfres)]="higherThanNoise"
    }
  }
  if("higherDerivative"%in% methods)
  {
    response3=data.frame(X1=(finalMatrix3))
    colnames(response3)="X1"
    p3=p3+geom_vline(data=response3,color="green",aes(xintercept=X1),linetype = 2)
    p3
    if(!is.null(manual))
    {
      dfres=cbind(dfres,response3-manual[-1,])
      colnames(dfres)[ncol(dfres)]="diff_higherDerivative"
      dfres[is.infinite(dfres[,"diff_higherDerivative"]),"diff_higherDerivative"]=NA
    }
    else
    {

      dfres=cbind(dfres,resp=response2)
      colnames(dfres)[ncol(dfres)]="higherDerivative"
    }
  }

  if(!is.null(manual))
  {
    colnames(dfres)[1:2]=c("pic","manual")
 #   ggplotly(p3)
  }
  else
  {
    colnames(dfres)[1]="pic"
  }
  diag=NULL
  if("startPeakProportion" %in% methods){diag=rbind(diag,diagnosis)}
  if("higherThanNoise" %in% methods){diag=rbind(diag,diagnosis2)}
  if("higherDerivative" %in% methods){diag=rbind(diag,diagnosis3)}
  dfres=cbind(dfres,t(diag),localMax)
  return(list(dfres=dfres,gg=p3,diagnosis=diag))
}
