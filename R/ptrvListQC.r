#' @importFrom stats sd
ptrvListQC=function(listFiles,starts="")
{
  for(i in 1:length(listFiles))
  {
     dataset=read.table(listFiles[i],sep="\t",header=T)
     ibt=ptrvIntensityByTime(dataset,referenceBreath="",correction="none",timePeriod=NULL,timeStart=0,removeNoise=FALSE,timeBlank=30,halfWindowSize=10, maxPeaks=50)
     startTime=ptrvDetectStart(ibt,starts=starts,proportionOfMax=0.01)
     print(startTime)
     intensity=ptrvIntensity(ibt$res)[,c("ion","area")]
   if(i==1){df_res=intensity};if(i>1){df_res=merge(df_res,intensity,by="ion")}
    # if(format=="long"){df_res=rbind(df_res,intensity)}
  }
  colnames(df_res)=c("ion",listFiles);df_res2=df_res[,-1];rownames(df_res2)=df_res[,1];colnames(df_res2)=colnames(df_res[,-1]);df_res=df_res2
  res=as.data.frame(t(df_res))
  res2=res
  res2["sd",]=apply(res,2,sd)
  res2["mean",]=apply(res,2,mean)
  res2["CV",]=round(100*res2["sd",]/res2["mean",],digits=2)
  return(res2)
}




