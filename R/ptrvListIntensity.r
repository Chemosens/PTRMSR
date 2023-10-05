#' @title ptrvListIntensity
#' @param listFiles vector containing the file names of the files to be read (they are read by read.table function, header=TRUE). Please add an header to your file containing the time ("RelTime" by default -can be modified in colTime variable) a column by ion
#' @param metaData metaData required containing a line per file in listFiles, then a column specifying the ion to follow for breathing (in a column named as the parameter nameBreathMeta -default to resp- ), the total duration of the evaluation (in a column named as nameEndMeta -default to finTimeSens_s-), the moment of tasting (to define the noise, in a column named as nameInMouthMeta, default to 'miseEnBouche_s')
#' @param format "wide" by default
#' @param correction "cycle" by default.
#' @param stat "area" by default but can also be tmax or max
#' @param sep separator(has to be the same for all files in listFiles). Default to tabulation.
#' @param nameBreathMeta default to "resp". Name of the metaData column containing the ion corresponding to breathing for each file
#' @param nameEndMeta default to "finTimeSens_s". Name of the metaData column containing the end of the tasting
#' @param nameInMouthMeta default to "miseEnBouche_s". Name of the column of metaData file containing the time of the beginning of the tasting (after the blank)
#' @param nameFileMeta default to "file". Name of the column of metaData file containing the names of the files (with their extension ! for example file1.txt)
#' @param wd working directory where the files of listFiles are.
#' @inheritParams ptrvIntensityByTime
#' @importFrom utils read.table
#' @export
ptrvListIntensity=function(listFiles,metaData=NULL,sep="\t",timeCol="RelTime",colToRemove=c("AbsTime","Cycle"),format="wide",correction="cycle",stat="area", halfWindowSize=5, total=FALSE,breathRatio=FALSE,method="MAD",SNR=0,ions=NULL,
                           funAggregate="mean",smoothMethod="MovingAverage",minimalDuration=2,minExpi=NULL,maxInspi=NULL,forMinExpiDivideMaxIntBy=5,forMaxInspiDivideMaxIntBy=4,nameBreathMeta="resp",nameEndMeta="finTimeSens_s",nameInMouthMeta="miseEnBouche_s",nameFileMeta="file",removeNoise=TRUE,wd=getwd())
{
  res=data.frame()
  for(i in 1:length(listFiles))
  {
    file=listFiles[i]
    print(file)
    dataset=read.table(paste0(wd,"/",file),sep=sep,header=T)
    metaInfo=metaData[metaData[,nameFileMeta]==file,]
    print(metaInfo)
    result_all=ptrvIntensityByTime(dataset=dataset,timeCol=timeCol,colToRemove=colToRemove,referenceBreath=metaInfo[nameBreathMeta],correction=correction,timePeriod=c(0,metaInfo[nameEndMeta]),
                                   removeNoise=removeNoise,timeBlank = as.numeric(metaInfo[nameInMouthMeta]),
                                   halfWindowSize=halfWindowSize, total=total,breathRatio=breathRatio,method=method,SNR=SNR,ions=ions,
                                   funAggregate= funAggregate,smoothMethod=smoothMethod,minimalDuration=minimalDuration,
                                   minExpi= minExpi,maxInspi=maxInspi,forMinExpiDivideMaxIntBy=forMinExpiDivideMaxIntBy,forMaxInspiDivideMaxIntBy=forMaxInspiDivideMaxIntBy)
    #result_all_none=ptrvIntensityByTime(dataset=dataset,referenceBreath=breath,correction="none",timePeriod=c(0,120),removeNoise=FALSE,timeBlank = 36)
    if(correction=="cycle")
    {
      if(format=="wide")
      {
        res_cycle=ptrvIntensity(result_all)[,c("ion","area")]
        if(i==1) { res=res_cycle;colnames(res)[2]=listFiles[1]}
        if(i>1){ res=merge(res,res_cycle,by="ion");colnames(res)[i+1]=listFiles[i]}
      }
      if(format=="long")
      {
        res_cycle=ptrvIntensity(result_all)[,c("ion","area")]
        res_cycle[,"file"]=listFiles[i]
        res=rbind(res,res_cycle)
      }
     }
    if(correction=="none")
    {

      res_i=result_all
      res_i[,"file"]=listFiles[i]
      res=rbind(res,res_i)
    }
  }
  return(res)
}