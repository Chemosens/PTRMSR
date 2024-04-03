#' @title ptrvListIntensityByTime
#' A function automating all pretreatements for several files,removing the noise and the breathing biases
#' @param listFiles list of the file names to be imported. These files should contain a culmn of time (timeCol) then columns containing ions.
#' @param metaData A data.frame with colnames as "file","product","subject","rep","breathing","into","swallow","start","stop").See details
#' @param dec_vec Vector with the same length as listFiles containing the decimal separator used in the list of files
#' @param sep separator used in the files to be read
#' @param removeBlankTime boolean. If TRUE, the time of blank time is removed from the time column
#' @param stat Statistic to be extracted for each ion in each file. Among c("sum","moy","sd","n","min","max","tmax","area")
#' @param ions vector of ion to be analyzed
#' @param wd working directory where the files of listFiles are.
#' @details The "file" column of metaData correspond to the identifier of an evaluation.
#' The 'start' column contains the time corresponding to the beginning of the measurement (when the subject begins to breath in PTR device)
#' The "into" column correspond to the time when the sample is put into the mouth of the subject. The blank period is calculated as between the 'start' and the 'into" period.
#' The 'nothing' column is not useful for this version of package but should contain the time when the subject has nothing more in the mouth
#' The 'stop' column contains the time of the end of the evaluation (not the time of swallowing)
#' The 'swallow"  column should contain the time of swallowing but is not used in this version of the package.
#' The 'breathing"  column should contains the name of the ion used to evaluate breathing  (the name of the column of your files that contains isoprene or acetone, for example)
#'
#' Remark: maxInspi and minExpi can also take the value 'median' that indicates that the median of the breathing ion intensity is chosen as limit to detect inspiration or expiration as peak
#' @importFrom utils read.table
#' @return a list containing :  res (data containing the corrected timepoints), metaData, cycleLimits and call.
#' @inheritParams ptrvIntensityByTime
#' @export
#'
ptrvListIntensityByTime=function(listFiles,timeCol="RelTime",colToRemove=c("AbsTime","Cycle"),removeBlankTime=FALSE,ions=NULL,dec_vec=rep(".",length(listFiles)),sep="\t",
                                 metaData=NULL,correction="cycle",halfWindowSize=5,smoothMethodBreath="MovingAverage",method="MAD",total=FALSE,breathRatio=FALSE,stat="area",minimalDuration=2,minExpi=NULL,maxInspi=NULL,forMinExpiDivideMaxIntBy=5,forMaxInspiDivideMaxIntBy=4,wd=getwd(),
                                 removeNoise=FALSE,statNoise="avg", k=3,removeNegative=TRUE,
                                 smoothing=FALSE,smoothMethod="Spline",spar=NULL,sameTime=TRUE,time_x=NULL,negativeValuesToZero=TRUE)
{

  if(!"start"%in%colnames(metaData)){stop("'start' should be a colname of metaData corresponding to the start of the evaluation")}
  if(!"stop"%in%colnames(metaData)){stop("'stop' should be a colname of metaData")}
  if(!"into"%in%colnames(metaData)){stop("'into' should be a colname of metaData")}
  if(!"breathing"%in%colnames(metaData)){stop("'breathing' should be a colname of metaData")}
  if(!"file"%in%colnames(metaData)){stop("'file' should be a colname of metaData")}

  cycleLimits=NULL
  match.arg(stat ,c("sum","moy","sd","n","min","max","tmax","area"))
  call=list(ions=ions,correction=correction,halfWindowSize=halfWindowSize,method=method)
  res=data.frame()
  result_df=data.frame()
  if(is.null(metaData)){stop("Please enter a metaData data.frame with 'file', 'start', 'stop', 'into' and 'breathing' as columns")}
  minExpiMedian=maxInspiMedian=FALSE
  if(!is.null(minExpi))
  {
    if(minExpi=="median")  {minExpiMedian=TRUE}
  }

  if(!is.null(maxInspi))
  {
    if(maxInspi=="median"){maxInspiMedian=TRUE}
  }

  for(i in 1:length(listFiles))
  {
   file=listFiles[i]
   print(paste0(i,"/",length(listFiles)))
   pathFile=paste0(wd,"/",file)
   dataset=read.table(pathFile,sep=sep,header=T,dec=dec_vec[i])
   metaInfo=metaData[metaData[,"file"]==file,]
   if(!file%in%metaData[,"file"]){stop(paste0(file," is not in metaData"))}
   if(minExpiMedian)
   {
     minExpi=median(dataset[,metaInfo[1,"breathing"]])
    }

   if(maxInspiMedian)
   {
     maxInspi=median(dataset[,metaInfo[1,"breathing"]])
   }
   result_all=ptrvIntensityByTime(dataset=dataset,timeCol=timeCol, colToRemove=colToRemove,ions=ions,referenceBreath=metaInfo[1,"breathing"],
                                   timeStart=metaInfo[1,"start"],correction=correction,timePeriod=c(metaInfo[1,"start"],metaInfo[1,"stop"]),
                                   halfWindowSize=halfWindowSize,method=method,total=total,breathRatio=breathRatio,
                                   smoothMethodBreath=smoothMethodBreath,minimalDuration=minimalDuration,minExpi=minExpi,maxInspi=maxInspi,
                                   forMinExpiDivideMaxIntBy=forMinExpiDivideMaxIntBy,forMaxInspiDivideMaxIntBy=forMaxInspiDivideMaxIntBy)
    cycleLimits[[i]]=result_all$gg$p_cyclelimits

    result_all_df=result_all$res
    if(smoothing)
    {
      result_all_df=ptrvSmooth(dataset=result_all_df,spar=spar,sameTime=sameTime,time_x=time_x,method=smoothMethod,negativeValuesToZero=negativeValuesToZero)
    }
    if(removeNoise)
    {
      result_all_df=ptrvRemoveNoise(dataset=result_all_df,timeBlank=c(metaInfo[1,"start"],metaInfo[1,"into"]),stat=statNoise, k=k,removeNegative=removeNegative
        )
    }

    result_all_df[,"file"]=file
    result_all_df[,"product"]=metaInfo["product"]
    result_all_df[,"subject"]=metaInfo["subject"]
    result_all_df[,"rep"]=metaInfo["rep"]
    if(removeBlankTime){ result_all_df=ptrvComparableData(df=result_all_df,into=metaInfo[i,"into"],timeColumn="time")}

    result_df=rbind(result_df,result_all_df)
  #   res_cycle=ptrvIntensity(result_all_df)[,c("ion",stat)]
  #
  #
  #    #res_none=ptrvIntensity(result_all_none)[,c("ion","area")]
  #   if(i==1) { res=res_cycle;colnames(res)[2]=listFiles[1]}
  #   if(i>1){ res=merge(res,res_cycle,by="ion");colnames(res)[i+1]=listFiles[i]}
   }
  # mat=t(res[,-1])
  # mat2=as.data.frame(mat)
  # colnames(mat2)=res[,1]
  # ions2=colnames(mat2)
  # mat2[,"file"]=rownames(mat2)
  # dfMerged=merge(metaData[,c("file","product","subject","rep")],mat2,by="file")
  # colnames(dfMerged)[1:4]=c("file","product","subject","rep")
  # dfReshaped=reshape(dfMerged,varying=list(ions2),times=ions2,v.names="intensity",timevar="ion",direction="long")
   result=list(res=result_df,metaData=metaData,cycleLimits=cycleLimits,call=call)
  class(result)="ptrvList"
  return(result)
}