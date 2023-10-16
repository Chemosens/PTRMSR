#' @title ptrvListIntensityByTime
#' A function automating all pretreatements for several files,removing the noise and the breathing biases
#' @param listFiles list of the file names to be imported
#' @param metaData A data.frame with colnames as "file","product","subject","rep","breathing","into","swallow","end")
#' @param dec_vec Vector with the same length as listFiles containing the decimal separator used in the list of files
#' @param sep separator used in the files to be read
#' @param removeBlankTime boolean. If TRUE, the time of blank time is substracted from the time column
#' @param stat Statistic to be extracted for each ion in each file. Among c("sum","moy","sd","n","min","max","tmax","area")
#' @param ions vector of ion to be analyzed
#' @param wd working directory where the files of listFiles are.
#' @importFrom utils read.table
#' @return a list containing : total intensity: AUC (or tmax, etc) in a long format, time: data containing the corrected timepoints, listRes: AUC in a wide format
#' @inheritParams ptrvIntensityByTime
#' @export
#'
ptrvListIntensityByTime=function(listFiles,timeCol="RelTime",colToRemove=c("AbsTime","Cycle"),removeBlankTime=FALSE,ions=NULL,dec_vec=rep(".",length(listFiles)),sep="\t",metaData=NULL,correction="cycle",timeBlank=c(0,30),halfWindowSize=5,method="MAD",total=FALSE,breathRatio=FALSE,stat="area",minimalDuration=2,smoothMethod="MovingAverage",minExpi=NULL,maxInspi=NULL,forMinExpiDivideMaxIntBy=5,forMaxInspiDivideMaxIntBy=4,wd=getwd())
{
  cycleLimits=NULL
  match.arg(stat ,c("sum","moy","sd","n","min","max","tmax","area"))
  call=list(ions=ions,correction=correction,timeBlank=timeBlank,halfWindowSize=halfWindowSize,method=method)
  res=data.frame()
  result_df=data.frame()
  if(is.null(metaData)){stop("Please enter a metaData data.frame with 'file', 'start', 'stop', 'into' and 'breathing' as columns")}
  for(i in 1:length(listFiles))
  {
   file=listFiles[i]
   print(paste0(i,"/",length(listFiles)))
   pathFile=paste0(wd,"/",file)
   dataset=read.table(pathFile,sep=sep,header=T,dec=dec_vec[i])

   metaInfo=metaData[metaData[,"file"]==file,]
   if(!file%in%metaData[,"file"]){stop(paste0(file," is not in metaData"))}
   if(!"start"%in%colnames(metaData)){stop("'start' should be a colname of metaData")}
   if(!"stop"%in%colnames(metaData)){stop("'stop' should be a colname of metaData")}
   if(!"into"%in%colnames(metaData)){stop("'into' should be a colname of metaData")}
   if(!"breathing"%in%colnames(metaData)){stop("'breathing' should be a colname of metaData")}
   result_all=ptrvIntensityByTime(dataset=dataset,timeCol=timeCol, colToRemove=colToRemove,ions=ions,referenceBreath=metaInfo[1,"breathing"],
                                   timeStart=metaInfo[1,"start"],correction=correction,timePeriod=c(metaInfo[1,"start"],metaInfo[1,"stop"]),
                                   removeNoise=TRUE,timeBlank = c(metaInfo[1,"start"],metaInfo[1,"into"]),
                                   halfWindowSize=halfWindowSize,method=method,total=total,breathRatio=breathRatio,
                                   smoothMethod=smoothMethod,minimalDuration=minimalDuration,minExpi=minExpi,maxInspi=maxInspi,
                                   forMinExpiDivideMaxIntBy=forMinExpiDivideMaxIntBy,forMaxInspiDivideMaxIntBy=forMaxInspiDivideMaxIntBy)
    cycleLimits[[i]]=result_all$gg$p_cyclelimits
    result_all_df=result_all$res
    result_all_df[,"file"]=file
    result_all_df[,"product"]=metaInfo["product"]
    result_all_df[,"subject"]=metaInfo["subject"]
    result_all_df[,"rep"]=metaInfo["rep"]
    if(removeBlankTime){ result_all_df=ptrvComparableData(df=result_all_df,into=metaInfo[i,"into"],timeColumn="time")}
    result_df=rbind(result_df,result_all_df)
    res_cycle=ptrvIntensity(result_all_df)[,c("ion",stat)]


     #res_none=ptrvIntensity(result_all_none)[,c("ion","area")]
    if(i==1) { res=res_cycle;colnames(res)[2]=listFiles[1]}
    if(i>1){ res=merge(res,res_cycle,by="ion");colnames(res)[i+1]=listFiles[i]}
  }
  mat=t(res[,-1])
  mat2=as.data.frame(mat)
  colnames(mat2)=res[,1]
  ions2=colnames(mat2)
  mat2[,"file"]=rownames(mat2)
  dfMerged=merge(metaData[,c("file","product","subject","rep")],mat2,by="file")
  colnames(dfMerged)[1:4]=c("file","product","subject","rep")
  dfReshaped=reshape(dfMerged,varying=list(ions2),times=ions2,v.names="intensity",timevar="ion",direction="long")
  result=list(totalIntensity=dfReshaped,time=result_df,listRes=dfMerged,metaData=metaData,cycleLimits=cycleLimits,call=call)
  class(result)="ptrvList"
  return(result)
}