#' @title ptrvListIntensity
#' @param listFiles vector containing the file names of the files to be read
#' @param metaData metaData required #TODO
#' @param format "wide" by default
#' @param correction "cycle" by default.
#' @param stat "area" by default but can also be tmax or max
#' @importFrom utils read.table
#' @export
ptrvListIntensity=function(listFiles,metaData=NULL,format="wide",correction="cycle",stat="area")
{
  res=data.frame()
  for(i in 1:length(listFiles))
  {
    file=listFiles[i]
    print(file)
    dataset=read.table(file,sep='\t',header=T)
    metaInfo=metaData[metaData[,"file"]==file,]
    print(metaInfo)
    result_all=ptrvIntensityByTime(dataset=dataset,referenceBreath=metaInfo["resp"],correction=correction,timePeriod=c(0,metaInfo["finTimeSens_s"]),removeNoise=TRUE,timeBlank = as.numeric(metaInfo["miseEnBouche_s"]))
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