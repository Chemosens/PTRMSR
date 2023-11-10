#' @title ptrvListIntensity
#' @param df a result of ptrvListIntensityByFiles ($res) ie a data.frame whose colnames are  'time ' 'ion' 'intensity' 'duration' 'file' and others
#' @param format "wide" by default, can also be 'long'
#' @param stat "area" by default but can also be "tmax" or "max"
#' @inheritParams ptrvIntensity
#' @importFrom utils read.table
#' @export
ptrvListIntensity=function(df,format="wide",stat="area", timePeriod = NULL, negativeAsNull = TRUE)
{
  listFiles=levels(factor(df[,"file"]))
  for(i in 1:length(listFiles))
  {
    file=listFiles[i]
    print(file)
    result_all=df[df[,"file"]==file,]
    if(format=="wide")
    {
        res_cycle=ptrvIntensity(result_all,timePeriod=timePeriod,negativeAsNull=negativeAsNull)[,c("ion",stat)]
        if(i==1) { res=res_cycle;colnames(res)[2]=listFiles[1]}
        if(i>1){ res=merge(res,res_cycle,by="ion");colnames(res)[i+1]=listFiles[i]}
    }
    if(format=="long")
   {
     res_cycle=ptrvIntensity(result_all,timePeriod=timePeriod,negativeAsNull=negativeAsNull)[,c("ion",stat)]
     res_cycle[,"file"]=listFiles[i]
     res=rbind(res,res_cycle)
    }

  }
  return(res)
}