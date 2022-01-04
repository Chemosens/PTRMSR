#' @title ptrvListComparableData
#' @param metaData metaData containing these columns: "finPTR_s","miseEnBouche_s"
#' @param df data.frame with time column
#' @param timeColumn name of the time column in df
#' @export
ptrvListComparableData=function(df,metaData,timeColumn="time")
{
  df_res=data.frame()
  metaDataDuration=metaData[,"finPTR_s"]-metaData[,"miseEnBouche_s"]
  stopTime=min(metaDataDuration,na.m=T)
  print(stopTime)
  for(file in metaData[,"file"])
  {
    df_file=df[df[,"file"]==file,]
    df_file[,"time"]=as.numeric(as.character(df_file[,"time"]))
    miseEnBouche=metaData[metaData[,"file"]==file,"miseEnBouche_s"]
    df_file2=ptrvComparableData(df_file,miseEnBouche=miseEnBouche,stopTime=stopTime,timeColumn=timeColumn)
    df_res=rbind(df_res,df_file2)
  }
  return(df_res)
}

