#' @title ptrvListComparableData
#' @param metaData metaData containing these columns: "finPTR_s","miseEnBouche_s"
#' @param df data.frame with time column
#' @param timeColumn name of the time column in df
#' @param nameEndMeta default to "finTimeSens_s". Name of the metaData column containing the end of the tasting
#' @param nameInMouthMeta default to "miseEnBouche_s". Name of the column of metaData file containing the time of the beginning of the tasting (after the blank)
#' @param nameFileMeta default to "file". Name of the column of metaData file containing the names of the files (with their extension ! for example file1.txt)
#' @export
ptrvListComparableData=function(df,metaData,timeColumn="time",nameInMouthMeta="miseEnBouche_s",nameFileMeta="file",nameEndMeta="finTimeSens_s")
{
  df_res=data.frame()
  metaDataDuration=metaData[,"finPTR_s"]-metaData[,nameInMouthMeta]
  stopTime=min(metaDataDuration,na.m=T)
  print(stopTime)
  for(file in metaData[,nameFileMeta])
  {
    df_file=df[df[,nameFileMeta]==file,]
    df_file[,timeColumn]=as.numeric(as.character(df_file[,timeColumn]))
    miseEnBouche=metaData[metaData[,nameFileMeta]==file,nameInMouthMeta]
    df_file2=ptrvComparableData(df_file,into=miseEnBouche,stopTime=stopTime,timeColumn=timeColumn)
    df_res=rbind(df_res,df_file2)
  }
  return(df_res)
}

