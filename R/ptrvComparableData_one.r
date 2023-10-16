#' @title ptrvComparableData
#' @param df data.frame with time column to be translated
#' @param into Number corresponding to the starting time of tasting
#' @param stopTime End of the tasting time
#' @param timeColumn Name of the column time by default (="time")
#' @export
ptrvComparableData=function(df,into=0,stopTime=Inf,timeColumn="time")
{
  df[,timeColumn]=as.numeric(as.character(df[,timeColumn]))
  df[,timeColumn]=df[,timeColumn]-into
  df=df[df[,timeColumn]>=0,]
  df=df[df[,timeColumn]<=stopTime,]
  return(df)
}
