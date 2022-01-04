#' @title ptrvQCAverages
#' @description returns dataframe containing the average (avg) by product and day
#' @param df a dataframe with name,days,products, replicates as first columns. Further columns corresponds to the intensity relatively to ions (one ion by column)
#' @param ions vector containing the names of the column to be considered as ions and to be analysed
#' @export
ptrvQCAverages=function(df,ions)
{
  if(is.null(ions)){ions=colnames(df)[-c(1:4)]}
  df_long=reshape(df,varying=list(ions),timevar="ion",times=ions,v.names="area",direction="long")
  res=dcast(df_long,days+products~ions,fun.aggregate=mean,value.var="area")
  return(res)
}