#' @title ptrvQCKruskal 
#' @param df a dataframe with name,days,products, replicates as first columns. Further columns corresponds to the intensity relatively to ions (one ion by column)
#' @return results of kruskal wallis test on days
#' @importFrom stats kruskal.test
#' @export
ptrvQCKruskal=function(df)
{
  moyG=apply(df[,-c(1,2,3,4)], 2, function(x){return(mean(x,na.rm=T))})
  statKW=apply(df[,-c(1,2,3,4)], 2, function(x){return(kruskal.test(x,df[,"days"])$stat)})
  pval=apply(df[,-c(1,2,3,4)], 2, function(x){return(kruskal.test(x,df[,"days"])$p.value)})
  res=data.frame(average=moyG,stat=statKW,pval=pval)
  write.table(res,row.names=FALSE,sep=";",file="res_kruskal.csv")
  return(res)
}