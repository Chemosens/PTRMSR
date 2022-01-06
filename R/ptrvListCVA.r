#' @title ptrvListCVA
#' @param df data frame with "ion","intensity","product","subject" columns,
#' @param ionToRemove vector of character containing ions to remove from the analysis
#' @param normalizeByEval Boolean. By default TRUE
#' @param log boolean indicated if the intensities should be logged (TRUE) or not (FALSE)
#' @param option "OneWayANOVA" or "TwoWayANOVA"
#' @param axes a list of couples of integers corresponding to the maps to be obtained
#' @export
#' @importFrom CSUtils PCAgg gMapPlot CVAgg gDistributionPlot gGetLegend anovaTable gBarPlot runBy
ptrvListCVA=function(df,ionToRemove=NULL,option="OneWayANOVA",normalizeByEval=FALSE, log=FALSE,axes=c(1,2))
{
  time=NULL
   df=df[,c("product","subject","rep","ion","intensity")]
   ionToUse=unique(df[,"ion"]);
   ionToUse=ionToUse[!ionToUse%in%ionToRemove]
   df=df[df[,"ion"]%in% ionToUse,]
   if(log) { df[,"intensity"]=log(df[,"intensity"])    }
    if(normalizeByEval){df=normalizeByEval(df,ionToUse)$intens}
     colnames(df)=c("product","subject","rep","descriptor","score")
    result=CVAgg(df,list(axes),option=option)
  return(result)
}
