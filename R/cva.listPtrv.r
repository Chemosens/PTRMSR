#' @title cva.listPtrv
#' @param type  Choose among: "CVA of abundances","ANOVA of intensities","PCA of abundances","Intensity Heatmap"
#' @param ionToRemove vector of character containing ions to remove from the analysis
#' @param normalizeByEval Boolean. By default TRUE
#' @param log boolean indicated if the intensities should be logged (TRUE) or not (FALSE)
#' @param df data frame with "ion","intensity","product","subject" columns,
#' @export
#' @importFrom pheatmap pheatmap
#' @importFrom utils tail
#' @importFrom grDevices rainbow
#' @importFrom chemosensR PCAgg gMapPlot CVAgg gDistributionPlot gGetLegend anovaTable gBarPlot runBy
totalCVA=function(df,ionToRemove=NULL,option="OneWayANOVA",normalizeByEval=FALSE, log=FALSE,axes=c(1,2))
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
