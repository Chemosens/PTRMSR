#' @title analysis.listPtrv
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
totalPCA=function(df,ionToRemove=NULL,dataType="productMeans",normalizeByEval=FALSE, log=FALSE,axes=list(c(1,2)),representation="TwoMaps",option="Correlation",variableName="ion",scoreName="intensity",expandBiplot=NULL)
{
  time=NULL
   df=df[,c("product","subject","rep",variableName,scoreName)]
   ionToUse=unique(df[,variableName]); ionToUse=ionToUse[!ionToUse%in%ionToRemove]
   df=df[df[,variableName]%in% ionToUse,]
   if(log) { df[,scoreName]=log(df[,scoreName])    }
   if(normalizeByEval){df=normalizeByEval(df,ionToUse)$intens}
   # parameters$option="correlation"
   # parameters$title="Correlation PCA"
   # result=runBy(df=df, runBy=runBy, outputFormat=outputFormat, outputFile=outputFile, title=title, fun=ptrvListPca, parameters=parameters)

    result=PCAgg(df,dataType=dataType,option=option,representation=representation,value.var=scoreName,variable=variableName,axes=axes,expandBiplot=expandBiplot)
  return(result)
}
