#' @title Runs PCA on a data frame stemming from ptrvReadList()
#' @param df data frame with "ion","intensity","product","subject" columns,
#' @param dataType  "raw" (if PCA should be computed on the whole dataset) or "productMeans" if PCA should be computed on the product mean table
#' @param axes list of couples of integers corresponding to the maps to plot
#' @param representation "twoMaps" or "distanceBiplot"
#' @param option "Correlation" or "Covariance"
#' @param variableName column containing the variable names. Default to 'ion'
#' @param expandBiplot a number corresponding to the expansion of the variables compared to the individuals in the biplot.
#' @param scoreName column name of the score. Default to 'intensity'
#' @param ionToRemove vector of character containing ions to remove from the analysis
#' @param normalizeByEval Boolean. By default TRUE.
#' @param log boolean indicated if the intensities should be logged (TRUE) or not (FALSE)
#' @export
#' @importFrom pheatmap pheatmap
#' @importFrom utils tail
#' @importFrom grDevices rainbow
#' @importFrom CSUtils PCAgg gMapPlot CVAgg gDistributionPlot gGetLegend gBarPlot runBy
ptrvListPCA=function(df,ionToRemove=NULL,dataType="productMeans",normalizeByEval=FALSE,log=FALSE,axes=list(c(1,2)),representation="TwoMaps",option="Correlation",variableName="ion",scoreName="intensity",expandBiplot=NULL)
{
  time=NULL
   df=df[,c("product","subject","rep",variableName,scoreName)]
   ionToUse=unique(df[,variableName]); ionToUse=ionToUse[!ionToUse%in%ionToRemove]
   df=df[df[,variableName]%in% ionToUse,]
   if(log) { df[,scoreName]=log(df[,scoreName])    }
   if(normalizeByEval){df=normalizeByEval(df,ionToUse)$df}
   print(df[1,])
    result=PCAgg(df,dataType=dataType,option=option,representation=representation,value.var=scoreName,variable=variableName,axes=axes,expandBiplot=expandBiplot)
  return(result)
}
