#' ptrvListAnova
#' returns the two way mixed ANOVA product*subject (with random subject effect) for each ion
#' @param df data frame with "ion","intensity","product","subject" columns,
#' @param ionToRemove vector of ion to remove from the analysis
#' @param log if TRUE the data is logged
#' @param normalizeByEval if TRUE, each evaluation (row) of the matrix is divided by the sum of all the variables for this evaluation (row sum)
#' @param model "intensity~product+(1|subject)+(1|product:subject)" or other model containing these factors
#' @param alpha limit of significance
#' @param columns choose between c("G_Mean","F_Product","P_Product","F_Product_Significance","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product","P_Subject") )
#' @export
ptrvListAnova=function(df,ionToRemove=NULL,log=FALSE,normalizeByEval=FALSE, model="intensity~product+(1|subject)+(1|product:subject)",alpha=0.05,columns=c("G_Mean","F_Product","P_Product","F_Product_Significance","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product","P_Subject") )
{
  if(log) { df[df[,"intensity"]<0,"intensity"]=min(df[df[,"intensity"]>0,"intensity"],na.rm=T);df[,"intensity"]=log(df[,"intensity"])    }
  ionToUse=unique(df[,"ion"]); ionToUse=ionToUse[!ionToUse%in%ionToRemove]
  if(normalizeByEval){df=normalizeByEval(df,ionToUse)$intens}
  unit="ion"

  if (length(unique(df$rep))>1) {
    model="intensity~product+(1|subject)+(1|product:subject)"
  } else {
    stop("no replicate, please enter model='intensity~product+(1|subject)'")
  }
  res=anovaTable(df=df, unit="ion", model=model, alpha=alpha, columns=columns)
  return (res)
}