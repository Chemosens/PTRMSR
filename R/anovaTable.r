#' @title Run a batch of ANOVAs and store the results in a table.
#' @description Run a batch of ANOVAs and store the results in a table.
#' @param df A dataframe with columns subject, product, "unit", score.
#' @param unit The name of the column of the dataframe that will be used as a loop.
#' @param model A lmer model of ANOVA.
#' @param alpha Risk for the post-hoc multiple comparison test.
#' @param columns Elements to add in the table.
#' @return A dataframe.
#' @importFrom stats residuals shapiro.test
#' @importFrom car Anova leveneTest
#' @importFrom lme4 lmer
#' @importFrom lsmeans lsmeans
#' @importFrom multcomp cld
#' @importFrom lmerTest rand
#' @importFrom CSUtils starPValue
#' @export
#' @examples{
#' #data(duration)
#' #anovaTable(duration,model='score~product')
#' }
anovaTable=function(df, model, alpha=0.05, unit="descriptor", columns=c("G_Mean","F_Product","P_Product","F_Product_Significance","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product")) {

  tableANOVA=as.data.frame(unique(as.character(df[,unit])))
  colnames(tableANOVA)=unit
  tableANOVA$ID=0

  # TODO : LSD, RMSE
  computeANOVA=function(x) {

    att=as.character(x[[unit]])

    d=df[df[,unit]==att,]
    index=tableANOVA[,unit]==att

    res=lmer(as.formula(model), data=d)
    res.anova=Anova(res,type=3,singular.ok=TRUE,test.statistic="F")

    res.lsmeans=lsmeans(res,~ product)
    res.cld=cld(res.lsmeans, alpha=alpha, Letters=letters, adjust="tukey")

    if ("G_Mean" %in% columns) {
      tableANOVA[index,"G_Mean"]<<-round(mean(d$score),2)
    }
    if ("F_Product" %in% columns) {
      tableANOVA[index,"F_Product"]<<-round(res.anova["product","F"],2)
    }
    if ("F_Product_Significance" %in% columns) {
      tableANOVA[index,"F_Product_Significance"]<<-starPValue(res.anova["product","Pr(>F)"])
    }
    if ("P_Product" %in% columns) {
      tableANOVA[index,"P_Product"]<<-round(res.anova["product","Pr(>F)"],3)
    }
    if ("SE_Product" %in% columns) {
      tableANOVA[index,"SE_Product"]<<-round(res.cld[1,"SE"],2)
    }
    # if ("R2" %in% columns) {
    #   tableANOVA[index,"R2"]<<-round(r.squaredGLMM(res)[,"R2c"],2)
    # }
    if ("P_ShapiroWilks" %in% columns) {
      tableANOVA[index,"P_ShapiroWilks"]<<-round(shapiro.test(residuals(res))$p.value,3)
    }
    if ("P_LeveneProduct" %in% columns) {
      tableANOVA[index,"P_Levene_Product"]<<-round(leveneTest(residuals(res) ~ d$product)["group","Pr(>F)"],3)
    }
    if ("Mean_Product" %in% columns) {
      for (p in sort(res.cld[,"product"])) {
        tableANOVA[index,paste("Mean_",p,sep="")]<<-round(res.cld[res.cld$product==p,"lsmean"],2)
      }
    }
    if ("Group_Product" %in% columns) {
      for (p in sort(res.cld[,"product"])) {
        tableANOVA[index,paste("Group_",p,sep="")]<<-trimws(res.cld[res.cld$product==p,".group"])
      }
    }

    if ("P_Subject" %in% columns) {
      resRandom=rand(res)
      tableANOVA[index,"P_Subject"]<<-round(resRandom["(1 | subject)","Pr(>Chisq)"],2)
    }

    if ("P_ProductSubject" %in% columns) {
      resRandom=rand(res)
      tableANOVA[index,"P_ProductSubject"]<<-round(resRandom["(1 | product:subject)","Pr(>Chisq)"],2)
    }


  }

  apply(tableANOVA,1, computeANOVA)

  tableANOVA=tableANOVA[order(tableANOVA$P_Product),]

  tableANOVA$ID=NULL

  #  tableANOVA=print(tableANOVA, row.names = FALSE)

  #TODO : supprimer rownames

  return (tableANOVA)
}