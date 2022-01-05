library(ggplot2)
library(plotly)
library(reshape2)
library(gridExtra)
data("ptrvList")
ptrvListHeatmap(df=ptrvList$totalIntensity,formula=as.formula(product+subject+rep~ion),fun.aggregate="mean",clusterRows=T,clusterCols=T)
ptrvListHeatmap(df=ptrvList$totalIntensity,formula=as.formula(product+subject+rep~ion),fun.aggregate="mean",clusterRows=T,clusterCols=T)
ptrvListHeatmap(df=ptrvList$totalIntensity,formula=as.formula(product+subject+rep~ion),fun.aggregate="max",clusterRows=T,clusterCols=T)

ptrvListHeatmap(df=ptrvList$totalIntensity,formula=as.formula(product+subject~ion),fun.aggregate="max",clusterRows=T,clusterCols=T,showRownames=T)
ptrvListHeatmap(df=ptrvList$totalIntensity,formula=as.formula(product+subject~ion),fun.aggregate="mean",clusterRows=T,clusterCols=T,showRownames=T)

ptrvListHeatmap(df=ptrvList$totalIntensity,formula=as.formula(product+subject~ion),fun.aggregate="mean",clusterRows=T,clusterCols=T,showRownames=T,annotationRow="subject")

ptrvListHeatmap(df=ptrvList$totalIntensity,formula=as.formula(product+subject~ion),normalizeByEval = TRUE,fun.aggregate="mean",clusterRows=T,clusterCols=T,showRownames=T,annotationRow="subject")

ph=ptrvListHeatmap(df=ptrvList$totalIntensity,formula=as.formula(product+subject~ion),normalizeByEval = FALSE,fun.aggregate="mean",clusterRows=T,clusterCols=T,showRownames=T,annotationRow="subject")

test_that("anova",
          expect_true(!is.null(ph))
)
