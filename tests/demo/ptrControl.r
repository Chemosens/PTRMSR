# etalonnage

setwd("C:/INRA/Data/fichiersTXTPTREtalonnage")
library(reshape2)
library(gridExtra)
library(ggplot2)
library(PTRMSR)

repo="C:/INRA/Data/fichiersTXTPTREtalonnage"
dfs=ptrvQCGetDfs(repo)
p=ptrvQCPlotSummary(dfs$auc)
grid.arrange(grobs=p$bars)
grid.arrange(grobs=p$boxplot)
grid.arrange(grobs=p$points)

# Second output
ptrvQCStats(dfs$auc)
ptrvQCKruskal(dfs$auc)
  
