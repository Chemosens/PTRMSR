library(ggplot2)
library(plotly)
library(reshape2)
library(gridExtra)
data("ptrvList")
#load("C:/INRA/Data/Données Cantin/pretreatedData.Rdata.RData")
#save(res_intensity_tds,file="ptrvList.RData")
# Visualiser le jdd brut ()

resanova=ptrvListAnova(ptrvList$totalIntensity,normalizeByEval=FALSE)
ph=ptrvListHeatmap(ptrvList$totalIntensity)

test_that("anova",
          expect_true(!is.null(resanova))
)
test_that("anova",
          expect_true(!is.null(ph))
)

