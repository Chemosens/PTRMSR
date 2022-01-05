library(ggplot2)
library(plotly)
library(reshape2)
library(gridExtra)
library(ggrepel)
data("ptrvList")
breath="m69.06989..isoprene...Conc."

respca=ptrvListPCA(ptrvList$totalIntensity,dataType="productMeans")
plotPCAgg(respca,type="ind")
plotPCAgg(respca,type="corCircle")
plotPCAgg(respca,type="cor1")
plotPCAgg(respca,type="cor2")

respca=ptrvListPCA(ptrvList$totalIntensity,dataType="raw")
plotPCAgg(respca,type="ind")
plotPCAgg(respca,type="ind",indSup="ell")
plotPCAgg(respca,type="ind",indSup=c("points","ell"))

plotPCAgg(respca,type="corCircle")
plotPCAgg(respca,type="cor1",n=90)
plotPCAgg(respca,type="cor2")

resbiplot=ptrvListPCA(ptrvList$totalIntensity,dataType="productMeans",representation="DistanceBiplot")
plotPCAgg(resbiplot,type="biplot")

resbiplot=ptrvListPCA(ptrvList$totalIntensity,dataType="productMeans",representation="DistanceBiplot",option="Covariance")
plotPCAgg(resbiplot,type="biplot")


test_that("pca",
          expect_true(!is.null(respca))
)
