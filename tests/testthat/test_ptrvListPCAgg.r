library(ggplot2)
library(plotly)
library(reshape2)
library(gridExtra)
library(ggrepel)
library(CSUtils)
data("ptrvList")

respcagg1=PCAgg(df=ptrvList$totalIntensity[,c("product","subject","rep","ion","intensity")],dataType="raw",option="Covariance",representation="TwoMaps",value.var="intensity",variable="ion")
respcagg2=PCAgg(df=ptrvList$totalIntensity[,c("product","subject","rep","ion","intensity")],dataType="productMeans",option="Covariance",representation="TwoMaps",value.var="intensity",variable="ion")
respcagg1c=PCAgg(df=ptrvList$totalIntensity[,c("product","subject","rep","ion","intensity")],dataType="raw",option="Correlation",representation="TwoMaps",value.var="intensity",variable="ion")
respcagg2c=PCAgg(df=ptrvList$totalIntensity[,c("product","subject","rep","ion","intensity")],dataType="productMeans",option="Correlation",representation="TwoMaps",value.var="intensity",variable="ion")

p1=plotPCAgg(respcagg1c,type="ind",text=TRUE)
p2=plotPCAgg(respcagg1c,type="corCircle")
p1
p2

p1=plotPCAgg(respcagg2c,type="ind",text=TRUE)
p2=plotPCAgg(respcagg2c,type="corCircle",text=FALSE)
p1
p2


p3=plotPCAgg(respcagg1c,type="cor1",n=10)
p4=plotPCAgg(respcagg1c,type="cor2",n=10)
p3
p4

test_that("pca",
          expect_true(!is.null(respcagg1))
)

