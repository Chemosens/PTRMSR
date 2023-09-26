data(ptrv)
library(ggplot2)
breath="m69.06989..isoprene...Conc."
sigIons=ptrvSignificantSNRIons(ptrv,referenceBreath=breath,method="max")
sigIonst=ptrvSignificantSNRIons(ptrv,referenceBreath=breath,method="ttest")
sigIonst$listIons
result_deg=ptrvIntensityByTime(dataset=ptrv,referenceBreath=breath,correction="cycle",timePeriod=NULL,removeNoise=FALSE)
restoplot=result_deg$res[result_deg$res[,"ion"]%in%sigIons$listIons,]
restoplott=result_deg$res[result_deg$res[,"ion"]%in%sigIonst$listIons,]

p=ggplot(restoplot,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()
#ggplotly(p)

p=ggplot(restoplott,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()
#ggplotly(p)

sigIons$listIons
bool0=all(sigIons$listIons%in%c("m115.11139..C7H15O....Conc." , "m115.14328..m115_5...Conc.",
 "m117.09160..C6H13O2....Conc." ,"m75.05924..C4H11O....Conc.",
"m87.08017..C5H11O....Conc.",   "m87.09636..m87_3...Conc.",
 "m91.07115..C4H11O2....Conc.") )
test_that("All significant are the wanted ones",expect_true(bool0))