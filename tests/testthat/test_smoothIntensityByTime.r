
 data(ptrv)
 breath="m69.06989..isoprene...Conc."
 res_ibt=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction="cycle",halfWindowSize=5)
 dataset=res_ibt$res
 res_smooth=ptrvSmooth(dataset,spar=NULL,sameTime=TRUE,time_x=NULL,method="Spline",negativeValuesToZero=TRUE)
sum(res_smooth$duration>0)==3195
