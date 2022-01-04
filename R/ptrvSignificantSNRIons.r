#' @title ptrvSignificantSNRIons
#' @inheritParams ptrvIntensityByTime
#' @param noisePeriod vector with two numbers indicating the period corresponding to a blank period.
#' @param multiplyNoiseBy a number such as a ion whose maximal intensity during tasting/maximal intensity during blank period > multiplyNoiseBy is detected as significant (used for method=="max")
#' @param method "max" or "ttest" (TODO "tangente")
#' @param removeNoise if TRUE, the noise is removed in the calculation of intensity by time
#' @export
ptrvSignificantSNRIons=function(dataset,method="max",referenceBreath,noisePeriod=c(0,30),correction="cycle",multiplyNoiseBy=3,removeNoise=FALSE,minimalDuration=2,maxPeaks=NULL)
{
  result_deg=ptrvIntensityByTime(dataset=dataset,referenceBreath=referenceBreath,correction=correction,timePeriod=NULL,removeNoise=removeNoise,minimalDuration=minimalDuration,maxPeaks=maxPeaks)
   # distribution du max=
    if(method=="max")
  {
      res_blanc=ptrvIntensity(result_deg$res,timePeriod=noisePeriod)
      res_deg=ptrvIntensity(result_deg$res,timePeriod=c(noisePeriod[2],max(result_deg$res[,"time"])))
      blanc=res_blanc[,"max"];names(blanc)=res_blanc[,"ion"]
      degus=res_deg[,"max"];names(degus)=res_deg[,"ion"]
      snRatio=degus/blanc[names(degus)]
      names(snRatio)=names(degus)
      listIons=names(snRatio)[snRatio>multiplyNoiseBy]
    }
  if(method=="ttest")
  {
    ions=unique(result_deg$res[,"ion"])
    pval=rep(NA,length(ions));names(pval)=ions
    for(ion in ions)
    {
      res_tmp=result_deg$res[result_deg$res[,"ion"]==ion,]
      res_tmp_blank=res_tmp[res_tmp[,"time"]<=noisePeriod[2]&res_tmp[,"time"]>=noisePeriod[1],"intensity"]
      res_tmp_deg=res_tmp[res_tmp[,"time"]>noisePeriod[2],"intensity"]
      pval[ion]=wilcox.test(res_tmp_blank,res_tmp_deg,alternative="less")$p.value
    }
    listIons=names(pval[pval<0.05/length(ions)])
    snRatio=pval
  }

  return(list(listIons=listIons,snRatio=snRatio))
}
