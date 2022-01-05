#' @title pickingPeaks
#'@importFrom MSnbase pickPeaks
#'@param df a dataframe containing two columns: "mz" and "intensity" corresponding respectively to abscissa and ordinates.
#'@param col name of the column containing the abscissa ("mz" by default)
#'@param signalPercentage of the centroid's intensity are used to calculate the refined m/z. By default the descend is stopped when the first signal that is equal or larger than the last observed one is encountered.
#'@param method "MAD"
#'@param refineMz "descendPeak" (see MSn base::pickPeaks)
#'@param nPoints n points required for smoothing  (see sgolayfilt)
#'@export
#'@importFrom signal sgolayfilt
pickingPeaks=function(df,col="mz",signalPercentage=10,nPoints=0,refineMz="descendPeak",method="MAD")
{
  if(nPoints>0)
  {
    sgolay0=sgolayfilt(x=df[,"intensity"], p = 3,n=nPoints,m=0)
    df[,"intensity"]=sgolay0
    df[df[,"intensity"]<0,"intensity"]=0
  }

  sp <- new("Spectrum1",
            intensity =df[,"intensity"],
            mz = df[,col],
            centroided = FALSE)
  sp2 <- MSnbase::pickPeaks(sp, refineMz = refineMz, signalPercentage = signalPercentage,method=method)
  dfres=data.frame(x=MSnbase::mz(sp2),intensity=MSnbase::intensity(sp2))
  df2=dfres[order(dfres[,"intensity"],decreasing=T),]
  df2[,"relative"]=100*df2[,"intensity"]/max(df2[,"intensity"])
  return(list(df=df2,smooth=df))
}
