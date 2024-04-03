#' @param dataset dataframe whose names are time, intensity and ion
#' @param timeBlank a vector of length two corresponding to the period of blank.
#' @param stat "max", "avg", "avgKsd" or "med': statistic to be removed from the rest of the data
#' @param k if stat ="avgKsd" is chosen, represents the average + k*the standard deviations or "med': statistic to be removed from the rest of the data
#' @param removeNegative if TRUE any negative value is put to zero after the
#' @export
ptrvRemoveNoise=function(dataset,timeBlank,stat="max", k=3,removeNegative=TRUE)
{
  ions=unique(dataset[,"ion"])
  statNoise=rep(NA,length(ions));names(statNoise)=ions
  for(ion in ions)
  {
    #average of the noise to be removed
    datasetion=dataset[dataset[,"time"]<=timeBlank[2]&dataset[,"time"]>=timeBlank[1]&dataset[,"ion"]==ion,]
    if(dim(datasetion)[1]==0){stop("the timeBlank period is too short, no timepoints inthere")}
    if(stat=="max")
    {
      statNoise[ion]=max(datasetion[,"intensity"],na.rm=T)
    }
    if(stat=="avg")
    {
      statNoise[ion]=mean(datasetion[,"intensity"],na.rm=T)
    }
    if(stat=="med")
    {
      statNoise[ion]=median(datasetion[,"intensity"],na.rm=T)
    }
    if(stat=="avgKsd")
    {
      sdNoise=sd(datasetion[,"intensity"],na.rm=T)
      avg=mean(datasetion[,"intensity"],na.rm=T)
      statNoise[ion]=avg+k*sdNoise
    }
     dataset[dataset[,"ion"]==ion,"intensity"]=dataset[dataset[,"ion"]==ion,"intensity"]-statNoise[ion]
  }
  if(removeNegative)
  {
    dataset[dataset[,"intensity"]<0,"intensity"]=0
  }

  return(dataset)
}