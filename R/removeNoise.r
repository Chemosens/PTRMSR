removeNoise=function(resultMeanT,ions,colIons="ion",timeBlank)
{
  for(ion in unique(res[,"ion"]))
  {
    #average of the noise to be removed
    maxNoise[ion]=max(resultMeanT[resultMeanT[,"time"]<=timeBlank[2]&resultMeanT[,"time"]>=timeBlank[1]&resultMeanT[,"ion"]==ion,"intensity"],na.rm=T)
    avgNoise[ion]=mean(resultMeanT[resultMeanT[,"time"]<=timeBlank[2]&resultMeanT[,"time"]>=timeBlank[1]&resultMeanT[,"ion"]==ion,"intensity"],na.rm=T)
    sdNoise[ion]=sd(resultMeanT[resultMeanT[,"time"]<=timeBlank[2]&resultMeanT[,"time"]>=timeBlank[1]&resultMeanT[,"ion"]==ion,"intensity"],na.rm=T)

    resultMeanT[resultMeanT[,"ion"]==ion,"intensity"]=resultMeanT[resultMeanT[,"ion"]==ion,"intensity"]-avgNoise[ion]
  }
  return(resultMeanT)
}

