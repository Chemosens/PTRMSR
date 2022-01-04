ptrvListGetHeights=function(df,discretisation=5,type="all") 
{
  maxTime=max(df[,"time"]+df[,"duration"]/2,na.rm=T)
# df[,"height"]=df[,"intensity"]/df[,"duration"]
  rep=df[["rep"]]
  product=df[["product"]]
  subject=df[["subject"]]
  ion=df[["ion"]]
      duration=seq(0,maxTime,discretisation)[2]-seq(0,maxTime,discretisation)[1]
    completeInt=expand.grid( product=unique(df$product),  subject=unique(df$subject),rep=unique(df$rep), ion=unique(df$ion),time=seq(0,maxTime,discretisation))
   completeInt[,"intensity"]=0
  for(i in 1:dim(df)[1])
  {
    suj=df[i,"subject"]
    prod=df[i,"product"]
    rep=df[i,"rep"]
    ion=df[i,"ion"]
    t1=df[i,"time"]-df[i,"duration"]/2
    t2=df[i,"time"]+df[i,"duration"]/2
    indexToComplete=completeInt[,"subject"]==suj&completeInt[,"rep"]==rep&completeInt[,"product"]==prod & completeInt[,"ion"]==ion &completeInt[,"time"]<=t2&completeInt[,"time"]>=t1
    completeInt[indexToComplete,"intensity"]= completeInt[indexToComplete,"intensity"]+df[i,"intensity"]
  }
  completeInt[,"intensity"]=completeInt[,"intensity"]/duration
  completeInt[,"duration"]=duration
  return(completeInt)
}




