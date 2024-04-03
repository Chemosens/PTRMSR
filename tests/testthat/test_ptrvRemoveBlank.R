df=data.frame(time=0:100, intensity=1+sin(0:100),ion="ion")
df[df[,"time"]>50,"intensity"]=df[df[,"time"]>50,"intensity"]+1
plot(df[,"intensity"],type="l")
res_max=ptrvRemoveNoise(df,timeBlank=c(0,50),stat="max", k=3,removeNegative=TRUE)
res_avg=ptrvRemoveNoise(df,timeBlank=c(0,50),stat="avg", k=3,removeNegative=TRUE)

plot(res_max[,"intensity"],type="l")
plot(res_avg[,"intensity"],type="l")

