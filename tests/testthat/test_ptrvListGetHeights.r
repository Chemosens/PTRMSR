# df=data.frame(subject="S01",product="P01",rep=1,ion=c("I1","I2","I3"),time=c(1,2,7),duration=c(1,5,1),intensity=c(1,10,4),start=0,stop=15)
# res_intensity=ptrIntensityByTime(df)
# ptrvListGetHeights(df=df)
# 
# timint=res_intensity$time[res_intensity$time[,"file"]=="CSCA098_S002_2.txt",]
# timint[timint[,"intensity"]<0,"intensity"]=0
# timint[,"xmin"]=as.numeric(as.character(timint[,"time"]))-as.numeric(as.character(timint[,"duration"]))/2
# timint[,"xmax"]=as.numeric(as.character(timint[,"time"]))+as.numeric(as.character(timint[,"duration"]))/2
# timint[,"ymin"]=0
# timint[,"ymax"]=timint[,"intensity"]/timint[,"duration"]
# ion=unique(timint[,"ion"])[1]
# #p1=ggplot(data=timint[timint[,"ion"]=="m104.04955..104.05...Conc.",],aes(xmin=xmin,ymax=ymax,ymin=ymin,xmax=xmax,group=ion,fill=ion))+geom_rect()+theme(legend.position="none")
# 
# p2=ggplot(data=timint[timint[,"ion"]==ion,],aes(xmin=xmin,ymax=ymax,ymin=ymin,xmax=xmax))+geom_rect(color="dark blue",fill="light blue")+theme(legend.position="none")+theme_bw()+ggtitle("After breathing correction")
# 
# gh=ptrvListGetHeights(res_intensity$time,discretisation=5)
# ghp=gh[gh[,"product"]==timint[1,"product"]&gh[,"subject"]==timint[1,"subject"]&gh[,"rep"]==timint[1,"rep"]&gh[,"ion"]==ion,]
# ghp[,"xmin"]=as.numeric(as.character(ghp[,"time"]))-as.numeric(as.character(ghp[,"duration"]))/2
# ghp[,"xmax"]=as.numeric(as.character(ghp[,"time"]))+as.numeric(as.character(ghp[,"duration"]))/2
# ghp[,"ymin"]=0
# ghp[,"ymax"]=ghp[,"intensity"]
# p01=ggplot(ghp,aes(x=time,y=intensity))+geom_line()
# p02=ggplot(data=ghp[ghp[,"ion"]==ion,],aes(xmin=xmin,ymax=ymax,ymin=ymin,xmax=xmax))+geom_rect(color="dark blue",fill="light blue")+theme(legend.position="none")+theme_bw()+ggtitle("After homogeneization of times")
# 
# grid.arrange(grobs=list(p2+xlab("time"),p02+xlim(0,100)+ylim(0,0.015)+xlab("time")),nrow=2)
# 
# grid.arrange(grobs=list(p2,p01+xlim(0,100)+ylim(0,0.015)),nrow=2)
# gh2=gh
# gh[gh[,"intensity"]<0,"intensity"]=0
# #ghn=normalizeByEval(gh,ionToUse=NULL)
# #ions=unique(ghn[,"ion"])
# 
# # Getting the ion sum on products
# ion="m115.11308..115.11...Conc."
# ionC="m115.11"
# ionA="m105.07"
# ionB="m74.068"
# gh[,"ion"]=substr(gh[,"ion"],1,7)
# ions=unique(gh[,"ion"])
# ei=dcast(gh,formula=time+product~ions,fun.aggregate=sum,value.var="intensity")
# er=reshape(ei,direction="long",varying=list(ions),times=ions,timevar="ion",v.names="intensity")
# pC=ggplot(er[er[,"ion"]==ionC,],aes(x=time,y=intensity,group=product,color=product))+geom_line()+ggtitle(ionC)+theme_bw()
# pA=ggplot(er[er[,"ion"]==ionA,],aes(x=time,y=intensity,group=product,color=product))+geom_line()+ggtitle(ionA)+theme_bw()
# pB=ggplot(er[er[,"ion"]==ionB,],aes(x=time,y=intensity,group=product,color=product))+geom_line()+ggtitle(ionB)+theme_bw()
# pion=ggplot(er[er[,"ion"]==ion,],aes(x=time,y=intensity,group=product,color=product))+geom_line()+ggtitle(ion)+theme_bw()
# 
# grid.arrange(pA,pB,pC,nrow=3)
# # Getting the tasting
# subsetg=gh[gh[,"product"]=="A"&gh[,"subject"]=="S002"&gh[,"rep"]=="1",]
# ggplot(subsetg,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme_bw()+theme(legend.position="none")
# resultat=dcast(subsetg,time~ion,value.var="intensity",fun.aggregate=sum)
# vecsum=apply(resultat[,ions],1,sum)
# swept=sweep(resultat[,ions],1,vecsum,"/");
# swept[is.na(swept)]=0
# dfswept=cbind(time=resultat[,"time"],swept)
# drsp=reshape(dfswept,direction="long",varying=list(ions),times=ions,v.names="intensity",timevar="ion")
# ggplot(drsp,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme_bw()+theme(legend.position="none")
# 
# 
# 
