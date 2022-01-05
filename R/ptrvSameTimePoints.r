#'ptrvSameTimePoints
#'returns a dataframe with the same timepoints
#'@param df a data frame whose colnames are time, product, subject,rep, duration and intensity
#'@param npoints number of timepoints to be obtained
#'@param meanBySubject boolean indicating whether the mean by subject should be computed
#'@param meanByRep boolean indicating whether the mean by replicate should be computed
#'@param normalizeByTime boolean indicating whether the data should be normalized (T) or not
#'@param breakTimes indicating the breaking time points. Default to seq(0,max(df$time),length.out=npoints)
#'@export
ptrvSameTimePoints=function(df,npoints=100,meanBySubject=TRUE,meanByRep=FALSE,normalizeByTime=FALSE,breakTimes=NULL)
{
  if(is.null(breakTimes))
  {
    breakTimes= seq(0,max(as.numeric(df[,"time"]),na.rm=T),length.out=npoints)
  }
  df[,"time_all"]=cut(df[,"time"],breaks=breakTimes,labels=breakTimes[-1])
  df[,"height"]=df[,"intensity"]/df[,"duration"]
  df[df[,"height"]<0,"height"]=0
  #dfToFill=expand.grid(rep=unique(df$rep), product=unique(df$product),  subject=unique(df$subject), ion=unique(df$ion),time=breakTimes)

  # if(nrow(df)<nrow(dfToFill))
  # {
  #   for(i in 1:nrow(df))
  #   {
  #     replicate=df[i,"rep"]
  #     ion=df[i,"ion"]
  #     product=df[i,"product"]
  #     subject=df[i,"subject"]
  #     time1=df[i,"time"]
  #     time2=df[i,"time"]+df[i,"duration"]
  #     height=df[i,"height"]
  #     relevantLines=dfToFill[,"subject"]==subject&dfToFill[,"product"]==product&product& dfToFill[,"ion"]==ion&dfToFill[,"rep"]==replicate&dfToFill[,"time"]<=time2&dfTofill[,"time"]>time1
  #     dfToFill[relevantLines,"height"]=height
  #   }
  # }
  # else{
  #
  #
  # }
  #
  #


  if(meanBySubject)
  {
    if(meanByRep)
    {
      df3=dcast(df,formula=time_all+product+ion~., value.var="height",fun.aggregate=mean,drop=FALSE)
      colnames(df3)=c("time","product","ion","intensity")
    }
    else
    {
      df3=dcast(df,formula=time_all+product+rep+ion~., value.var="height",fun.aggregate=mean,drop=FALSE)
      colnames(df3)=c("time","product","rep","ion","intensity")
    }
  }
  else
  {
    df3=dcast(df,formula=time_all+product+subject+rep+ion~., value.var="height",fun.aggregate=function(x){return(mean(x,na.rm=T))},drop=FALSE)
    colnames(df3)=c("time","product","subject","rep","ion","intensity")

    #df3=df[,c("time_all","product","subject","rep","ion","height")]

  }
  df3[,"time"]=as.numeric(as.character(df3[,"time"]))
  df3[,"ion"]=as.factor(df3[,"ion"])
  df3[,"product"]=as.factor(df3[,"product"])

  if(normalizeByTime) # Calcul du temps global et normalisation
  {
    ions=as.character(unique(df3[,"ion"]))
    if(meanBySubject)
    {
      if(meanByRep)
      {
        df4=dcast(df3,formula=time+product~ion,value.var="intensity",fun.aggregate=sum)
        colnames(df4)[1:2]=c("time","product")
      }
      else
      {
        df4=dcast(df3,formula=time+product+rep~ion,value.var="intensity",fun.aggregate=sum)
        colnames(df4)[1:3]=c("time","product","rep")
      }

    }
    else
    {
      if(!meanByRep)
      {
        df4=dcast(df3,formula=time+product+subject+rep~ion,value.var="intensity",fun.aggregate=sum)
        colnames(df4)[1:4]=c("time","product","subject","rep")
      }
      else
      {
        df4=dcast(df3,formula=time+product+subject~ion,value.var="intensity",fun.aggregate=sum)
        colnames(df4)[1:3]=c("time","product","subject")
      }

    }
    df5=df4
    df5[,ions]=sweep(as.matrix(df4[,ions]),2,apply(df4[,ions],2,mean),"-")
    df5[,ions]=sweep(as.matrix(df5[,ions]),2,apply(df5[,ions],2,sd),"/")
    df6=reshape(df5,varying=list(ions),times=ions,timevar="ion",v.names="intensity",direction="long")
    return(df6)
  }
  else
  {
    return(df3)
  }

}

#ion=c("ion1","ion2","ion1")
#subject=rep("S01",3)
#product=rep("P01",3)
#time=c(1,2,7)
#duration=c(1,5,1)
#intensity=c(1,10,4)
#df=data.frame(subject=subject,product=product,rep=1,ion=ion,time=time,duration=duration,intensity=intensity)
#df[,"rep"]=1
#resdf=ptrvSameTimePoints(df,npoints=100,meanBySubject=FALSE,meanByRep=FALSE,normalizeByTime=FALSE,breakTimes=NULL,type="ptr")
