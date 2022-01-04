#'  @param intens data frame containing the column "file","product","subject","rep","ion","intensity" (optionally "file" and "time")
#'  @param ionToUse if NULL, all the ions are use to normalize, if a vector of names of ions, it is used to normalize
#'  @export
normalizeByEval=function(intens,ionToUse=NULL)
{
if(is.null(ionToUse)){ions=unique(intens[,"ion"])}
  else{
    ions=ionToUse
    intens=intens[intens[,"ion"]%in%ionToUse,]
    }

if("file"%in%colnames(intens))
{
  wideIntens=dcast(intens[,c("file","product","subject","rep","ion","intensity")],file+product+subject+rep~ion,value.var="intensity",fun.aggregate=sum)
}
else
{ print("in")
  if(!"time"%in%colnames(intens))
  {
    wideIntens=dcast(intens[,c("product","subject","rep","ion","intensity")],product+subject+rep~ion,value.var="intensity",fun.aggregate=sum)
  }
  if("time"%in%colnames(intens))
  {
    wideIntens=dcast(intens[,c("product","subject","rep","time","ion","intensity")],product+subject+rep~ion+time,value.var="intensity",fun.aggregate=sum)
  }
}
newWide=wideIntens
#print(summary(wideIntens))
ions=as.character(ions)
sumIntens=apply(wideIntens[,ions],1,sum)
newWide[,ions]=sweep(wideIntens[,ions],1,apply(wideIntens[,ions],1,sum),"/")
newIntens=reshape(newWide,direction="long",v.names="intensity",timevar="ion",varying=list(ions),times=ions)

res=list(intens=newIntens,sum=sumIntens)
return(res)
}

#newIntens=normalizeByEval(res_intensity$totalIntensity)
