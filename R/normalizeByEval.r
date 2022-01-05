#'normalizeByEval
#'Divides each evaluation by the sum of its variables
#'@param df data frame containing the column "file","product","subject","rep","ion","intensity" (optionally "file" and "time")
#'@param ionToUse if NULL, all the ions are use to normalize, if a vector of names of ions, it is used to normalize
#'@export
#'@importFrom graphics lines
normalizeByEval=function(df,ionToUse=NULL)
{
  s=NULL
if(is.null(ionToUse)){ions=unique(df[,"ion"])}
  else{
    ions=ionToUse
    df=df[df[,"ion"]%in%ionToUse,]
    }

if("file"%in%colnames(df))
{
  widedf=dcast(df[,c("file","product","subject","rep","ion","intensity")],file+product+subject+rep~ion,value.var="intensity",fun.aggregate=sum)
}
else
{
  if(!"time"%in%colnames(s))
  {
    widedf=dcast(df[,c("product","subject","rep","ion","intensity")],product+subject+rep~ion,value.var="intensity",fun.aggregate=sum)
  }
  if("time"%in%colnames(df))
  {
    widedf=dcast(df[,c("product","subject","rep","time","ion","intensity")],product+subject+rep~ion+time,value.var="intensity",fun.aggregate=sum)
  }
}
newWide=widedf
#print(summary(widedf))
ions=as.character(ions)
sumdf=apply(widedf[,ions],1,sum)
newWide[,ions]=sweep(widedf[,ions],1,apply(widedf[,ions],1,sum),"/")
newdf=reshape(newWide,direction="long",v.names="intensity",timevar="ion",varying=list(ions),times=ions)

res=list(df=newdf,sum=sumdf)
return(res)
}

#newIntens=normalizeByEval(res_intensity$totalIntensity)
