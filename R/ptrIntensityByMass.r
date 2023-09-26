#'@title ptrIntensityByMass
#'@param ptr ptr object stemming from ptrRead
#'@param type file .h5 to read
#'@param integrationTable integrationTable containing columns: "mz","inf" and "sup"
#'@param breaks breaks to be used
#'@param rt vector of two numbers (min time and max time). If NULL (default), no time limits.
#'@param by step of time for integration of the chromatogram. Default 0.1
#'@export
#'@examples
#'data(ptr)
#'#ptrIntensityByMass(ptr)
ptrIntensityByMass=function(ptr,type="exactMass",rt=NULL,integrationTable=NULL,breaks=NULL,by=0.1)
{
  if(type=="integration")
  {
    MassAxis=ptr$MassAxis
   tofData=ptr$TofData
    time=ptr$Time
    if(is.null(integrationTable))
    {
      if(is.null(breaks))
      {
        sequence=seq(min(ptr$MassAxis),max(ptr$MassAxis),by=by)
        sequence=c(sequence,max(ptr$MassAxis))
      }
      upperLimit=sequence[2:length(sequence)]
      lowerLimit=sequence[1:(length(sequence)-1)]
      label=(upperLimit+lowerLimit)/2
      integrationTable=data.frame("name"=as.factor(as.character(label)),"mz"=label,"inf"=lowerLimit,"sup"=upperLimit)
    }
    if(is.character(integrationTable))
    {
      integrationTable=ptr$PeakTable
      colnames(integrationTable)=c("name","mz","inf","sup")
    }
    resdf=NULL
    for(label in integrationTable[,"name"])
    {
      print(label)
      masses_label=MassAxis[
        MassAxis>=integrationTable[integrationTable[,"name"]==label,"inf"]
        &MassAxis<=integrationTable[integrationTable[,"name"]==label,"sup"]
      ]
      tofSubset=tofData[which(MassAxis%in%masses_label),,]

      if(is.null(rt))
      {
        sumSubset=sum(apply(tofSubset,2:3,function(x)
        {return(sum(x))}))
      }
      if(!is.null(rt))
      {
        ndim=dim(tofSubset)[2:3]
     #   print(ndim)
        if(!is.na(ndim[2]))
        {
          mtime=matrix(as.numeric(time),nrow=ndim[1],ncol=ndim[2])
          indtime=mtime<rt[2]&mtime>rt[1]
          sumSubset=sum(apply(tofSubset,1,function(x)
          {   return(sum(x[indtime]))}))
          sumNoSubset=sum(apply(tofSubset,1,function(x)
          {   return(sum(x))}))
        }
        else{sumSubset=NA}


      }


      line=c(label,integrationTable[integrationTable[,"name"]==label,"mz"],sumSubset)
      resdf=rbind(resdf,line)
    }

    colnames(resdf)=c("name","mz","intensity")
    rownames(resdf)=NULL
  }
  if(type=="exactMass")
  {

    selectTimes=function(i,tofData=tofData,rt=rt)
    {
      tofSubset=tofData[i,,]
      if(is.null(rt))
      {
        sumSubset=sum(apply(tofSubset,1:2,function(x)
        {return(sum(x))}))
        return(sumSubset)
      }
      if(!is.null(rt))
      {  ndim=dim(tofSubset)[1:2]
        mtime=matrix(as.numeric(ptr$Time),nrow=ndim[1],ncol=ndim[2])
        indtime=mtime<rt[2]&mtime>rt[1]
        sumSubset=sum(tofSubset[indtime])
       # sumNoSubset=sum(apply(tofSubset,1,function(x)
        #{   return(sum(x))}))
        return(sumSubset)
      }
    }

    # For loop
    t0=Sys.time()
    #if(loop=="for")
    #{

      res=rep(NA,dim(tofData)[1])
      for(i in 1:dim(tofData)[1])
      {
        res[i]=selectTimes(i,tofData,rt)
      }
      resdf=data.frame(mz=ptr$MassAxis,intensity=res)
    #}

    Sys.time()-t0
    # lapply
    # parallelisation
    #, FUN = function(i){return()})
    #   cl = parallel:::makeCluster(7)
    #   clusterExport(cl, "tofData")
    #   Wboot_list = pblapply(seq(dim(tofData)[1]),
    #                         FUN=function(i){return(selectTimes(i,tofData=tofData,rt=c(1,30)))},
    #                         cl = cl)
    #   W = Reduce("cbind", lapply(Wboot_list, function(x) Reduce("c", x)))
    #   parallel:::stopCluster(cl)
      #

    }

  if(type=="sumSpectrum")
  {
    if(!is.null(rt))
    {
      warning("rt option is available only for type !='sumSpectrum'")
    }
    resdf=NULL
    if(!is.null(integrationTable))
    {
      if(is.character(integrationTable))
      {
        print("The integration table of the PTR device is used")
        integrationTable=ptr$PeakTable
        colnames(integrationTable)=c("name","mz","inf","sup")
      }
      MassAxis=ptr$MassAxis
      for(label in integrationTable[,"name"])
      {

        massesindex=which(MassAxis>=integrationTable[integrationTable[,"name"]==label,"inf"]
          &MassAxis<=integrationTable[integrationTable[,"name"]==label,"sup"])
        intens=sum(ptr$SumSpectrum[massesindex])
        mzi=mean(MassAxis[massesindex])

        line=c(mzi,intens,label)
        resdf=rbind(resdf,line)
        colnames(resdf)=c("mz","intensity","name")
      }
    }
    else
    {
      resdf=data.frame(ptr$MassAxis,ptr$SumSpectrum)
      colnames(resdf)=c("mz","intensity")
    }

  }
  resdf=as.data.frame(resdf)
 # print(summary(resdf))
  resdf[,"intensity"]=as.numeric(resdf[,"intensity"])
  resdf[,"mz"]=as.numeric(resdf[,"mz"])
  res=list(df=resdf)
  class(res)="ibm"
  return(res)
}
