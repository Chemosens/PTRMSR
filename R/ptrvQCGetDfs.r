#' @title ptrvQCGetDfs
#' @description returns and saves of 3 data frames containing: area under curves (auc), maximal intensity reached (max), time when the maximal intensity is reached (tmax)
#' @param repo a repository (with / instead of \) where all the .txt are saved
#' @param ncharDays a vector of two numbers representing the positions of first (and last) characters corresponding to the day in the file name
#' @param ncharProd a vector of two numbers representing the positions of first (and last) characters  corresponding to the product in the file name
#' @param ncharRep one number representing the position of the character corresponding to the replicate in the file name
#' @return a list of 3 data frames containing: area under curves (auc), maximal intensity reached (max), time when the maximal intensity is reached (tmax)
#' @export
ptrvQCGetDfs=function(repo,ncharDays=c(9,10),ncharProd=c(12,15),ncharRep=17)
{
  setwd(repo)
  listFiles=list.files(pattern=".txt")
  days=substr(listFiles,ncharDays[1],ncharDays[2])
  products=substr(listFiles,ncharProd[1],ncharProd[2])
  replicates=substr(listFiles,ncharRep,ncharRep)
  dataset1=read.table(listFiles[1],sep="\t",header=TRUE)
  ions=colnames(dataset1)[-c(1:3)]
  matAUC=matMax=matTmax=matrix(NA,length(listFiles),length(ions))
  colnames(matAUC)=colnames(matMax)=colnames(matTmax)=ions
  rownames(matAUC)=rownames(matMax)=rownames(matTmax)=listFiles
  for(i in 1:length(listFiles))
  {
    dataset=read.table(listFiles[i],sep="\t",header=TRUE)
    res_intensity=ptrvIntensityByTime(dataset,ions=NULL
                                      ,referenceBreath=NULL,
                                      correction = "none",
                                      removeNoise=FALSE,breathRatio =FALSE,timeBlank=c(0,25),
                                      halfWindowSize=12,method="SuperSmoother",total=FALSE,
                                      minimalDuration=2,minExpi=NULL,maxInspi=NULL,
                                      forMinExpiDivideMaxIntBy=4,forMaxInspiDivideMaxIntBy=5)
    stats=ptrvIntensity(res_intensity$res)
    stats2=stats[,-1]
    if(any(!stats[,"ion"]%in%ions)){stop("Not the same ion names in all files")}
    rownames(stats2)=stats[,"ion"]
    matAUC[i,]=stats2[ions,"area"]
    matMax[i,]=stats2[ions,"max"]
    matTmax[i,]=stats2[ions,"tmax"]
  }

  df_auc=data.frame(name=rownames(matAUC),days=days,products=products,replicates=replicates,matAUC)
  df_max=data.frame(name=rownames(matMax),days=days,products=products,replicates=replicates,matMax)
  df_tmax=data.frame(name=rownames(matTmax),days=days,products=products,replicates=replicates,matTmax)


  write.table(df_auc,row.names=FALSE,sep=";",file="res_auc.csv")
  write.table(df_tmax,row.names=FALSE,sep=";",file="res_tmax.csv")
  write.table(df_max,row.names=FALSE,sep=";",file="res_max.csv")
  return(dfs=list(auc=df_auc,max=df_max,tmax=df_tmax))
}