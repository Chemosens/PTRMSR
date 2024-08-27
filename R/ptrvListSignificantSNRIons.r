#' @title ptrvListSignificantSNRIons
#' @description This function calculates the ratio (max intensity for signal)/(max intensity for noise) after breathing correction and returns an ion as significant if this ratio is higher than 3.
#' @param listFiles list of files
#' @param metaData metaData
# #' @param metaData a metaData file
#  #' @param correction "none" or "cycle"
#' @param dec_vec vector with the same size as listFiles containing the decimal separator used in the files
#' @return A list containing: resIons: the ration Signal/Noise for all ions; intersection: the ions significant in all files; union: the ions significant in at least one file.
#' @inheritParams ptrvSignificantSNRIons
#' @export
#' @examples
#' # DONOTRUN
#' #sigIons=ptrvListSignificantSNRIons(listFiles=c("file.txt","file2.txt"),
#' # referenceBreath="isoprene",noisePeriod=c(0,25))
#' @importFrom stats wilcox.test
ptrvListSignificantSNRIons=function(listFiles,metaData,dec_vec=rep(".",length(listFiles)),multiplyNoiseBy=3,noisePeriod=c(0,25),removeNoise=TRUE,minimalDuration=2,halfWindowSize=5,smoothMethod="MovingAverage",minExpi=NULL,maxInspi=NULL,forMinExpiDivideMaxIntBy=4,forMaxInspiDivideMaxIntBy=5)
{
  dataset=read.table(listFiles[1],sep="\t",header=T,dec=",")
  ions=colnames(dataset)[-c(1:3)]
  resIons=list()
  product=rep(NA,length(listFiles))
  for(i in 1:length(listFiles))
  {
    print(listFiles[i])
    dataset=read.table(listFiles[i],sep="\t",dec=dec_vec[i],header=T)
    resIons[[i]]=ptrvSignificantSNRIons(dataset,referenceBreath=metaData[metaData[,"file"]==listFiles[i],"breathing"],noisePeriod=noisePeriod,correction="cycle",multiplyNoiseBy = multiplyNoiseBy,removeNoise=removeNoise,halfWindowSize=halfWindowSize,smoothMethod=smoothMethod,minimalDuration=minimalDuration,minExpi=minExpi,maxInspi=maxInspi,forMinExpiDivideMaxIntBy=forMinExpiDivideMaxIntBy,forMaxInspiDivideMaxIntBy=forMaxInspiDivideMaxIntBy)$snRatio
    product[i]=metaData[metaData[,"file"]==listFiles[i],"product"]
  }
  resSig=lapply(resIons,function(x){return(names(x[x>multiplyNoiseBy]))})
  ionSigAll=Reduce(intersect,resSig)
  ionSigUnique=Reduce(union,resSig)
  return(list(resIons=resIons,intersection=ionSigAll,union=ionSigUnique,product=product))
}