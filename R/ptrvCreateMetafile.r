#' @title ptrvCreateMetaFile
#' @importFrom utils write.table
#' @param wd Working directory to use for the creation of the metafile
#' @param subject vector of length 2 containing the position of character corresponding to the subject in the file
#' @param product  vector of length 2 containing the position of character corresponding to the subject in the file
#' @param replicate  vector of length 2 containing the position of character corresponding to the subject in the file
#' @param sep vector of separators for reading the files =";",
#' @export
ptrvCreateMetaFile=function(wd,subject=NULL,product=NULL,replicate=NULL,sep=";")
{
  setwd(wd)
  listFiles=list.files(pattern=".txt")
  df=data.frame(file=listFiles,prod=NA,suj=NA,rep=NA,resp=NA,miseEnBouche_s=NA,miseEnBouche_scan=NA,plusRienEnBouche_s=NA,plusRienEnBouche_scan=NA,finTimeSens_scan=NA,finTimeSens_s=NA,finPTR_scan=NA,finPTR_s=NA)
  if(!is.null(subject))
  {
    df[,"suj"]=substr(df[,"file"],subject[1],subject[2])
  }
  if(!is.null(product))
  {
    df[,"prod"]=substr(df[,"file"],product[1],product[2])
  }
  if(!is.null(replicate))
  {
    df[,"rep"]=substr(df[,"file"],replicate[1],replicate[2])
  }
  
  write.table(df,file="metaData.csv",sep=";",row.names=FALSE)
}