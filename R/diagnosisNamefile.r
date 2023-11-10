#' @param listFiles the list of files to be read
#' @param sep separator of the files
#' @export
diagnosis=function(listFiles,sep=sep)
{
  comparisonOfColumns=function(colnamesfiles)
  {
    ncolfile=sapply(colnamesfiles,length)
    if(sd(ncolfile)==0)
    {
      matrixOfColnames=t(Reduce(cbind,colnamesfiles));rownames(matrixOfColnames)=names(colnamesfiles);
      nbColnamesDiff=apply(matrixOfColnames,2,function(x){length(unique(x))})
      if(sum(nbColnamesDiff>1)!=0)
      {
        print("Different colnames for a given ion")
        print(matrixOfColnames[,nbColnamesDiff>1])
      }
    }  else{matrixOfColnames=NA}
    return(matrixOfColnames)
  }

  ncolfile=rep(NA,length(listFiles));names(ncolfile)=listFiles
  colnamesfiles=list()
  for(file in listFiles)
  {
    datafile=read.table(file,sep=sep,header=TRUE)
    ncolfile[file]=ncol(datafile)
    colnamesfiles[[file]]=colnames(datafile)
  }
  # Checking the columns
  if(sd(ncolfile)!=0)
  {
    print("Not the same number of columns ! (see below) ")
    print(ncolfile)
    problematicFiles=names(ncolfile)[ncolfile!=median(ncolfile)]
    usualFiles=names(ncolfile)[ncolfile==median(ncolfile)]
    usualMatrixOfColnames=comparisonOfColumns(colnamesfiles[usualFiles])

    for(pf in problematicFiles)
    {
      print(pf)
      if(!is.null(dim(usualMatrixOfColnames)))
      {
        addCol=colnamesfiles[[pf]][!colnamesfiles[[pf]]%in%usualMatrixOfColnames[1,]]
        remCol=usualMatrixOfColnames[1,][!usualMatrixOfColnames[1,]%in%colnamesfiles[[pf]]]
        if(length(addCol)>0){ print(paste0("additional column:",addCol))}
        if(length(remCol)>0){ print(paste0("removed column:",remCol))}
      }

    }
  }
  else{print("Everything ok :) congrat's")}

  matrixOfColnames=comparisonOfColumns(colnamesfiles)

  return(list(ncol=ncolfile,colnames=matrixOfColnames))
}