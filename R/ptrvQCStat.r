#' @title ptrvQCStats
#' @description returns and saves a list of dataframes containing the average (avg), standard deviations (std) and coefficients of variation (cv)
#' @param df a dataframe with name,days,products, replicates as first columns. Further columns corresponds to the intensity relatively to ions (one ion by column)
#' @param ions vector containing the names of the column to be considered as ions and to be analysed
#' @return a list of dataframes: avg for the average, std for standard deviations and cv for the coefficient of variations
#' @export
#' @importFrom plyr ddply

#' @importFrom stats kruskal.test
ptrvQCStats=function(df,ions=NULL)
{
  data_summary <- function(data, varname, groupnames){
  
    summary_func <- function(x, col){
      c(mean = mean(x[[col]], na.rm=TRUE),
        sd = sd(x[[col]], na.rm=TRUE),
        coeffVariation=100*sd(x[[col]], na.rm=TRUE)/mean(x[[col]],na.rm=T))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varname)
    #data_sum <- rename(data_sum, c("mean" = varname))
    return(data_sum)
  }
  
  if(is.null(ions)){ions=colnames(df)[-c(1:4)]}
  
  for(ion in ions)
  {
    dfsum=data_summary(df[,c("days","products",ion)],varname=ion,groupnames=c("days","products"))
    dfmoy=data_summary(df[,c("days","products",ion)],varname=ion,groupnames=c("days","products"))
    if(ion==ions[1])
    {
      dfMean=dfsum[1:3]
      dfSd=dfsum[,c(1:2,4)]
      dfCv=dfsum[,c(1:2,5)]
    }
    if(ion!=ions[1])
    {
      if(all(dfsum[,1:2]==dfMean[,1:2]))
      {
        dfMean=cbind(dfMean,dfsum[,3])
        dfSd=cbind(dfSd,dfsum[,4])
        dfCv=cbind(dfCv,dfsum[,5])
      }
      else
      {
        stop("one modality is missing ")
      }
    }
  }
  colnames(dfMean)=colnames(dfSd)=colnames(dfCv)=c("days","products",ions)
  write.table(dfMean,row.names=FALSE,sep=";",file="res_average.csv")
  write.table(dfSd,row.names=FALSE,sep=";",file="res_std.csv")
  write.table(dfCv,row.names=FALSE,sep=";",file="res_cv.csv")
  return(list(avg=dfMean,std=dfSd,cv=dfCv))
}