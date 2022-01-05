#' @title ptrvQCPlotSummary
#' @param df a dataframe with name,days,products, replicates as first columns. Further columns corresponds to the intensity relatively to ions (one ion by column)
#' @param ions vector containing the names of the column to be considered as ions and to be analysed
#' @return a list of plots (bars, boxplot, points)
#' @importFrom plyr ddply
#' @importFrom stats sd
#' @importFrom utils head
#' @export
ptrvQCPlotSummary=function(df,ions=NULL)
{
  if(is.null(ions)){ions=colnames(df)[-c(1:4)]}
  days=products=value=name=NULL
  data_summary <- function(data, varname, groupnames){

    summary_func <- function(x, col){
      c(mean = mean(x[[col]], na.rm=TRUE),
        sd = sd(x[[col]], na.rm=TRUE),
        coeffVariation=sd(x[[col]], na.rm=TRUE)/mean(x[[col]],na.rm=T))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varname)
    #data_sum <- rename(data_sum, c("mean" = varname))
    return(data_sum)
  }

  p_bar=p_boxplot=p_points=list()
  for(ion in ions)
  {
    df_ion=df[,c("name","days","products","replicates",ion)]
    colnames(df_ion)=c("name","days","products","replicates","value")
    dataSummary=data_summary(df_ion,varname="value" ,groupnames=c("days","products"))
    # dodge <- position_dodge(width=0.8)
    print(head(dataSummary))
    p_bar[[ion]]=ggplot(dataSummary,aes(x=days,y=mean,fill=products))+geom_bar(stat="identity",position="dodge")+geom_errorbar(position="dodge",size=0.25,aes(ymin=mean-sd,ymax=mean+sd))+theme_bw()+ggtitle(paste0(ion,": barplot"))
    p_boxplot[[ion]]=ggplot(df_ion,aes(x=days,y=value,fill=products))+geom_boxplot(position="dodge")+theme_bw()+ggtitle(paste0(ion,": boxplot(by product)"))
    p_points[[ion]]=ggplot(df_ion,aes(x=days,y=value,group=days,name=name))+geom_boxplot(color="light grey")+geom_jitter(width=0.25,aes(color=products),shape=17)+theme_bw()+ggtitle(paste0(ion,": boxplot"))
  }
  return(list(bars=p_bar,boxplot=p_boxplot,points=p_points))
}