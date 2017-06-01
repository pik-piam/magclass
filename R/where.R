#' where
#' 
#' Analysis function for magpie objects
#' 
#' 
#' @param x A logical statement with a magpie object
#' @param plot depreciated. Use the function whereplot in package luplot.
#' @return A list of analysis parameters
#' @author Benjamin Leon Bodirsky
#' @seealso whereplot in package luplot
#' @examples
#' 
#' data(population_magpie)
#'  test<-population_magpie
#'  dimnames(test)[[1]]<-c("AFG","DEU","FRA","EGY","IND","IDN","RUS","CHN","USA","YEM")
#'  where(test>500)
#' 
#' @export where
where<-function(x, plot=NULL){
  if(!is.null(plot)){warning("Argument plot is depreciated. Use whereplot() for plot=T and where() for plot=F.")}
  
  if(is.null(getNames(x)[[1]])) {getNames(x)<-"dummy"}
  
  analysis<-list()
  
  tmp<-which(x==TRUE,arr.ind = TRUE)
  tmp[,1]<-getRegions(x)[as.numeric(tmp[,1])]
  tmp[,2]<-getYears(x)[as.numeric(tmp[,2])]
  tmp[,3]<-getNames(x)[as.numeric(tmp[,3])]  
  analysis$true$individual<-tmp
  
  tmp<-which(x==FALSE,arr.ind = TRUE)
  tmp[,1]<-getRegions(x)[as.numeric(tmp[,1])]
  tmp[,2]<-getYears(x)[as.numeric(tmp[,2])]
  tmp[,3]<-getNames(x)[as.numeric(tmp[,3])]  
  analysis$false$individual<-tmp
  
  tmp<-which(is.na(x),arr.ind = TRUE)
  tmp[,1]<-getRegions(x)[as.numeric(tmp[,1])]
  tmp[,2]<-getYears(x)[as.numeric(tmp[,2])]
  tmp[,3]<-getNames(x)[as.numeric(tmp[,3])]  
  analysis$na$individual<-tmp
  
  tmp<-as.magpie(apply(x,MARGIN=1,FUN=function(x){any(x==FALSE,na.rm=TRUE)})*1)
  analysis$false$regions<-dimnames(tmp)[[1]][tmp==1]
  
  tmp<-as.magpie(apply(x,MARGIN=2,FUN=function(x){any(x==FALSE,na.rm=TRUE)})*1)
  analysis$false$years<-getYears(tmp)[tmp==1]
  
  tmp<-as.magpie(apply(x,MARGIN=1,FUN=function(x){any(is.na(x),na.rm=TRUE)})*1)
  analysis$na$regions<-dimnames(tmp)[[1]][tmp==1]
  
  tmp<-as.magpie(apply(x,MARGIN=2,FUN=function(x){any(is.na(x),na.rm=TRUE)})*1)
  analysis$na$years<-getYears(tmp)[tmp==1]
  
  tmp<-as.magpie(apply(x,MARGIN=1,FUN=function(x){any(x==TRUE,na.rm=TRUE)})*1)
  analysis$true$regions<-dimnames(tmp)[[1]][tmp==1]
  
  tmp<-as.magpie(apply(x,MARGIN=2,FUN=function(x){any(x==TRUE,na.rm=TRUE)})*1)
  analysis$true$years<-getYears(tmp)[tmp==1]
  
  # check how many T, F, NAs and NANs-

  tmp=c(sum((x==TRUE)*1,na.rm = TRUE),sum((x==FALSE)*1,na.rm = TRUE),sum(is.na(x)*1,na.rm = TRUE))
  other=length(x)-sum(tmp)
  if(other!=0){warning("function is made to analyse logical statements. Your values contain values that are not 1/0/NA")}
  tmp=c(tmp,other)
  names(tmp)=c("TRUE","FALSE","NA","other")
  analysis$summary<-tmp


  return(analysis)
}
