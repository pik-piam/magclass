#' isYear
#' 
#' Function to find out whether a vector consists of strings in the format
#' "yXXXX" or "XXXX" with X being a number
#' 
#' 
#' @param x A vector
#' @param with_y indicates which dataformat years have to have (4-digit without
#' y (e.g.1984) or 5digit including y (y1984))
#' @return Returns a vector of the length of x with TRUE and FALSE
#' @author Benjamin Bodirsky
#' @examples
#' 
#' x<-c("1955","y1853","12a4")
#' isYear(x, with_y=TRUE)
#' isYear(x, with_y=FALSE)
#' 
#' @export
isYear<-function(x, with_y=TRUE)
{
  if(!is.vector(x)){stop("Year Object is no Vector")}
  return_vector<-rep(TRUE,length(x))
  for (i in 1:length(x))
    {
      if (with_y==FALSE)
        {
          if (nchar(x[i])!=4)                 {return_vector[i]=FALSE}
          if (grepl("[0-9]{4}",x[i])==FALSE)  {return_vector[i]=FALSE}
        } else
        { 
          if (nchar(x[i])!=5)                 {return_vector[i]=FALSE}
          if (grepl("y[0-9]{4}",x[i])==FALSE) {return_vector[i]=FALSE}
        }      
    }
  return(return_vector)
}
