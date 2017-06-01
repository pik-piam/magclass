#' Copy Attributes
#' 
#' This function copies attributes from one object and assigns them to another.
#' 
#' 
#' @aliases copy.attributes copy.attributes<-
#' @param from object from which the attributes should be taken
#' @param value Same as "from" (object from which the attributes should be
#' taken)
#' @param to object to which the attributes should be written
#' @param delete attributes which should not be copied. By default this are
#' class specific attributes which might cause problems if copied to another
#' object. But you can add or remove attributes from the vector.
#' @param delete2 Identical to delete and just added for convenience for the
#' case that you want to delete additional attributes but do not want to repeat
#' the vector given in delete. In the function both vectors, delete and
#' delete2, are just merged to one deletion vector.
#' @author Jan Philipp Dietrich
#' @examples
#' 
#' from <- array(12)
#' attr(from,"blablub") <- "I am an attribute!"
#' attr(from,"blablub2") <- "I am another attribute!"
#' 
#' print(attributes(from))
#' 
#' to <- as.magpie(0)
#' print(attributes(to))
#' 
#' copy.attributes(to) <- from
#' print(attributes(to))
#' 
#' @export
copy.attributes <- function(from,to,delete=c('names','row.names','class','dim','dimnames'),delete2=NULL) {
  a <- attributes(from)
  a[c(delete,delete2)] <- NULL
  attributes(to) <- c(attributes(to),a)
  return(to)
}

#' @describeIn copy.attributes assign attributes from object "value"
#' @export
"copy.attributes<-" <- function(to,delete=c('names','row.names','class','dim','dimnames'),delete2=NULL,value) {
  return(copy.attributes(from=value,to=to,delete=delete,delete2=delete2))
}
