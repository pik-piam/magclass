# Create lookup-table for efficient conversion of years to POSIX
# 
# Create lookup-table for efficient conversion of years to POSIX
# 
# 
# @param start start year
# @param end end year
# @param by by
# @return POSIX format as used in quitte
# @author Michaja Pehl, Anselm Schultes
# @examples
# 
# #       ISOyear <- make.ISOyear()
# #       ISOyear(2016)
# 
# Made an internal function since it is implemented in the quitte package and
# used only in context with quitte objects.
make.ISOyear <- function(start = 1900, end = 2200, by = 5) {
  ISOyear <- data.frame(key = seq(start, end, by),
                        value = seq.POSIXt(ISOdate(start, 7, 2),
                                           ISOdate(end, 7, 2),
                                           paste(by, "years")))
  f <- function(keys) {
    
    rows <- match(keys, ISOyear$key)
    if (any(is.na(rows))) {
      new.keys <- keys[is.na(rows)]
      ISOyear <<- rbind(ISOyear, data.frame(key = new.keys, 
                                            value = ISOdate(new.keys, 7, 2)))
      rows <- match(keys, ISOyear$key)
    }
    
    return(ISOyear[rows,"value"])
  }
  
  return(f)
}
