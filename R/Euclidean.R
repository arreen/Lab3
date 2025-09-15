euclidean <- function(a, b){
  if(a){stop("Not a numeric value")} # Error message for a
  if(b){stop("Not a numeric value ")} # Error message for b
  repeat{ # repeat loop since it works best here
    if(a %% b == 0){ # if it can be divided, then stop
      break
    }
    r <- a %% b # Some smart ass comment about why this is the correct lines
    a <- b
    b <- r
  }
  return(b)
}