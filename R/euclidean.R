#' @title
#' Euclidean Algorithm
#' @description A short description...
#'
#' The Euclidean algorithm finds the greatest common divisor, GCD between to whole numbers, a and b.
#'
#' @param a a whole number of length 1
#' @param b a whole number of length 1
#'
#' @returns The GCD
#' @export
#'
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#'
#' @source https://en.wikipedia.org/wiki/Euclidean_algorithm
#'
euclidean <- function(a, b){
  if( !is.numeric(a) | !is.numeric(b) ){stop("Only input numeric values")}
  if( length(a) != 1 ){stop("a has a length greater than 1")}
  if( length(b) != 1 ){stop("b has a length greater than 1")}
  if( (a %% 1 > 0) ){stop("a is not an integer")}
  if( (b %% 1 > 0) ){stop("b is not an integer")}



  repeat{ # repeat loop since it works best here
    if(a %% b == 0){ # if it can be divided, then stop
      break
    }
    r <- a %% b # remainder
    a <- b
    b <- r
  }
  return(b)
}
