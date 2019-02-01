#' 2-D plane Linear Interpolation
#' A linear interpolation function using point slope formula to return a single point.
#'
#' @param y0 Initial point on the y-axis on a 2-D plane
#' @param y1 End point on the y-axis on a 2-D plane
#' @param x0 Initial point on the x-axis on a 2-D plane
#' @param x1 End point on the y-axis on a 2-D plane
#' @param x Known point on the x-axis on a 2-D plane
#'
#' @return y Missing point on the y-axis on a 2-D plane
#' @export
#'
#' @examples
#' LinInterpolate(3,5,1,3,2)
LinInterpolate <- function(y0,y1,x0,x1,x){

  y <- y0 + (x-x0) * ((y1-y0)/(x1-x0))

  return(y)
} # lin.interpolate
