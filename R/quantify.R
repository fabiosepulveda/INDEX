#' Quantification
#'
#' @description
#' This function quantifies qualitative variables.
#'
#' @param x A data frame which provides the data set with all the study variables.
#' @param y A numeric vector which indicates the position of the columns of type factor.
#'
#' @return
#' Return a data frame with quantified data.
#'
#' @author
#' Fabio Sepulveda:  \email{fhsepulveda@udemedellin.edu.co}
#'
#' @references
#' Gifi, A. (1990). Nonlinear Multivariate Analysis. New York: Wiley
#'
#' @seealso
#' Fabio
#'
#' @export

quantify <-function(x,y){
  if(is.null(y)){
    x <- x
  } else
  {matqual <- x[,y]
  fitmatqual <- homals(matqual,ndim = 1)
  clnames <- colnames(matqual)
  matquan <- fitmatqual$scoremat
  x[,colnames(matquan)]<- matquan}
  return(x)
}
