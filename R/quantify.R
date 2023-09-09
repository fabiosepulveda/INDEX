#' Quantification
#'
#' @description
#' This function quantifies qualitative variable.
#'
#' @param x A data frame which provides the data set with all the study variables.
#' @param y A numeric vector which indicates the position of the columns of type factor.
#'
#' @return
#' Return a data frame with quantified data.
#'
#' @importFrom Gifi homals
#'
#' @author
#' Eduardo Jiménez: \email{edjimfer@ugr.es}
#'
#' @references
#' Gifi, A. (1990). Nonlinear Multivariate Analysis. New York: Wiley
#'
#' @seealso
#' Gifi
#'
#' @examples
#'
#' data(LondonHouse)
#' LondonHouse$BATH2 = factor(LondonHouse$BATH2)
#' LondonHouse$GARAGE1 = factor(LondonHouse$GARAGE1)
#' LondonHouse$BEDS2 = factor(LondonHouse$BEDS2)
#' qualitative = c(3,4,5)
#' quantify(LondonHouse,qualitative)
#'
#' @export

quantify <- function(x,y){
  if(is.null(y)){
    x <- x
  } else
  {
    matqual <- x[,y]
   fitmatqual <- homals(matqual,ndim = 1)
   clnames <- colnames(matqual)
   matquan <- fitmatqual$scoremat
   x[,colnames(matquan)] <- matquan
  }
  return(x)
}
