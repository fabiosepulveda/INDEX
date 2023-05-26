#' Normalization
#'
#' @description
#' This function computes the normalized matrix.
#'
#' @param x A numeric matrix or data frame which provides the data set with all the study variables.
#' @param polarity A numeric vector which indicates the relation (polarity) among the latent phenomenon and all the study variables.By default is null, if the relation is negative.
#'
#' @return
#' Return the normalizex matrix.
#'
#'
#' @author
#' Fabio Sepulveda:  \email{fhsepulveda@udemedellin.edu.co}
#'
#' @references
#' Jiménez-Fernández, E., Sánchez, A., & Ortega-Pérez, M. (2022). Dealing with weighting scheme in composite indicators: an unsupervised distance-machine learning proposal for quantitative data. Socio-Economic Planning Sciences, 83, 101339.
#'
#' @seealso
#' Fabio
#'
#' @examples
#'
#' data(NUTS2)
#' A <- NUTS2[,4:11]
#' polarity <- c(5,6,7,8)
#' matrix_normalization(A,polarity)
#'
#' @export

matrix_normalization <- function(x,polarity =NULL){
  n <- ncol(x) #Variables
  m <- nrow(x) #Countrys
  if (sum(is.na(x)) >0) {
    warning("there are NA's in the data")
  }
  if (!is.matrix(x)) {
    warning("the argument 'matriz' would be a matrix object")
    x <- as.matrix(x)
  }

  # Function Normalization
  normalization <- function(x,polarity=NULL){
    matrix <- x
    columns <- c(1:n)
    pospol <- polarity
    if(!is.null(polarity)){
      negpol <- columns[-pospol]
    }else{
      negpol <- NULL
    }

    normdata <- matrix(0,ncol = ncol(matrix),nrow = nrow(matrix))

    norm_minmax <- function(x){
      (x- min(x)) /(max(x)-min(x))
    }

    norm_maxmin <- function(x){
      (max(x)-x) /(max(x)-min(x))
    }

    for(j in pospol){
      normdata[,j]= norm_minmax(matrix[,j])
    }

    for(j in negpol){
      normdata[,j]= norm_maxmin(matrix[,j])
    }
    normdata <- as.data.frame(normdata)
    colnames(normdata) = colnames(x)
    rownames(normdata) <- rownames(x)
    return(normdata)
  }
  invisible(return(normalization(x,polarity )))
}
