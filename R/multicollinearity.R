#' Multicollinearity
#'
#' @description
#' This function computes a matrix with the variance inflation factor and evaluate if the variance inflation factor is greater than 10
#'
#' @param x A numeric matrix or data frame which provides the data set with all the study variables.
#'
#' @return
#' Return the a matrix with the variance inflation factor and a boolean variable if there is multicollinearity
#' @importFrom car vif
#' @importFrom stats lm
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom stats as.formula
#'
#' @author
#' Fabio Sepulveda:  \email{fhsepulveda@udemedellin.edu.co}
#' Andrés Vinueza:   \email{andres.vinueza@epn.edu.ec}
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
#' B <- NUTS2[,4:11]
#' multicollinearity(B)
#'
#' @export


multicollinearity <- function(x){
  x <- as.data.frame(x)
  vars <- colnames(x)
  l <- data.frame()

  for (variable in vars) {
    lm1 <-lm( as.formula( paste0(variable,"~.")),data = x)
    options(scipen = 9990)
    v <- vif(lm1)
    l <- bind_rows(l,v)
  }
  rownames(l) <-  vars
  l[is.na(l)] <- 0
  v <- FALSE
  if(length(l[l>10] )>0){
    v <- TRUE
    warning("There is variance inflation factor greater than 10")
  }
  invisible(return(list(multicollinearity = v,
                        matrix_variance_inflation = l
  )  ))
}
