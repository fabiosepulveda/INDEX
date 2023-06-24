#' DP2 Function
#'
#' @description
#' This function computes a indicator
#'
#' @param x A numeric matrix or data frame which provides the data set with all the study variables.
#' @param epsilon  A double maximum number of distance accepted.
#' @param iterations maximum number of iterations for the algorithm
#' @param polarity polarity
#'
#' @return
#' Return the a matrix with the variance inflation factor and a boolean variable if there is multicollinearity
#' @importFrom stats lm
#' @importFrom stats cor
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 aes
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
#' C <- NUTS2[,4:11]
#' polarity <- c(5,6,7,8)
#' dp2(C, iterations=10 ,polarity = polarity)
#'
#' @export
#'
#'
dp2 <- function(x,iterations,epsilon = 0.0001,polarity=NULL,qualitative=NULL){

  n <- ncol(x) #Variables
  m <- nrow(x) #Countrys

  if (sum(is.na(x)) >0) {
    warning("there are NA's in the data")
  }

  # Functions ---------------------------------------------------------------

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

  t_abs <- function(x,n,m) {
    v1 <- t(apply(x, 1, function(y) abs(y) ))
    return(v1)
  }



  order_data = function(x,n,m){
    l = cor(x)
    p=abs(l[1:n,ncol(l)])
    orden <- colnames(x)[order(p,decreasing = TRUE)]
    x <- x[,1:(n)]

    DD = subset(t_abs(x,n,m) ,select = orden)
    DD = as.matrix(DD)
    C = rep(0,ncol(DD))

    for (i in 2:ncol(DD)) {
      modelo = lm(DD[,i]~DD[,1:i-1])
      C[i] = summary(modelo)$r.squared

    }
    W = list(correlations = p, DataOrdered = DD, Coefs.determi = C)
    return(W)
  }

  DP2parcial = function(x,z1,z2,n,m){
    q = as.matrix(1-z2)
    AA = z1%*%q
    R = cbind(t_abs(x,n,m),Indicador=AA)
    return(R)
  }

  Frechet = function(x, n, m){
    v1 <- t_abs(x, n, m)
    v1 <- rowSums(v1)
    return(v1)
  }

  if(!is.null(qualitative)){
    cuanti <-function(x,qualitative){
      if(is.null(qualitative)){
        x <- x
      } else
      {matqual <- x[,qualitative]
      fitmatqual <- homals(matqual,ndim = 1)
      clnames <- colnames(matqual)
      matquan <- fitmatqual$scoremat
      x[,colnames(matquan)]<- matquan}
      return(x)
    }
  }
  #Assign names to data base
  if(is.null(colnames(x))){
    colnames(x) <- 1:ncol(x)
  }


  if(!is.null(qualitative)){
    x <- as.matrix(cuanti(x,qualitative))
  }else{
    x <- as.matrix(x)
  }
  x <- normalization(x,polarity)
  # frechet distance
  h=Frechet(x,n,m)
  h <- as.matrix(h)

  G = cbind(as.matrix(t_abs(x,n,m)),h)

  CC = order_data(G,n,m)


  TT =  DP2parcial(x,CC[[2]],CC[[3]],n,m)

  iteration <- 0
  itera <- 2

  dist_list <- c()
  distance <- Inf
  while ((iteration <= iterations) && (epsilon <= distance)) {

    iteration = iteration + 1

    Y <-  TT
    CC <-  order_data(Y,n,m)
    TT <- DP2parcial(x,CC[[2]],CC[[3]],n,m)
    distance <-  mean(abs(TT[,ncol(x)+1] - Y[,ncol(x)+1]))
    dist_list <- c(dist_list,distance)
    cat(paste("Iteracion",iteration,"Distance" ))
    print(distance)

    if(iteration>=itera){
      if(abs(dist_list[iteration]-dist_list[iteration-1])<=epsilon/100 ){
        warning("the itertions stops because the error is stable")
        break
      }
    }


  }

  data_g <- data.frame(Iteration = 1:iteration,
                       Distance = dist_list)
  data_g <- data_g %>%
    ggplot( aes(x="Iteration", y="Distance")) +
    geom_line( color="blue",linewidth=0.2) +
    geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
    theme_classic()+
    ggtitle("Evolution distance")

  invisible(return(list(
    distance = distance,
    plot = data_g,
    Comp.Indicator = TT,
    Comp.Indicator_1 = TT[,n+1],
    iteration = iteration,
    Names.Single.Ind = colnames(x),
    Order =  colnames(CC$DataOrdered),
    Coefficient.Determination <- CC$Coefs.determi

  ) ))
}
