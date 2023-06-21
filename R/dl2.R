#' DL2 Function
#'
#' @description
#' This function computes a indicator
#' @param x A numeric matrix or data frame which provides the data set with all the study variables.
#' @param polarity  A double maximum number of distance accepted.
#' @param alpha maximum number of iterations for the algorithm
#' @param qualitative numeric vector which provides the position of qualitative variables
#' @param iterations maximum number of iterations
#' @param tau error
#' @param prop_split proportion
#' @param degrees degrees
#'
#'
#'
#' @return
#' Return the a matrix with the variance inflation factor and a boolean variable if there is multicollinearity
#' @importFrom stats lm
#' @importFrom stats cor
#' @importFrom stats cor.test
#' @importFrom dplyr filter
#' @importFrom stats predict
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom corrgram corrgram
#' @importFrom corrgram panel.shade
#' @importFrom corrgram panel.pie
#' @importFrom corrgram panel.txt
#' @importFrom vip vi
#' @importFrom caret train
#' @importFrom caret trainControl
#' @importFrom stats as.formula
#' @importFrom rsample initial_split
#' @importFrom rsample training
#' @importFrom rsample testing
#' @importFrom utils globalVariables
#'
#' @author
#' Eduardo Jiménez: \email{edjimfer@ugr.es}
#' Angeles Sánchez: \email{sancheza@ugr.es}
#'
#' @references
#' Jiménez-Fernández, E., Sánchez, A., & Ortega-Pérez, M. (2022). Dealing with weighting scheme in composite indicators: an unsupervised distance-machine learning proposal for quantitative data. Socio-Economic Planning Sciences, 83, 101339.
#'
#' @seealso
#' Fabio
#'
#' @examples
#'
#' "Example 1"
#' data(NUTS2)
#' C <- NUTS2[,4:11]
#' polarity <- c(5,6,7,8)
#' dl2(C, polarity = polarity,  alpha = 0.05,
#' iterations = 20,tau = 0.9,prop_split = 0.8,degrees = 1:2   )
#'
#' "Example 2"
#' library("mlbench")
#' data(BostonHousing)
#' qualitative <- c(4,9)
#' matriz <- as.data.frame(BostonHousing)
#' matriz$rad <- as.factor(matriz$rad)
#' polarity <- c(1,2,5,7,9)
#' dim(BostonHousing)
#' dl2(matriz,polarity =   polarity,qualitative = qualitative,0.05,10,0.9,0.8,1:3)
#'
#' @export
#'
#'
#'
#'
#'


dl2 <-function(x,polarity=NULL,alpha =0.05,qualitative=NULL,
                    iterations=100,tau,prop_split=0.8,degrees){
  if (!is.data.frame(x)) {
    warning("the argument 'x' would be a data.frame object")
    x <- as.data.frame(x)
  }
  ################# Quantification of the qualitative variables
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

  ################# Functions
  # Normalization compatible with respect to the polarity of indicators

  normalization <- function(x,polarity=NULL){
    names_var <- colnames(x)
    names_regions <- rownames(x)
    columns <- c(1:n)
    pospol <- polarity
    if(!is.null(polarity)){
      negpol <- columns[-pospol]
    }else{
      negpol <- NULL
    }

    normdata <- matrix(0,ncol = ncol(x),nrow = nrow(x))

    norm_minmax <- function(x){
      (x- min(x)) /(max(x)-min(x))
    }

    norm_maxmin <- function(x){
      (max(x)-x) /(max(x)-min(x))
    }

    for(j in pospol){
      normdata[,j]= norm_minmax(x[,j])
    }

    for(j in negpol){
      normdata[,j]= norm_maxmin(x[,j])
    }
    normdata <- as.data.frame(normdata)
    colnames(normdata) <-  names_var
    rownames(normdata) <- names_regions
    return(normdata)
  }

  ## Metric
  calcularDistancia = function(x){
    mDif.sqr <- x^2
    return(mDif.sqr)
  }
  ## First composite indicator indicator, allwieghts equal to one
  indexFrechet = function(x) {
    iF <- sqrt(matrix(rowSums(x), ncol = 1))
    colnames(iF) <- "Frechet.Index"
    return(iF)
  }
  ## Compute weights throught MARS and variable importance
  weigthFactors = function(x,Comp.ind,prop_split,degrees){
    #x <- normmat
    #Comp.ind =dps
    m <- ncol(x)
    dat_os <- x
    x$C.Ind <- Comp.ind
    set.seed(123)
    vul_split <- initial_split(x, prop = prop_split, strata = "C.Ind")
    vul_train <- training(vul_split)
    vul_test  <- testing(vul_split)

    hyper_grid <- expand.grid(
      degree = degrees,
      nprune = floor(seq(2, 100, length.out = 10) )
    )
    # We can then fit our model again using only the training data.
    set.seed(123)
    tuned_mars <- train(
      as.formula("C.Ind~."),
      data=vul_train,
      method = "earth",
      metric = "RMSE",
      trControl = trainControl(method = "optimism_boot", number = 10, savePredictions = "all"),
      tuneGrid = hyper_grid
    )
    # Plot best accuracy
    plt <- plot(tuned_mars)
    #display model with lowest test RMSE
    resultsOpt <- tuned_mars$results %>%
      filter(nprune==tuned_mars$bestTune$nprune, degree==tuned_mars$bestTune$degree)
    # Now, we want to check our data on the test set.

    test.features = subset(vul_test, select=-C.Ind)
    test.target = subset(vul_test, select=C.Ind )[,1]
    predictions = predict(tuned_mars, newdata = test.features)
    # RMSE
    RMSEtestPredictError <- sqrt(mean((test.target - predictions)^2))
    # R2
    CorrtestPredict <- cor(test.target, predictions) ^ 2
    # Vi imporatne variable
    p2 <- vi(tuned_mars,method = "firm",sort =FALSE,decreasing = FALSE)
    Variable <- p2$Variable
    weigths <- p2$Importance
    mini = min(weigths[weigths > 0])
    for(j in 1:m){
      if(is.na(weigths[j])) { print('Missing values has been found')}
      else
      {if(weigths[j]==0){
        weigths[j]=mini/m
      }
      }
    }
    weigths <-  weigths/sum(weigths)
    res <- data.frame(t(weigths))
    colnames(res) <- Variable
    res.ord <- res[colnames(dat_os)]
    important <- list(as.numeric(res.ord),names(res.ord),resultsOpt, RMSEtestPredictError,CorrtestPredict,plt)
    return(important)
  }
  # Function to compute the composite indicator with the above weights
  calculateDP2 = function(x, xFactores, iteration = 1) {
    n <- dim(x)[1]
    m <- dim(x)[2]
    coefs <- matrix(xFactores, n, m, byrow = TRUE)
    mDP <- x * coefs
    DP2 <- sqrt(t(t(apply(mDP, MARGIN = 1, sum))))
    colnames(DP2) <- paste("p2distance", iteration,
                           sep = ".")
    calcDP2 <- list(DP2,mDP)
    return(calcDP2)
  }
  ############## Algorithm
  n <- dim(x)[2]
  error.d <- numeric()
  weights <- c(rep(1,ncol(x)))
  if(!is.null(qualitative)){
    matri <- as.matrix(cuanti(x,qualitative))
  }else{
    matri <- as.matrix(x)
  }
  normat <- normalization(matri,polarity)
  corrgr <- corrgram(normat, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="Correlation between each features present in the data")
  mDif <- calcularDistancia(normat);mDif
  dp2_aux <- indexFrechet(mDif)
  dps <- dp2_aux
  iteration <- 1
  itera <- 2
  repeat {
    print(paste("Iteration", iteration))
    weigths <- weigthFactors(normat,dps,prop_split,degrees)
    calcDP2 <- calculateDP2(mDif,weigths[[1]])
    d2_ite <- calcDP2[[1]]
    matrixind <- calcDP2[[2]]
    test <- cor.test(as.vector(d2_ite), as.vector(dps),
                     alternative = c("two.sided"),
                     method = c("kendall"), conf.level = 1-alpha)
    error.d <- c(error.d,test$estimate,test$p.value)
    if (((test$p.value < alpha) && (test$estimate > tau)) || (iteration >= iterations)) {
      break
    }


    if(iteration>=itera){
      if( abs(error.d[2*iteration-3]-error.d[2*iteration-1])<=alpha/100 ){
        warning("the itertions stops because the error is stable")# No es el error es el estimadfos
        break
      }
    }
    iteration <- iteration + 1
    dps=d2_ite
  }


  invisible(return(list(
    tau = error.d,
    normat = normat,
    frechet = dp2_aux,
    matquan = matri,
    plot = weigths[[6]],
    corrgr  = corrgr,
    Comp.Indicator = d2_ite,
    Comp.Indicator_1 = dps,
    iteration = iteration,
    dist.in = mDif,
    dist.fin = matrixind,
    Names.Single.Ind = weigths[[2]],
    Single.Ind = weigths[[1]],
    ResOpt = weigths[[3]],
    testPredictError = weigths[[4]],
    CorrtestPredict = weigths[[5]]
  ) ))

}

utils::globalVariables(c("nprune", "degree","C.Ind"))
