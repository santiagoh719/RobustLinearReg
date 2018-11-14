
siegel_regression <- function(formula, data = NULL){
  # Siegel 1982

  #Chek and assign data:
  aux_data <- is.null(data)
  if(aux_data){ data <- environment(formula)}
  term <- as.character(attr(terms(formula),"variables")[-1])
  if(length(term) > 2){stop('only linear models alow')}
  y <- as.vector(data[[term[1]]])
  x <- as.vector(data[[term[2]]])
  n <- length(y)
  if(n != length(x)){
    stop('x and y must be of same length')
  }

  #Compute:
  y1 <- matrix(y,n,n,byrow = T)
  y2 <- t(y1)

  x1 <- matrix(x,n,n,byrow = T)
  x2 <- t(x1)

  aux <- (y1-y2)/(x1-x2)
  a <- median(apply(aux,1,median,na.rm = T))
  aux <- (x1*y2-x2*y1)/(x1-x2)
  b <- median(apply(aux,1,median,na.rm = T))


  #Create the lm object:
  output=list()

  output$coefficients <- c( b, a)
  names(output$coefficients)[1:2] <- c('(Intercept)',term[[2]])
  output$residuals <- y-a*x-b
  output$fitted.values <- x*a+b
  output$df.residual  <- n-2
  output$rank <- 2
  output$terms <- terms(formula)
  output$call <- match.call()
  output$model <- data.frame(y, x)
  names(output$model) <- term
  output$assign <- c(0, 1)
  if (aux_data) {
    output$effects <- lm(formula)$effects
    output$qr <- lm(formula)$qr
  }else {
    output$effects <- lm(formula, data)$effects
    output$qr <- lm(formula, data)$qr
  }
  output$effects[2] <- sqrt(sum((output$fitted - mean(output$fitted))^2))
  output$xlevels <- list()
  names(output$model) <- term
  attr(output$model, "terms") <- terms(formula)
  class(output) <- c("lm")

  return(output)
}
