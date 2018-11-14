\name{theil_sen}
\alias{theil_sen_regression}
\alias{siegel_regression}

\title{Theil Sen Regression}
\description{
  Compute Theil Sen Regression and Repeated Medians Regresison (Siegel)
}
\usage{
  theil_sen_regression(formula, data = NULL)

  siegel_regression(formula, data = NULL)
}
\arguments{
  \item{formula}{ object of type formula, must be linear}

  \item{data}{ optional data frame with the data used in formula in columns}

}



\value{
  Returns an object of type "lm"
}

\details{
  \code{theil_sen_regression} compute linear regression using the Theil–Sen estimator base on the median of the slopes.
  \code{siegel_regression} compute linear regression using the repeated median estimator for the slope, propouse by Siegel (1982).
  The main diference is that \code{siegel_regression} is less sensitive to outliers in the data.
}

\examples{

  x <- runif(100,30,40)*t/100
  t <- 1:100
  x[c(10,12,76,34,21)] <- x[c(10,12,76,34,21)] + 30
  model <- theil_sen_regression(x~t)
  lm_model <- lm(x~t)
  plot(x~t)
  abline(model,col='blue')
  abline(lm_model,col='red')
}
