\name{theil_sen}
\alias{theil_sen_regression}
\alias{siegel_regression}

\title{Theil Sen Regression}
\description{
  Compute Theil Sen Regression and Repeated Medians Regression (Siegel)
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
  \code{siegel_regression} compute linear regression using the repeated median estimator for the slope, propose by Siegel (1982).
  The main difference is that \code{siegel_regression} is less sensitive to outliers in the data.
}

\examples{
  # create x axis (t)
  t <- 1:100
  # create values that follow a linear relation with the x axis
  x <- rnorm(100,35,4)*t/100
  # add some outliers
  x[c(10,12,76,34,21)] <- x[c(10,12,76,34,21)] + 40
  model <- theil_sen_regression(x~t)
  lm_model <- lm(x~t)
  # compare linear regression with theil_sen_regression
  plot(x~t)
  abline(model,col='blue')
  abline(lm_model,col='red')
}

\references{
- Theil, H., 1992. A rank-invariant method of linear and polynomial regression analysis. In: Henri Theil’s contributions to economics and econometrics. Springer, pp. 345–381. URL https://doi.org/10.1007/978-94-011-2546-8_20

- Sen, P. K., 1968. Estimates of the regression coefficient based on kendall’s tau. Journal of the American statistical association 63 (324), 1379–1389.
}
