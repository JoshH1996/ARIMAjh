#'Fit ARIMA Model
#'
#'This function can be used to find the estimated parameters for an ARIMA model with a specified order.
#'The function will also provide the standard errors for these estimates and the AIC, AICc and BIC which
#'are values which can be used to asses the fit of the chosen ARIMA model.
#'@param x is the column of a dataframe or a vector that you wish to fit an ARIMA model for
#'@param order relates to the p, d, q values that you wish to utilise for the ARIMA model.
#'P represents the number of lags used in the model, D represents the degree of differencing and Q represents the number of error terms that will be used in the model.
#'If D>0 no mean term will be included in the model.
#'@examples
#'JHarima(Lloyds_Share_Price$Price, c(1,1,1))
#'This is an example where the ARIMA model with p =1, d=1 and q=1 is fitted for the values
#'in the price column of the dataframe Lloyds_Share_Price
#'@export
JHarima <- function (x, y, z, order = c(0, 0, 0), h = 50)
{
  if(anyNA(x))
    stop("There can be no NA values")
  series <- deparse(substitute(x))
  origx <- x
  if (NCOL(x) > 1L)
    stop("only implemented for univariate time series")
  if (!is.numeric(x))
    stop("'x' must be numeric")
  if(y != "null"){
    ynew <- y[order(as.Date(z, format="%d/%m/%Y")),]}
    suppressWarnings(ats <- stats::arima(x = x, order = order))
  np <- length(ats$coef) + 1
  ns <- length(ats$residuals) - ats$arma[6] - ats$arma[7] *
    ats$arma[5]
  ats$bic <- ats$aic + np * (log(ns) - 2)
  ats$x <- origx
    ats$sigma2 <- sum(ats$residuals^2, na.rm = TRUE)/(ns - np + 1)
    ats$aicc <- ats$aic + 2 * np * (ns/(ns - np - 1) - 1)
    class(ats)=c("JHarima","Arima")
    if(y != "null"){ats$x <- ynew}
    ats$y <- z
    ats$z <- x
    ats$h <- h
   return(ats)
}
out <- structure(ats, class = c("ARIMA", "Arima"))
out$fitted <- fitted(out)
out$series <- series
return(out)