
library(texreg)

# Github: https://github.com/leifeld/texreg/blob/master/R/extract.R

# -- extract.lm (stats) --------------------------------------------------------

extract.lm <- function(model, include.rsquared = TRUE, include.adjrs = TRUE,
                       include.nobs = TRUE, include.fstatistic = FALSE,
                       include.rmse = FALSE, ...) {
  s <- summary(model, ...)
  
  names <- rownames(s$coefficients)
  co <- s$coefficients[, 1]
  se <- s$coefficients[, 2]
  pval <- s$coefficients[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (isTRUE(include.rsquared)) {
    rs <- s$r.squared  # extract R-squared
    gof <- c(gof, rs)
    gof.names <- c(gof.names, "R$^2$")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (isTRUE(include.adjrs)) {
    adj <- s$adj.r.squared  # extract adjusted R-squared
    gof <- c(gof, adj)
    gof.names <- c(gof.names, "Adj. R$^2$")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (isTRUE(include.nobs)) {
    n <- nobs(model)  # extract number of observations
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num. obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (isTRUE(include.fstatistic)) {
    fstat <- s$fstatistic[[1]]
    gof <- c(gof, fstat)
    gof.names <- c(gof.names, "F statistic")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (isTRUE(include.rmse) && !is.null(s$sigma[[1]])) {
    rmse <- s$sigma[[1]]
    gof <- c(gof, rmse)
    gof.names <- c(gof.names, "RMSE")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

setMethod("extract", signature = className("lm", "stats"),
          definition = extract.lm)