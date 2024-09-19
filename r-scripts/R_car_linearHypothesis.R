
car::linearHypothesis

methods(linearHypothesis)

getS3method(f = "linearHypothesis", class = "lm")

linearHypothesis_lm_edited <- function (model,
                                     hypothesis.matrix,
                                     rhs = NULL,
                                     test = c("F", "Chisq"),
                                     vcov. = NULL,
                                     white.adjust = c(FALSE, TRUE, "hc3", "hc0", "hc1", "hc2", "hc4"),
                                     singular.ok = FALSE, ...) {
  
  
  # inputs
  model <- lm.res.06
  hypothesis.matrix <- c("I(HiEL * STR)=0", "I(HiEL * STR^2)=0", "I(HiEL * STR^3)=0")
  rhs <- NULL
  test <- NULL
  vcov. <- NULL
  white.adjust <- c("hc1")
  singular.ok <- FALSE
  
  
  
  
  
  if (df.residual(model) == 0) {
    stop("residual df = 0")
  }
  if (deviance(model) < sqrt(.Machine$double.eps)) {
    stop("residual sum of squares is 0 (within rounding error)")
  }
  if (!singular.ok && is.aliased(model)) {
    stop("there are aliased coefficients in the model.")
  }
  test <- match.arg(test)
  white.adjust <- as.character(white.adjust)
  white.adjust <- match.arg(white.adjust)
  if (white.adjust != "FALSE") {
    if (white.adjust == "TRUE") {
      white.adjust <- "hc3"
    }
    vcov. <- hccm(model, type = white.adjust)
  }
  rval <- linearHypothesis.default(model, hypothesis.matrix, 
                                   rhs = rhs, test = test, vcov. = vcov., singular.ok = singular.ok, 
                                   ...)
  if (is.null(vcov.)) {
    rval2 <- matrix(rep(NA, 4), ncol = 2)
    colnames(rval2) <- c("RSS", "Sum of Sq")
    SSH <- rval[2, test]
    if (test == "F") {
      SSH <- SSH * abs(rval[2, "Df"])
    }
    df <- rval[2, "Res.Df"]
    error.SS <- deviance(model)
    rval2[, 1] <- c(error.SS + SSH * error.SS/df, error.SS)
    rval2[2, 2] <- abs(diff(rval2[, 1]))
    rval2 <- cbind(rval, rval2)[, c(1, 5, 2, 6, 3, 4)]
    class(rval2) <- c("anova", "data.frame")
    attr(rval2, "heading") <- attr(rval, "heading")
    attr(rval2, "value") <- attr(rval, "value")
    attr(rval2, "vcov") <- attr(rval, "vcov")
    rval <- rval2
  }
  rval
}
# <bytecode: 0x000001e7ef95d938>
#   <environment: namespace:car>

getS3method(f = "linearHypothesis", class = "default")

linearHypothesis_default_edited <- function (model,
                                             hypothesis.matrix,
                                             rhs = NULL,
                                             test = c("Chisq", "F"),
                                             vcov. = NULL,
                                             singular.ok = FALSE,
                                             verbose = FALSE,
                                             coef. = coef(model),
                                             suppress.vcov.msg = FALSE,
                                             error.df, ...) {
  
  
  
  if (missing(error.df)) {
    df <- df.residual(model)
    test <- match.arg(test)
    if (test == "F" && (is.null(df) || is.na(df))) {
      test <- "Chisq"
      message("residual df unavailable, test set to 'Chisq'")
    }
  } else {
    df <- error.df
  }
  if (is.null(df)) {
    df <- Inf
  }
  if (df == 0) {
    stop("residual df = 0")
  }
  V <- if (is.null(vcov.)) {
    vcov(model, complete = FALSE)
  } else if (is.function(vcov.)) {
    vcov.(model)
  } else {
    vcov.
  }
  b <- coef.
  if (any(aliased <- is.na(b)) && !singular.ok) {
    stop("there are aliased coefficients in the model")
  }
  b <- b[!aliased]
  if (is.null(b)) {
    stop(paste("there is no coef() method for models of class", paste(class(model), collapse = ", ")))
  }
  if (is.character(hypothesis.matrix)) {
    L <- makeHypothesis(names(b), hypothesis.matrix, rhs)
    if (is.null(dim(L))) {
      L <- t(L)
    }
    rhs <- L[, NCOL(L)]
    L <- L[, -NCOL(L), drop = FALSE]
    rownames(L) <- hypothesis.matrix
  } else {
    L <- if (is.null(dim(hypothesis.matrix))) {
      t(hypothesis.matrix)
    } else {
      hypothesis.matrix
    }
    if (is.null(rhs)) {
      rhs <- rep(0, nrow(L))
    }
  }
  q <- NROW(L)
  value.hyp <- L %*% b - rhs
  vcov.hyp <- L %*% V %*% t(L)
  if (verbose) {
    cat("\nHypothesis matrix:\n")
    print(L)
    cat("\nRight-hand-side vector:\n")
    print(rhs)
    cat("\nEstimated linear function (hypothesis.matrix %*% coef - rhs)\n")
    print(drop(value.hyp))
    cat("\n")
    if (length(vcov.hyp) == 1) 
      cat("\nEstimated variance of linear function\n")
    else cat("\nEstimated variance/covariance matrix for linear function\n")
    print(drop(vcov.hyp))
    cat("\n")
  }
  SSH <- as.vector(t(value.hyp) %*% solve(vcov.hyp) %*% value.hyp)
  test <- match.arg(test)
  if (!(is.finite(df) && df > 0)) {
    test <- "Chisq"
  }
  name <- try(formula(model), silent = TRUE)
  if (inherits(name, "try-error")) {
    name <- substitute(model)
  }
  title <- "Linear hypothesis test\n\nHypothesis:"
  topnote <- paste("Model 1: restricted model", "\n", "Model 2: ", paste(deparse(name), collapse = "\n"), sep = "")
  note <- if (is.null(vcov.) || suppress.vcov.msg) {
    ""
  } else {
    "\nNote: Coefficient covariance matrix supplied.\n"
  }
  rval <- matrix(rep(NA, 8), ncol = 4)
  colnames(rval) <- c("Res.Df", "Df", test, paste("Pr(>", test, ")", sep = ""))
  rownames(rval) <- 1:2
  rval[, 1] <- c(df + q, df)
  if (test == "F") {
    f <- SSH/q
    p <- pf(f, q, df, lower.tail = FALSE)
    rval[2, 2:4] <- c(q, f, p)
  } else {
    p <- pchisq(SSH, q, lower.tail = FALSE)
    rval[2, 2:4] <- c(q, SSH, p)
  }
  if (!(is.finite(df) && df > 0)) {
    rval <- rval[, -1]
  }
  result <- structure(as.data.frame(rval), heading = c(title, printHypothesis(L, rhs, names(b)), "", topnote, note), 
                      class = c("anova", "data.frame"))
  attr(result, "value") <- value.hyp
  attr(result, "vcov") <- vcov.hyp
  result
}
# <bytecode: 0x000001e7ef9c9c50>
#   <environment: namespace:car>

methods(hccm)

getS3method(f = "hccm", class = "lm")

hccm_lm_edited <- function (model, type = c("hc3", "hc0", "hc1", "hc2", "hc4"), singular.ok = TRUE, ...) {
  
  # inputs
  model <- model
  type <- white.adjust
  
  
  
  e <- na.omit(residuals(model))
  removed <- attr(e, "na.action")
  wts <- if (is.null(weights(model))) {
    1
  } else {
    weights(model)
  }
  type <- match.arg(type)
  if (any(aliased <- is.na(coef(model))) && !singular.ok) {
    stop("there are aliased coefficients in the model")
  }
  sumry <- summary(model, corr = FALSE)
  s2 <- sumry$sigma^2
  V <- sumry$cov.unscaled
  if (type == FALSE) {
    return(s2 * V)
  }
  h <- hatvalues(model)
  if (!is.null(removed)) {
    wts <- wts[-removed]
    h <- h[-removed]
  }
  X <- model.matrix(model)[, !aliased, drop = FALSE]
  df.res <- df.residual(model)
  n <- length(e)
  e <- wts * e
  p <- ncol(X)
  factor <- switch(type, hc0 = 1, hc1 = df.res/n, hc2 = 1 - h, hc3 = (1 - h)^2, hc4 = (1 - h)^pmin(4, n * h/p))
  V <- V %*% t(X) %*% apply(X, 2, "*", (e^2)/factor) %*% V
  bad <- h > 1 - sqrt(.Machine$double.eps)
  if ((n.bad <- sum(bad)) > 0) {
    nms <- names(e)
    bads <- if (n.bad <= 10) {
      paste(nms[bad], collapse = ", ")
    } else {
      paste0(paste(nms[bad[1:10]], collapse = ", "), ", ...")
    }
  }
  if (n.bad > 0 & any(is.nan(V))) {
    stop("hccm estimator is singular because of ", n.bad, if (n.bad == 1) " case " else " cases ", "with hatvalue = 1:\n   ", bads)
  }
  if (qr(V)$rank < p) {
    stop("hccm estimator is singular because of ", n.bad, if (n.bad == 1) " case " else " cases ", "with hatvalue = 1:\n   ", bads)
  }
    
  V
  
}
# <bytecode: 0x000001e7ef97c908>
#   <environment: namespace:car>