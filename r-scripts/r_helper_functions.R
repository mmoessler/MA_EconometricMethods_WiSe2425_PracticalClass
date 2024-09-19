
lm_ct_fun <- function(formula, ..., hc.type = "HC1") {
  
  z <- stats::lm(formula, ...)
  z$sum <- summary(z)
  
  if (hc.type == "const") {
    z$ct <- lmtest::coeftest(z, vcov = sandwich::vcovHC(z, type="const")) # test based on ordinary SEs
  } else if (hc.type == "HC1") {
    z$ct <- lmtest::coeftest(z, vcov = sandwich::vcovHC(z, type="HC1")) # test based on heteroskedasticity robust SEs
  }
  
  z
  
}

plm_ct_fun <- function(formula, data, index, model = "within", effect = "individual", hc.type = "STATA") {
  
  ii <- which(colnames(data) %in% index[1])
  jj <- which(colnames(data) %in% index[2])
  
  data[,ii] <- as.factor(data[,ii])
  data[,jj] <- as.factor(data[,jj])
  
  # plm object
  z <- plm::plm(formula=formula, data=data, index=index, model=model, effect=effect)
  z$sum <- summary(z)
  
  # small sample adjustment (see STATA)  
  G <- nrow(unique(attributes(model.matrix(z))$index[index[1]]))
  N <- nrow(data)
  K <- ncol(model.matrix(z))
  dfa <- (G/(G - 1)) * (N - 1)/(N - K - 1)
  
  if (hc.type == "STATA") {
    vc <- dfa * plm::vcovG(z, type = "HC0", cluster = "group")
  } else {
    vc <- sandwich::vcovHC(z, type = "HC1")
  }
  z$ct <- lmtest::coeftest(z, vcov = vc)
  
  z
  
}

glm_ct_fun <- function(formula, ..., hc.type = "HC1") {
  
  z <- stats::glm(formula, ...)
  
  z$sum <- summary(z)
  
  if (hc.type == "const") {
    z$ct <- lmtest::coeftest(z, vcov = sandwich::vcovHC(z, type="const")) # test based on ordinary SEs
  } else if (hc.type == "HC1") {
    z$ct <- lmtest::coeftest(z, vcov = sandwich::vcovHC(z, type="HC1")) # test based on heteroskedasticity robust SEs
  }
  
  z
  
}

ivreg_ct_fun <- function(formula, ..., hc.type = "HC1") {
  
  z <- AER::ivreg(formula, ...)
  z$sum <- summary(z)
  
  if (hc.type == "const") {
    z$ct <- lmtest::coeftest(z, vcov = sandwich::vcovHC(z, type="const")) # test based on ordinary SEs
  } else if (hc.type == "HC1") {
    z$ct <- lmtest::coeftest(z, vcov = sandwich::vcovHC(z, type="HC1")) # test based on heteroskedasticity robust SEs
  }
  
  z
  
}

lag_len_crit_fun <- function(model) {
  
  if (c("lm") %in% class(model)) {
    R2 <- summary(model)$r.squared
    p <- length(model$coef)-1
  } else if (c("summary.lm") %in% class(model)) {
    R2 <- model$r.squared
    p <- nrow(model$coef)-1
  }
  
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  ssr.t <- ssr/t
  log.ssr.t <- log(ssr.t)
  
  p1.ln.t.t <- (p+1)*log(t)/t
  
  BIC <- log(ssr/t) + (p+1) * log(t)/t
  AIC <- log(ssr/t) + (p+1) * 2/t
  
  ret.lis <- list(p = p, t = t,
                  ssr = round(ssr,4),
                  ssr.t = round(ssr.t,4),
                  log.ssr.t = round(log.ssr.t,4),
                  p1.ln.t.t = round(p1.ln.t.t,4),
                  R2 = round(R2,4),
                  BIC = round(BIC,4),
                  AIC = round(AIC,4))
  
  return(ret.lis)
  
}

ill_hyp_tes_fun <- function(t.act, test=c("two.sid")) {
  
  # plot parameters
  par(mfrow=c(1,1),
      mar=c(4,2,4,2))
  # plot the standard normal density on the interval [-6,6]
  curve(dnorm(x),
        xlim = c(-6, 6),
        yaxs = "i",
        xlab = "z",
        ylab = "",
        lwd = 2,
        axes = "F",
        ylim = c(0,0.45))
  if(test==c("one.sid.leq")){
    # add x-axis
    axis(3,at = c(-6, 0, 1.64, 2.33, 6), las=2,
         labels = c("", "0", "1.64", "2.33", ""))
    axis(1,at = c(-6, round(t.act,2), 6))
    # shade p-value region in right tail
    if (t.act <= 6) {
      polygon(x = c(t.act, seq(t.act, 6, 0.01), 6),
              y = c(0, dnorm(seq(t.act, 6, 0.01)), 0), 
              col = "steelblue")
    }
    # add critical value lines
    abline(v=1.64, lty=2)
    abline(v=2.33, lty=2)
    # add actual test value lines
    abline(v=t.act, col="red", lty=2)
  } else if(test==c("one.sid.geq")){
    # add x-axis
    axis(3,at = c(-6, -2.33, -1.64, 0, 6), las=2,
         labels = c("", "-2.33", "-1.64", "0", ""))
    axis(1,at = c(-6, round(t.act,2), 6))
    # shade p-value region in left tail
    if (t.act >= -6) {
      polygon(x = c(-6, seq(-6, t.act, 0.01), t.act),
              y = c(0, dnorm(seq(-6, t.act, 0.01)), 0), 
              col = "steelblue")
    }
    # add critical value lines
    abline(v=-1.64, lty=2)
    abline(v=-2.33, lty=2)
    # add actual test value lines
    abline(v=t.act, col="red", lty=2)
  } else {
    t.act <- abs(t.act)
    # add x-axis
    axis(3,at = c(-6, -2.58, -1.96, 0, 1.96, 2.58, 6), las=2,
         labels = c("","-2.58", "-1.96", "0", "1.96", "2.58", ""))
    axis(1,at = c(0, round(-t.act,2), round(t.act,2)))
    # shade p-value/2 region in left tail
    if (-t.act >= -6) {
      polygon(x = c(-6, seq(-6, -t.act, 0.01), -t.act),
              y = c(0, dnorm(seq(-6, -t.act, 0.01)),0), 
              col = "steelblue")
    }
    # shade p-value/2 region in right tail
    if (t.act <= 6) {
      polygon(x = c(t.act, seq(t.act, 6, 0.01), 6),
              y = c(0, dnorm(seq(t.act, 6, 0.01)), 0), 
              col = "steelblue")
    }
    # add critical value lines
    abline(v=-1.96, lty=2)
    abline(v= 1.96, lty=2)
    abline(v=-2.58, lty=2)
    abline(v= 2.58, lty=2)
    # add actual test value lines
    abline(v=-t.act, col="red", lty=2)
    abline(v=t.act, col="red", lty=2)
  } 
  
}

##
## Augmented-Dickey-Fuller Test
##

urca_ur_df_fun <- function (y, type = c("none", "drift", "trend"), lags = 1, selectlags = c("Fixed", "AIC", "BIC")) {
  
  selectlags<-match.arg(selectlags)
  type <- match.arg(type)
  if (ncol(as.matrix(y)) > 1) {
    stop("\ny is not a vector or univariate time series.\n")
  }
  if (any(is.na(y))) {
    stop("\nNAs in y.\n")
  }
  y <- as.vector(y)
  lag <- as.integer(lags)
  if (lag < 0) {
    stop("\nLags must be set to an non negative integer value.\n")
  }
  CALL <- match.call()
  DNAME <- deparse(substitute(y))
  x.name <- deparse(substitute(y))
  lags <- lags + 1
  z <- diff(y)
  n <- length(z)
  x <- embed(z, lags) # MM: x is (max) lag rows short then z => Same # of obs. for all models
  z.diff <- x[, 1]
  z.lag.1 <- y[lags:n]
  tt <- lags:n
  
  # MM added to extract different "AR models"
  lm.res <- list()
  # MM added to estimate DF test regression
  if (type == "none") {
    lm.res[[1]] <- lm(z.diff ~ z.lag.1 - 1)
  }
  if (type == "drift") {
    lm.res[[1]] <- lm(z.diff ~ z.lag.1 + 1)  
  }
  if (type == "trend") {
    lm.res[[1]] <- lm(z.diff ~ z.lag.1 + 1 + tt)
  }
  
  
  
  if (lags > 1) {
    if(selectlags!="Fixed"){
      critRes<-rep(NA, lags)
      for(i in 2:(lags)){
        z.diff.lag = x[, 2:i]
        if (type == "none") {
          result <- lm(z.diff ~ z.lag.1 - 1 + z.diff.lag)
        }
        if (type == "drift") {
          result <- lm(z.diff ~ z.lag.1 + 1 + z.diff.lag)  
        }
        if (type == "trend") {
          result <- lm(z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
        }
        critRes[i]<-AIC(result, k = switch(selectlags, "AIC" = 2, "BIC" = log(length(z.diff))))
        
        # MM added to extract different "AR models"
        lm.res[[i]] <- result
        
      }
      lags<-which.min(critRes)
    }
    z.diff.lag = x[, 2:lags]
    if (type == "none") {
      result <- lm(z.diff ~ z.lag.1 - 1 + z.diff.lag)
      tau <- coef(summary(result))[1, 3]
      teststat <- as.matrix(tau)
      colnames(teststat) <- 'tau1'
    }
    if (type == "drift") {
      result <- lm(z.diff ~ z.lag.1 + 1 + z.diff.lag)
      tau <- coef(summary(result))[2, 3]
      phi1.reg <- lm(z.diff ~ -1 + z.diff.lag)
      phi1 <- anova(phi1.reg, result)$F[2]
      teststat <- as.matrix(t(c(tau, phi1)))
      colnames(teststat) <- c('tau2', 'phi1')
    }
    if (type == "trend") {
      result <- lm(z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
      tau <- coef(summary(result))[2, 3]
      phi2.reg <- lm(z.diff ~ -1 + z.diff.lag)
      phi3.reg <- lm(z.diff ~ z.diff.lag)
      phi2 <- anova(phi2.reg, result)$F[2]
      phi3 <- anova(phi3.reg, result)$F[2]
      teststat <- as.matrix(t(c(tau, phi2, phi3)))
      colnames(teststat) <- c('tau3', 'phi2', 'phi3')
    }
  } else {
    if (type == "none") {
      result <- lm(z.diff ~ z.lag.1 - 1)
      tau <- coef(summary(result))[1, 3]
      teststat <- as.matrix(tau)
      colnames(teststat) <- 'tau1'
    }
    if (type == "drift") {
      result <- lm(z.diff ~ z.lag.1 + 1)
      phi1.reg <- lm(z.diff ~ -1)
      phi1 <- anova(phi1.reg, result)$F[2]
      tau <- coef(summary(result))[2, 3]
      teststat <- as.matrix(t(c(tau, phi1)))
      colnames(teststat) <- c('tau2', 'phi1')
    }
    if (type == "trend") {
      result <- lm(z.diff ~ z.lag.1 + 1 + tt)
      phi2.reg <- lm(z.diff ~ -1)
      phi3.reg <- lm(z.diff ~ 1)
      phi2 <- anova(phi2.reg, result)$F[2]
      phi3 <- anova(phi3.reg, result)$F[2]
      tau <- coef(summary(result))[2, 3]
      teststat <- as.matrix(t(c(tau, phi2, phi3)))
      colnames(teststat) <- c('tau3', 'phi2', 'phi3')
    }
  }
  rownames(teststat) <- 'statistic'
  testreg <- summary(result)
  res <- residuals(testreg)
  if(n < 25) {
    rowselec <- 1
  }
  if(25 <= n & n < 50) {
    rowselec <- 2
  }
  if(50 <= n & n < 100) {
    rowselec <- 3
  }
  if(100 <= n & n < 250) {
    rowselec <- 4
  }
  if(250 <= n & n < 500) {
    rowselec <- 5
  }
  if(n >= 500) {
    rowselec <- 6
  }
  if (type == "none"){ 
    cval.tau1 <- rbind(
      c(-2.66, -1.95, -1.60),
      c(-2.62, -1.95, -1.61),
      c(-2.60, -1.95, -1.61),
      c(-2.58, -1.95, -1.62),
      c(-2.58, -1.95, -1.62),
      c(-2.58, -1.95, -1.62))
    cvals <- t(cval.tau1[rowselec, ])
    testnames <- 'tau1'
  }
  if (type == "drift"){ 
    cval.tau2 <- rbind(
      c(-3.75, -3.00, -2.63),
      c(-3.58, -2.93, -2.60),
      c(-3.51, -2.89, -2.58),
      c(-3.46, -2.88, -2.57),
      c(-3.44, -2.87, -2.57),
      c(-3.43, -2.86, -2.57))
    cval.phi1 <- rbind(
      c(7.88, 5.18, 4.12),
      c(7.06, 4.86, 3.94),
      c(6.70, 4.71, 3.86),
      c(6.52, 4.63, 3.81),
      c(6.47, 4.61, 3.79),
      c(6.43, 4.59, 3.78))
    cvals <- rbind(
      cval.tau2[rowselec, ],
      cval.phi1[rowselec, ])
    testnames <- c('tau2', 'phi1')
  }
  if (type == "trend"){ 
    cval.tau3 <- rbind(
      c(-4.38, -3.60, -3.24),
      c(-4.15, -3.50, -3.18),
      c(-4.04, -3.45, -3.15),
      c(-3.99, -3.43, -3.13),
      c(-3.98, -3.42, -3.13),
      c(-3.96, -3.41, -3.12))
    cval.phi2 <- rbind(
      c(8.21, 5.68, 4.67),
      c(7.02, 5.13, 4.31),
      c(6.50, 4.88, 4.16),
      c(6.22, 4.75, 4.07),
      c(6.15, 4.71, 4.05),
      c(6.09, 4.68, 4.03))
    cval.phi3 <- rbind(
      c(10.61, 7.24, 5.91),
      c( 9.31, 6.73, 5.61),
      c( 8.73, 6.49, 5.47),
      c( 8.43, 6.49, 5.47),
      c( 8.34, 6.30, 5.36),
      c( 8.27, 6.25, 5.34))  
    cvals <- rbind(
      cval.tau3[rowselec, ],
      cval.phi2[rowselec, ],
      cval.phi3[rowselec, ])
    
    testnames <- c('tau3', 'phi2', 'phi3')
  }
  colnames(cvals) <- c("1pct", "5pct", "10pct")
  rownames(cvals) <- testnames
  
  ret.lis <- list(y = y, model = type, cval=cvals, lags=lag, teststat = teststat, testreg=testreg, res=res, test.name="Augmented Dickey-Fuller Test", result=result)
  try(ret.lis$lm.res <- lm.res, silent = TRUE)
  try(ret.lis$critRes <- critRes, silent = TRUE)
  
  return(ret.lis)
  
  # new("ur.df.edit", y = y, model = type, cval=cvals, lags=lag, teststat = teststat, testreg=testreg, res=res, test.name="Augmented Dickey-Fuller Test")
  
}

