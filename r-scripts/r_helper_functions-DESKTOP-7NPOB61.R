
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
