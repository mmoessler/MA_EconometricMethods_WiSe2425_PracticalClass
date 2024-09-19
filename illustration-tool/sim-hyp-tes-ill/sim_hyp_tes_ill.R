
# remove all objects
rm(list=ls())

# set directory of figures and tables
fig.dir <- "./sim-hyp-tes-ill/figures/"

library(scales)

# set plot margins
set_plt_mar <- function() {
  par(mar = c(5.1, 4.1, 1, 2.1))
  # bottom, left, top, right
  # 5.1, 4.1, 4.1, 2.1
}

# set seed 
set.seed(12345)

# function to generate pdf ----
hyp_tes_ill_pdf <- function(t.act, test) {
  
  # plot parameters
  par(mfrow=c(1,1),
      mar=c(4,3.5,4,1))
  #bottom, left, top, and right
  
  # > One-sided less or equal ----
  if (test==c("one_sid_leq")){
    # plot the standard normal density on the interval [-6,6]
    curve(dnorm(x),
          xlim = c(-3, 3),
          yaxs = "i",
          xlab = "z",
          ylab = "",
          lwd = 2,
          axes = "F",
          ylim = c(0,0.45),
          cex.lab = 1.5,
          col = alpha("black", 0.25))
    lines(seq(0,3,0.01),dnorm(seq(0,3,0.01)),
          lwd = 2,
          col = alpha("black", 1))
    # add axis
    axis(1, at = c(-3, 0, 1.64, 2.33, 3),
         labels = c("-3", "0", "1.64", "2.33", "3"),
         cex.axis = 1.5, las = 2)
    axis(3, at = c(-3, t.act, 3),
         labels = c("", format(round(t.act, 2), digits = 2, nsmall = 2), ""),
         cex.axis = 1.5, col.axis = "red")
    axis(2,at = seq(0,0.45,0.05), las=2,
         labels = as.character(seq(0,0.45,0.05)),
         cex.axis = 1.5)
    # shade p-value region in right tail
    if (t.act <= 3) {
      polygon(x = c(t.act, seq(t.act, 3, 0.01), 3),
              y = c(0, dnorm(seq(t.act, 3, 0.01)), 0), 
              col = "steelblue")
    }
    # add critical value lines
    abline(v=1.64, lty=2, lwd=2)
    abline(v=2.33, lty=2, lwd=2)
    # add actual test value lines
    abline(v=t.act, col="red", lty=2, lwd=2)
    
    # > One-sided greater or equal ----
  } else if (test==c("one_sid_geq")){
    # plot the standard normal density on the interval [-6,6]
    curve(dnorm(x),
          xlim = c(-3, 3),
          yaxs = "i",
          xlab = "z",
          ylab = "",
          lwd = 2,
          axes = "F",
          ylim = c(0,0.45),
          cex.lab = 1.5,
          col = alpha("black", 0.25))
    lines(seq(-3,0,0.01),dnorm(seq(-3,0,0.01)),
          lwd = 2,
          col = alpha("black", 1))
    # add axis
    axis(1, at = c(-3, -2.33, -1.64, 0, 3), las=2,
         labels = c("-3", "-2.33", "-1.64", "0", "3"),
         cex.axis = 1.5)
    axis(3, at = c(-3, t.act, 3),
         labels = c("", format(round(t.act, 2), digits = 2, nsmall = 2), ""),
         cex.axis = 1.5, col.axis = "red")
    axis(2, at = seq(0,0.45,0.05), las = 2,
         labels = as.character(seq(0,0.45,0.05)),
         cex.axis = 1.5)
    # shade p-value region in left tail
    if (t.act > -3) {
      polygon(x = c(-3, seq(-3, t.act, 0.01), t.act),
              y = c(0, dnorm(seq(-3, t.act, 0.01)), 0), 
              col = "steelblue")
    }
    # add critical value lines
    abline(v=-1.64, lty=2, lwd=2)
    abline(v=-2.33, lty=2, lwd=2)
    # add actual test value lines
    abline(v=t.act, col="red", lty=2, lwd=2)
    
    # > Two-sided ----
  } else if (test==c("two_sid") && t.act <=0) { 
    # plot the standard normal density on the interval [-6,6]
    curve(dnorm(x),
          xlim = c(-3, 3),
          yaxs = "i",
          xlab = "z",
          ylab = "",
          lwd = 2,
          axes = "F",
          ylim = c(0,0.45),
          cex.lab = 1.5,
          col = alpha("black", 0.25))
    lines(seq(-3,0,0.01),dnorm(seq(-3,0,0.01)),
          lwd = 2,
          col = alpha("black", 1))
    # add x-axis bottom
    axis(1, at = c(-3, -2.58, -1.96, 0), las = 2, # left tail axis crit. values (here relevant!)
         labels = c("","-2.58", "-1.96", "0"),
         cex.axis = 1.5)
    axis(1, at = c(0, 1.96, 2.58, 3), las = 2, # right tail axis crit. values (here not relevant!)
         labels = c("0", "1.96", "2.58", ""),
         cex.axis = 1.5,
         col.axis = alpha("black",0.25))
    
    # add x-axis top
    axis(3, at = c(-3, 0), # left tail (here relevant!)
         labels = c("", ""),
         cex.axis = 1.5)
    axis(3, at = t.act, # left tail actual test statistic (here relevant!)
         labels = format(t.act, nsmall = 2),
         cex.axis = 1.5, col.axis = "red")
    axis(3, at = c(0, 3), # right tail (here not relevant!) 
         labels = c("", ""),
         cex.axis = 1.5,
         col.axis = alpha("red",0.25))
    axis(3, at = -t.act, # right tail actual test statistic (here not relevant!)
         labels = format(-t.act, nsmall = 2),
         cex.axis = 1.5,
         col.axis = alpha("red", 0.25))
    
    axis(2, at = seq(0,0.45,0.05), las=2,
         labels = as.character(seq(0,0.45,0.05)),
         cex.axis = 1.5)
    
    # shade p-value/2 region in left tail
    if (t.act > -3) {
      polygon(x = c(-3, seq(-3, t.act, 0.01), t.act),
              y = c(0, dnorm(seq(-3, t.act, 0.01)),0), 
              col = "steelblue",
              border =  NA)
    }
    # shade p-value/2 region in right tail
    if (-t.act < 3) {
      polygon(x = c(-t.act, seq(-t.act, 3, 0.01), 3),
              y = c(0, dnorm(seq(-t.act, 3, 0.01)), 0), 
              col = alpha("steelblue",0.5),
              border = NA)
    }
    # add critical value lines
    abline(v=-1.96, lty=2, lwd=2)
    abline(v= 1.96, lty=2, lwd=2, col=alpha("black",0.5))
    abline(v=-2.58, lty=2, lwd=2)
    abline(v= 2.58, lty=2, lwd=2, col=alpha("black",0.5))
    # add actual test value lines
    abline(v=-t.act, lty=2, lwd=2, col=alpha("red",0.5))
    abline(v=t.act, col="red", lty=2, lwd=2)
  } else if (test==c("two_sid") && t.act > 0) { # right tail
    # plot the standard normal density on the interval [-6,6]
    curve(dnorm(x),
          xlim = c(-3, 3),
          yaxs = "i",
          xlab = "z",
          ylab = "",
          lwd = 2,
          axes = "F",
          ylim = c(0,0.45),
          cex.lab = 1.5,
          col = alpha("black", 0.25))
    lines(seq(0,3,0.01),dnorm(seq(0,3,0.01)),
          lwd = 2,
          col = alpha("black", 1))
    # add x-axis bottom
    axis(1, at = c(-3, -2.58, -1.96, 0), las=2, # left tail axis crit. values (here not relevant!)
         labels = c("","-2.58", "-1.96", "0"),
         cex.axis = 1.5,
         col.axis = alpha("black",0.25))
    axis(1, at = c(0, 1.96, 2.58, 3), las=2, # right tail axis crit. values (here relevant!)
         labels = c("0", "1.96", "2.58", ""),
         cex.axis = 1.5)
    
    # add x-axis top
    axis(3, at = c(0, 3), # right tail (here relevant!)
         labels = c("", ""),
         cex.axis = 1.5)
    axis(3, at = t.act, # right tail actual test statistic (here relevant!)
         labels = format(t.act, nsmall = 2),
         cex.axis = 1.5, col.axis = "red")
    axis(3, at = c(-3, 0), # left tail (here not relevant!)
         labels = c("", ""),
         cex.axis = 1.5,
         col.axis = alpha("red",0.25))
    axis(3, at = -t.act, # left tail actual test statistic (here not relevant!)
         labels = format(-t.act, nsmall = 2),
         cex.axis = 1.5,
         col.axis = alpha("red", 0.25))
    
    axis(2,at = seq(0,0.45,0.05), las=2,
         labels = as.character(seq(0,0.45,0.05)),
         cex.axis = 1.5)
    
    # shade p-value/2 region in left tail
    if (-t.act > -3) {
      polygon(x = c(-3, seq(-3, -t.act, 0.01), -t.act),
              y = c(0, dnorm(seq(-3, -t.act, 0.01)),0), 
              col = alpha("steelblue",0.5),
              border =  NA)
    }
    # shade p-value/2 region in right tail
    if (t.act < 3) {
      polygon(x = c(t.act, seq(t.act, 3, 0.01), 3),
              y = c(0, dnorm(seq(t.act, 3, 0.01)), 0), 
              col = "steelblue",
              border = NA)
    }
    # add critical value lines
    abline(v=-1.96, lty=2, lwd=2, col=alpha("black",0.5))
    abline(v= 1.96, lty=2, lwd=2)
    abline(v=-2.58, lty=2, lwd=2, col=alpha("black",0.5))
    abline(v= 2.58, lty=2, lwd=2)
    # add actual test value lines
    abline(v=-t.act, lty=2, lwd=2, col=alpha("red",0.5))
    abline(v=t.act, col="red", lty=2, lwd=2)
  }
  
}

# function to generate cdf plot ----
hyp_tes_ill_cdf <- function(t.act, test) {
  
  z <- t.act
  phi.z <- pnorm(t.act)

  # plot parameters
  par(mfrow=c(1,1),
      mar=c(4,3,4,10))
  #bottom, left, top, and right
  
  # > Two-sided ----
  if (test==c("two_sid")) {
    
    # use always the negative absolute value
    z <- -abs(z)
    phi.z <- pnorm(-abs(z))
    
    # plot standard normal cumulative density on the interval [-6,6]
    curve(pnorm(x),
          xlim = c(-3, 3),
          yaxs = "i",
          xlab = "z",
          ylab = "",
          lwd = 2,
          axes = "F",
          ylim = c(0,1),
          cex.lab = 1.5,
          col = alpha("black", 0.25))
    lines(seq(-3, 0, 0.01),pnorm( seq(-3, 0, 0.01)))
    axis(1,at = format(round(c(-3, 0, 3), 2), 2, 2), las = 2,
         # labels = c("", "0", ""),
         cex.axis = 1.5)
    axis(2,at = seq(0,1,0.1), las=2,
         labels = as.character(seq(0,1,0.1)),
         cex.axis = 1.5)
    abline(h=phi.z, col="steelblue", lty=2, lwd=2)
    abline(v=z, col="red", lty=2, lwd=2)
    axis(3,at = c(-3, round(z,3), 3), las=1,
         labels = c("", as.character(round(z,3)), ""),
         cex.axis = 1.5, col.axis = "red")
    axis(4,at = c(round(phi.z,3)), las=1,
         labels = bquote(Phi(.(round(z,3)))== .(round(phi.z,3))),
         cex.axis = 1.5, col.axis = "steelblue")
    
    # > One-sided ----
  } else {
    
    # plot standard normal density on the interval [-6,6]
    curve(pnorm(x),
          xlim = c(-3, 3),
          yaxs = "i",
          xlab = "z",
          ylab = "",
          lwd = 2,
          axes = "F",
          ylim = c(0,1),
          cex.lab = 1.5,
          col = "black")
    axis(1, at = c(-3, 0, 3), las = 2,
         # labels = c("", "0", ""),
         cex.axis = 1.5)
    axis(2, at = seq(0,1,0.1), las=2,
         labels = as.character(seq(0,1,0.1)),
         cex.axis = 1.5)
    abline(h=phi.z, col="steelblue", lty=2, lwd=2)
    abline(v=z, col="red", lty=2, lwd=2)
    axis(3,at = c(-3, round(z,3), 3), las=1,
         labels = c("", as.character(round(z,3)), ""),
         cex.axis = 1.5, col.axis = "red")
    axis(4,at = c(round(phi.z,3)), las=1,
         labels = bquote(Phi(.(round(z,3)))== .(round(phi.z,3))),
         cex.axis = 1.5, col.axis = "steelblue")
    
  }
  
}

# inputs (variable)
tval.vec <- seq(-3,3,0.01)
# test.vec <- c("two_sid", "one_sid_leq", "one_sid_geq")

# inputs (fixed)

# simulation/illustration ----

for (ii in 1:length(tval.vec)) {
  
  # plot no 01: pdf plot (one sided <=) ----
  plt.nam <- paste(fig.dir, "figure_01_", ii, ".svg", sep = "")
  svg(plt.nam)
  
  hyp_tes_ill_pdf(t.act = tval.vec[ii], test = "one_sid_leq")
  
  dev.off()
  
  # plot no 02: cdf plot (one sided <=) ----
  plt.nam <- paste(fig.dir, "figure_02_", ii, ".svg", sep = "")
  svg(plt.nam)
  
  hyp_tes_ill_cdf(t.act = tval.vec[ii], test = "one_sid_leq")
  
  dev.off()
  
  # plot no 03: pdf plot (two sided) ----
  plt.nam <- paste(fig.dir, "figure_03_", ii, ".svg", sep = "")
  svg(plt.nam)
  
  hyp_tes_ill_pdf(t.act = tval.vec[ii], test = "two_sid")
  
  dev.off()
  
  # plot no 04: cdf plot (two sided) ----
  plt.nam <- paste(fig.dir, "figure_04_", ii, ".svg", sep = "")
  svg(plt.nam)
  
  hyp_tes_ill_cdf(t.act = tval.vec[ii], test = "two_sid")
  
  dev.off()
  
  # plot no 05: pdf plot (one sided >=) ----
  plt.nam <- paste(fig.dir, "figure_05_", ii, ".svg", sep = "")
  svg(plt.nam)
  
  hyp_tes_ill_pdf(t.act = tval.vec[ii], test = "one_sid_geq")
  
  dev.off()
  
  # plot no 06: cdf plot (one sided >=) ----
  plt.nam <- paste(fig.dir, "figure_06_", ii, ".svg", sep = "")
  svg(plt.nam)
  
  hyp_tes_ill_cdf(t.act = tval.vec[ii], test = "one_sid_geq")
  
  dev.off()
  
}
  
