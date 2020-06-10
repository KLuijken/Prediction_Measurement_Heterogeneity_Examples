#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Helper function to generate predictive performance plot, adapted from function
# val.prob from rms package
#------------------------------------------------------------------------------#

adjusted_val.prob <- function (p, y, logit, group, weights = rep(1, length(y)), normwt = FALSE, 
                         pl = TRUE, smooth = TRUE, logistic.cal = TRUE, xlab = "Predicted Probability", 
                         ylab = "Actual Probability", lim = c(0, 1), m, g, cuts, emax.lim = c(0, 
                                                                                              1), legendloc = lim[1] + c(0.55 * diff(lim), 0.27 * diff(lim)), 
                         statloc = c(0, 0.99), riskdist = c("predicted", "calibrated"), 
                         cex = 1, mkh = 0.02, connect.group = FALSE, connect.smooth = TRUE, 
                         g.group = 4, evaluate = 100, nmin = 0) 
{
  p <- expit(p)
  
  if (missing(p)) 
    p <- plogis(logit)
  else logit <- qlogis(p)
  if (length(p) != length(y)) 
    stop("lengths of p or logit and y do not agree")
  names(p) <- names(y) <- names(logit) <- NULL
  riskdist <- match.arg(riskdist)
  Spi <- function(p, y) {
    z <- sum((y - p) * (1 - 2 * p))/sqrt(sum((1 - 2 * p) * 
                                               (1 - 2 * p) * p * (1 - p)))
    P <- 2 * pnorm(-abs(z))
    c(Z = z, P = P)
  }
  if (!missing(group)) {
    if (length(group) == 1 && is.logical(group) && group) 
      group <- rep("", length(y))
    if (!is.factor(group)) 
      group <- if (is.logical(group) || is.character(group)) 
        as.factor(group)
    else cut2(group, g = g.group)
    names(group) <- NULL
    nma <- !(is.na(p + y + weights) | is.na(group))
    ng <- length(levels(group))
  }
  else {
    nma <- !is.na(p + y + weights)
    ng <- 0
  }
  logit <- logit[nma]
  y <- y[nma]
  p <- p[nma]
  if (ng > 0) {
    group <- group[nma]
    weights <- weights[nma]
    return(val.probg(p, y, group, evaluate, weights, normwt, 
                     nmin))
  }
  if (length(unique(p)) == 1) {
    P <- mean(y)
    Intc <- qlogis(P)
    n <- length(y)
    D <- -1/n
    L01 <- -2 * sum(y * logit - logb(1 + exp(logit)), na.rm = TRUE)
    L.cal <- -2 * sum(y * Intc - logb(1 + exp(Intc)), na.rm = TRUE)
    U.chisq <- L01 - L.cal
    U.p <- 1 - pchisq(U.chisq, 1)
    U <- (U.chisq - 1)/n
    Q <- D - U
    spi <- unname(Spi(p, y))
    stats <- c(0, 0.5, 0, D, 0, 1, U, U.chisq, U.p, Q, mean((y - 
                                                               p[1])^2), Intc, 0, 0, 0, rep(abs(p[1] - P), 2), spi)
    names(stats) <- c("Dxy", "C (ROC)", "R2", "D", "D:Chi-sq", 
                      "D:p", "U", "U:Chi-sq", "U:p", "Q", "Brier", "Intercept", 
                      "Slope", "Emax", "E90", "Eavg", "S:z", "S:p")
    return(stats)
  }
  i <- !is.infinite(logit)
  nm <- sum(!i)
  if (nm > 0) 
    warning(paste(nm, "observations deleted from logistic calibration due to probs. of 0 or 1"))
  f.fixed <- lrm.fit(logit[i], y[i], initial = c(0, 1), maxit = 1L)
  f.recal <- lrm.fit(logit[i], y[i])
  stats <- f.fixed$stats
  n <- stats["Obs"]
  predprob <- seq(emax.lim[1], emax.lim[2], by = 5e-04)
  Sm <- lowess(p, y, iter = 0)
  cal.smooth <- approx(Sm, xout = p, ties = mean)$y
  er <- abs(p - cal.smooth)
  eavg <- mean(er)
  emax <- max(er)
  e90 <- unname(quantile(er, 0.9))
  if (pl) {
    plot(0.5, 0.5, xlim = lim, ylim = lim, type = "n", xlab = xlab, 
         ylab = ylab, las=1)
    abline(0, 1, lwd = 6, col = gray(0.85))
    lt <- 1
    leg <- "Ideal"
    marks <- -1
    lwd <- 6
    col <- gray(0.85)
    if (logistic.cal) {
      lt <- c(lt, 1)
      leg <- c(leg, "Logistic calibration")
      lwd <- c(lwd, 1)
      col <- c(col, "black")
      marks <- c(marks, -1)
    }
    if (smooth) {
      if (connect.smooth) {
        lines(Sm, lty = 3)
        lt <- c(lt, 3)
        lwd <- c(lwd, 1)
        col <- c(col, "black")
        marks <- c(marks, -1)
      }
      else {
        points(Sm)
        lt <- c(lt, 0)
        lwd <- c(lwd, 1)
        col <- c(col, "black")
        marks <- c(marks, 1)
      }
      leg <- c(leg, "Nonparametric")
    }
    if (!missing(m) | !missing(g) | !missing(cuts)) {
      if (!missing(m)) 
        q <- cut2(p, m = m, levels.mean = TRUE, digits = 7)
      else if (!missing(g)) 
        q <- cut2(p, g = g, levels.mean = TRUE, digits = 7)
      else if (!missing(cuts)) 
        q <- cut2(p, cuts = cuts, levels.mean = TRUE, 
                  digits = 7)
      means <- as.numeric(levels(q))
      prop <- tapply(y, q, function(x) mean(x, na.rm = TRUE))
      points(means, prop, pch = 2)
      if (connect.group) {
        lines(means, prop)
        lt <- c(lt, 1)
      }
      else lt <- c(lt, 0)
      leg <- c(leg, "Grouped observations")
      col <- c(col, "black")
      lwd <- c(lwd, 1)
      marks <- c(marks, 2)
    }
  }
  lr <- stats["Model L.R."]
  p.lr <- stats["P"]
  D <- (lr - 1)/n
  L01 <- -2 * sum(y * logit - logb(1 + exp(logit)), na.rm = TRUE)
  U.chisq <- L01 - f.recal$deviance[2]
  p.U <- 1 - pchisq(U.chisq, 2)
  U <- (U.chisq - 2)/n
  Q <- D - U
  Dxy <- stats["Dxy"]
  C <- stats["C"]
  R2 <- stats["R2"]
  B <- mean((p - y)^2)
  spi <- unname(Spi(p, y))
  stats <- c(Dxy, C, R2, D, lr, p.lr, U, U.chisq, p.U, Q, B, 
             f.recal$coef, emax, e90, eavg, spi)
  names(stats) <- c("Dxy", "C (ROC)", "R2", "D", "D:Chi-sq", 
                    "D:p", "U", "U:Chi-sq", "U:p", "Q", "Brier", "Intercept", 
                    "Slope", "Emax", "E90", "Eavg", "S:z", "S:p")
  if (pl) {
    logit <- seq(-7, 7, length = 200)
    prob <- plogis(logit)
    pred.prob <- f.recal$coef[1] + f.recal$coef[2] * logit
    pred.prob <- plogis(pred.prob)
    if (logistic.cal) 
      lines(prob, pred.prob, lty = 1)
    lp <- legendloc
    if (!is.logical(lp)) {
      if (!is.list(lp)) 
        lp <- list(x = lp[1], y = lp[2])
      legend(lp, leg, lty = lt, pch = marks, cex = cex, 
             lwd = lwd, col = col, bty = "n")
    }
    if (!is.logical(statloc)) {
      dostats <- c("Dxy", "C (ROC)", "R2", "D", "U", "Q", 
                   "Brier", "Intercept", "Slope", "Emax", "E90", 
                   "Eavg", "S:z", "S:p")
      leg <- format(names(stats)[dostats])
      leg <- paste(leg, ":", format(stats[dostats]), sep = "")
      if (!is.list(statloc)) 
        statloc <- list(x = statloc[1], y = statloc[2])
      text(statloc, paste(format(names(stats[dostats])), 
                          collapse = "\n"), adj = c(0, 1), cex = cex)
      text(statloc$x + 0.225 * diff(lim), statloc$y, paste(format(round(stats[dostats], 
                                                                        3)), collapse = "\n"), adj = c(1, 1), cex = cex)
    }
    if (is.character(riskdist)) {
      if (riskdist == "calibrated") {
        x <- f.recal$coef[1] + f.recal$coef[2] * qlogis(p)
        x <- plogis(x)
        x[p == 0] <- 0
        x[p == 1] <- 1
      }
      else x <- p
      bins <- seq(lim[1], lim[2], length = 101)
      x <- x[x >= lim[1] & x <= lim[2]]
      f <- table(cut(x, bins))
      j <- f > 0
      bins <- (bins[-101])[j]
      f <- f[j]
      f <- lim[1] + 0.15 * diff(lim) * f/max(f)
      segments(bins, 0, bins, f)
    }
  }
  stats
}
