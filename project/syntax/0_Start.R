#' ---
#' title: Collection of functions for analyses
#' author: Timo Gnambs
#' output: 
#'    html_document: 
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---


# load packages
library(Hmisc)
library(lme4)
library(weights)
library(psych)



#
# Correlation matrix from imputed dataset
#
# Requires:
#   package "weights"
#
# Args:
#   obj:   a list of imputed datasets
#   items: a vector of variables names included in obj (optional)
#          if missing, correlations between all variables are calculated
#   weights: a variable name included in obj indicating the sampling weights (optional)
#            if missing, unweighted correlations are calculated
#   subset: a logical vector indicating a subset of respondents (optional)
#   digits: a numeric vector indicating the rounding precision
#   print: a logical vector indicating whether to print the results
#
# Returns:
#   A correlation matrix
#
cor.imp <- function(obj, items = NULL, weights = NULL, 
                    subset = NULL, digits = 2, print = TRUE) {
    
    if (is.null(items)) items <- colnames(obj[[1]])
    if (is.null(subset)) subset <- rep(TRUE, nrow(obj[[1]]))
    if (missing(weights)) {
        weights <- rep(1, sum(subset))
    } else {
        weights <- obj[[1]][subset, weights]
    }
    
    # correlation matrices within each sample
    rmat <- semat <- array(0, dim = c(length(items), length(items), length(obj)))
    for (i in seq_len(length(obj))) {
        #d <- na.omit(obj[[i]][subset, items])
        r <- suppressWarnings(wtd.cor(obj[[i]][subset, items], weight = weights))
        rmat[, , i] <- r$'correlation'
        semat[, , i] <- r$std.err
    }
    
    out <- list(r = array(NA, dim = dim(rmat)[-3]),
                t = array(NA, dim = dim(rmat)[-3]),
                df = array(NA, dim = dim(rmat)[-3]),
                p = array(NA, dim = dim(rmat)[-3]))
    for (i in seq(1, length(items) )) {
        for (j in seq(1, length(items))) {
            if (i == j) next
            est <- testEstimates(qhat = rmat[i, j, ], uhat = semat[i, j, ]^2)
            out$r[i, j] <- out$r[j, i]   <- est$estimates[1, "Estimate"]
            out$t[i, j] <- out$t[j, i]   <- est$estimates[1, "t.value"]
            out$df[i, j] <- out$df[j, i] <- est$estimates[1, "df"]
            out$p[i, j] <- out$p[j, i]   <- est$estimates[1, "P(>|t|)"]
        }
    }
    
    colnames(out$r) <- colnames(out$p) <- colnames(out$t) <- colnames(out$df) <-
        rownames(out$r) <- rownames(out$p) <- rownames(out$t) <- rownames(out$df) <-
        items
    
    if (print) {
        cat("Pearson correlations:\n")
        print(round(out$r, digits), digits = digits)
        cat("\nt-values:\n")
        print(round(out$t, digits), digits = digits)
        cat("\nDegrees of freedom:\n")
        print(out$df, digits = digits)
        cat("\nP-values:\n")
        print(round(out$p, digits), digits = digits)
    }
    invisible(out)
}




#
# Descriptive statistics from imputed dataset
#
# Requires:
#   package "Hmisc"
#
# Args:
#   obj:   a list of imputed datasets
#   items: a vector of variables names included in obj (optional)
#          if missing, correlations between all variables are calculated
#   weights: a variable name included in obj indicating the sampling weights (optional)
#            if missing, unweighted correlations are calculated
#   subset: a logical vector indicating a subset of respondents (optional)
#   digits: a numeric vector indicating the rounding precision
#   stats:  a character vector indicating the summary statistics to compute
#
# Returns:
#   A matrix with descriptive statistics
#
describe.imp <- function(obj, items = NULL, weights = NULL, 
                         subset = NULL, digits = 2,
                         stats = c("mean", "sd", "median", 
                                   "min", "max")) {
    
    if (is.null(items)) items <- colnames(obj[[1]])
    if (is.null(subset)) subset <- rep(TRUE, nrow(obj[[1]]))
    if (is.null(weights)) {
        weights <- rep(1, sum(subset))
    } else {
        weights <- obj[[1]][subset, weights]
    }
    out <- c()
    
    # means
    if ("mean" %in% stats) {
        out <- rowMeans(sapply(obj, function(x) { 
                            sapply(items,function(y) { 
                                wtd.mean(x[subset, y], weights = weights) 
                            }) 
                }))
    }
    
    # standard deviations
    if ("sd" %in% stats) {
        out <- cbind(out, 
                     sqrt(rowMeans(sapply(obj, function(x) { 
                                  sapply(items,function(y) { 
                                      wtd.var(x[subset, y], weights = weights)
                                  }) 
                     }))))
    }
    
    # median
    if ("median" %in% stats) {
        out <- cbind(out,
                     rowMeans(sapply(obj, function(x) { 
                                  sapply(items,function(y) { 
                                      wtd.quantile(x[subset, y], 
                                                   probs = .50, weights = weights) 
                                  }) 
                     })))
    }
    
    # minimum
    if ("min" %in% stats) {
        out <- cbind(out,
                     rowMeans(sapply(obj, function(x) { 
                                  sapply(items,function(y) { 
                                      min(x[subset, y], na.rm = TRUE) 
                                  }) 
                     })))
    }
    
    # maximum
    if ("max" %in% stats) {
        out <- cbind(out,
                     rowMeans(sapply(obj, function(x) { 
                                  sapply(items,function(y) { 
                                      max(x[subset, y], na.rm = TRUE) 
                                  }) 
                     })))
    }

    out <- cbind(seq_len(length(items)), 
                 rep(sum(subset), length(items)), 
                 out)
    colnames(out) <- c("vars", "n", stats)
    rownames(out) <- items
    round(out, digits)
}




#
# T-test from imputed dataset
#
# Requires:
#   package "weights"
#
# Args:
#   obj:   a list of imputed datasets
#   items: a vector of variables names included in obj (optional)
#          if missing, correlations between all variables are calculated
#   weights: a variable name included in obj indicating the sampling weights (optional)
#            if missing, unweighted correlations are calculated
#   subset: a logical vector indicating a subset of respondents (optional)
#   digits: a numeric vector indicating the rounding precision
#   print: a logical vector indicating whether to print the results
#
# Returns:
#   A matrix with descriptive statistics
#
ttest.imp <- function(frm, data, weights = NULL, subset = NULL,
                      paired = FALSE, digits = 2,
                      mu = NULL, print = TRUE) {

    if (is.null(subset)) subset <- rep(TRUE, nrow(data[[1]]))
    if (is.null(weights)) {
        data <- lapply(data, function(x) {
                    x$weights <- 1
                    x
                })
        weights <- "weights"
    }
    
    vars <- attr(terms(frm), "variables")
    if (is.null(mu)) {
        vars <- c(as.character(vars[[2]]), as.character(vars[[3]]))
    } else {
        vars <- as.character(vars[[2]])
    }

    tt <- t(sapply(data, function(x) {
        
        # one sample t-test
        if (!is.null(mu)) {
            grp1 <- x[[vars[1]]][subset]
            grp2 <- mu
            wgt1 <- wgt2 <- x[[weights]][subset]
            
        # two paired samples t-test
        } else if (paired) {
            grp1 <- x[[vars[1]]][subset]
            grp2 <- x[[vars[2]]][subset]
            wgt1 <- wgt2 <- x[[weights]][subset]
            
        # two independent samples t-test
        } else {
            if (length(unique(x[subset, vars[2]])) != 2) 
                stop("Grouping variable must have 2 values.")
            o <- split(x[subset, ], x[subset, vars[2]])
            grp1 <- o[[1]][[vars[1]]]
            grp2 <- o[[2]][[vars[1]]]
            wgt1 <- o[[1]][[weights]]
            wgt2 <- o[[2]][[weights]]
        }
        fit <- wtd.t.test(x = grp1, y = grp2, weight = wgt1, weighty = wgt2, 
                          mean1 = FALSE, samedata = FALSE, drops = "all")
        c(fit$additional[c(1, 4)],
          wtd.mean(grp1, weights = wgt1),
          wtd.mean(grp2, weights = wgt2),
          wtd.var(grp1, weights = wgt1),
          wtd.var(grp2, weights = wgt2),
          sum(!is.na(grp1)),
          sum(!is.na(grp2)))
    }))
    out <- list(est = testEstimates(qhat = tt[, 1], uhat = tt[, 2]^2))
    
    # Cohen's d
    if (is.null(mu)) {
        out$d <- diff(colMeans(tt[, 4:3])) / sqrt(mean(colMeans(tt[, 5:6])))
    } else {
        out$d <- (mean(tt[, 3]) - mu) / sqrt(mean(tt[, 5]))
    }

    # print result
    if (print) {
        if (is.null(mu)) {
            cat("Two Sample Weighted t-test (Welch)\n\n")
            cat("data:", vars[1], "by", vars[2])
        } else {
            cat("One Sample Weighted t-test (Welch)\n\n")
            cat("data:", vars[1], "against", mu)
        }
        print(out$est)
        cat("\nalternative hypothesis: true difference in means is not equal to 0")
        cat("\n\nsample estimates:\n")
        mat <- rbind(colMeans(tt[, 3:4]), 
                     sqrt(colMeans(tt[, 5:6])), 
                     colMeans(tt[, 7:8]))
        rownames(mat) <- c("M", "SD", "N")
        colnames(mat) <- paste0("group ", 1:2)
        print(round(mat, digits))
        cat("\nCohen's d = ", round(out$d, digits))
    }
    invisible(out)
}




#
# Omega reliability from imputed dataset
#
# Requires:
#   package "psych"
#
# Args:
#   obj:   a list of imputed datasets
#   items: a vector of variables names included in obj (optional)
#          if missing, correlations between all variables are calculated
#   weights: a variable name included in obj indicating the sampling weights (optional)
#            if missing, unweighted correlations are calculated
#   subset: a logical vector indicating a subset of respondents (optional)
#   poly: a logical vector indicating if ordered responses should be modeled
#   print: a logical vector indicating whether to print the results
#   digits: a numeric vector indicating the rounding precision
#
# Returns:
#   Reliability estimate
#
omg.imp <- function(obj, items = NULL, weights = NULL,
                    subset = NULL, poly = FALSE,
                    print = TRUE, digits = 2) {
    
    if (is.null(items)) items <- colnames(obj[[1]])
    if (is.null(subset)) subset <- rep(TRUE, nrow(obj[[1]]))
    if (missing(weights)) {
        obj <- within(obj, { 
            weights <- 1 
        })
        weights <- "weights"
    } 

    if (poly) {
        rmat <- lapply(obj, function(x) {
            mixedCor(x[subset, ], p = items, 
                     weight = x[subset, weights])$poly$rho
        })
    } else {
        rmat <- lapply(obj, function(x) {
            mixedCor(x[subset, ], c = items, 
                     weight = x[subset, weights])$poly$rho
        })
    }
    mat <- Reduce("+", rmat) / length(rmat)
    mat <- cor.smooth(mat) # smooth non-positive definite matrices
    
    # omega
    fit <- fa(mat, 1, n.obs = sum(subset), fm = "ml")
    lds <- c(loadings(fit)[])
    omg <- (sum(abs(lds))^2) / (sum(abs(lds))^2 + sum(1 - lds^2))
    
    if (print) {
        cat("\n\nOmega reliability:\n")
        print(omg, digits = digits)
    }
    invisible(omg)
}



#
# Standardized mixed-effects regression weights
#
# Source:
#   adapted from https://stackoverflow.com/questions/25142901/standardized-coefficients-for-lmer-model
#
# Args:
#   object:   lmer object
#   stdy:     logical vector indicating whether to standardize using the 
#             dependent variable
#   stdx:     logical vector indicating whether to standardize using the 
#             independent variables
#   se:       logical vector indicating whether to standardize standard errors
#
# Returns:
#   data.frame with standardized regression coefficients
#
stdCoef.lmer <- function(object, stdy = TRUE, stdx = TRUE, se = FALSE) {
    
    # regression weights
    sdy <- ifelse(stdy, sd(attr(object, "resp")$y), 1) # the y values are now in the 'y' slot 
    ###                                                  of the resp attribute
    if (stdx) {
        sdx <- apply(attr(object, "pp")$X, 2, sd) # And the X matrix is in the 'X' slot of the pp attr
    } else {
        sdx <- 1
    }
    stdcoef <- fixef(object) * sdx / sdy
    
    # standard errors
    if (se) {
        #mimic se.ranef from pacakge "arm"
        se.fixef <- function(obj) as.data.frame(summary(obj)[10])[, 2] # last change - extracting 
        ##             the standard errors from the summary
        stdse <- se.fixef(object) * sdx / sdy
    } else {
        stdse <- NA
    }
    
    out <- data.frame(stdcoef = stdcoef)
    if (se) out$stdse <- stdse
    return(out)
}




#
# Standardized coefficients for lmer from imputed dataset
#
# Args:
#   obj:   a list of lmer results
#   ...:   optional arguments passed to stdCoef.lmer()
#
# Returns:
#   A vector with standardized regression weights
#
stdCoef <- function(obj, ...) {
    out <- rowMeans(sapply(obj, function(x) { stdCoef.lmer(x, ...)$stdcoef }))
    names(out) <- colnames(attr(obj[[1]], "pp")$X)
    out
}




#
# Mixed-effects regression analyses for imputed dataset
#
# Requires:
#   lme4
#
# Args:
#   frm:      formula for lmer regression
#   dati:     list with imputed data
#   stdx:     logical vector indicating whether to standardize using the 
#             independent variables
#   stdy:     logical vector indicating whether to standardize using the 
#             dependent variables
#   weights:  name of weight variable
#   subset:   a logical vector indicating a subset of respondents (optional)
#   print:    a logical vector indicating whether to print the results
#   control:  a list as lmerControl() passed to lmer()
#
# Returns:
#   void
#
lmer.imp <- function(frm, data, weights = NULL, subset = NULL,
                     stdy = FALSE, stdx = FALSE, print = TRUE,
                     control = lmerControl()) {

    if (is.null(subset)) subset <- rep(TRUE, nrow(data[[1]]))
    if (is.null(weights)) {
        data <- within(data, {
            weights <- 1
        })
        weights <- "weights"
    }
    
    # fit regression models and return parameter estimates
    d <- c()
    for (i in seq_len(length(data))) {
        f <- do.call("lmer", list(formula = frm, 
                                  data = data[[i]], 
                                  weights = data[[i]][, weights],
                                  subset = subset,
                                  control = control))
        qhat <- fixef(f)
        uhat <- diag(vcov(f))
        varcomp <- as.data.frame(VarCorr(f))[, "vcov"]
        if (stdx | stdy) {
            std <- stdCoef.lmer(f, stdx = stdx, stdy = stdy)
            d <- cbind(d, c(qhat, uhat, std[, 1], varcomp))
        } else {
            d <- cbind(d, c(qhat, uhat, NA, varcomp))
        }
    }

    # number of parameters
    k <- (nrow(d) - 2) / 3

    # variance components
    r <- rowMeans(d[1:2 + k * 3, ])
    r <- c(r, r[1] / sum(r))
    names(r) <- c("Intercept", "Residual", "ICC")
    
    # pooled results
    out <- list(est = testEstimates(qhat = d[seq_len(k), ], 
                                    uhat = d[seq_len(k) + k, ]),
                r = r)
    
    # standardized results
    if (stdx | stdy) {
        out$std <- rowMeans(d[seq_len(k) + 2 * k, ])  # standardized results
        names(out$std) <- rownames(d[seq_len(k), ])
    }
    # print results
    if (print) {
        print(out$est)
        if (stdx | stdy) {
            cat("Standardized estimates:\n")
            print(out$std, digits = 3)
            cat("\n")
        }
        cat("Variance components:\n")
        print(out$r, digits = 3)
    }
    
    # return results
    invisible(out)
}




# R Function for Calculating the Estimated Probabilities for Choosing Each Response Category for Each Indicator
#
# @source Liu, Y., Millsap, R. E., West, S. G., Tein, J.-Y., Tanaka, R., &
#                  Grimm, K. J. (2017). Testing Measurement Invariance 
#                  in Longitudinal Data With Ordered-Categorical Measures. 
#                  Psychological Methods, 22, 486-506. 
#                  http://dx.doi.org/10.1037/met0000075
#
# Takes in output from the baseline/ loading invariance/ threshold invariance/ unique factor 
#   invariance model to evaluate the longitudinal measurement equivalence of a one-factor
#   ordered-categorical CFA model with theta parameterization.
# Calculates the predicted probabilities of choosing each response category of each indicator
#   at ONE measurement occasion. The resulting probability matrix has rows representing indicators #   and columns representing response categories.
# Assumes that unique factors within a measurement occasion are uncorrelated.
# The function can handle cases where some indicators have one less threshold than others;
#   however, it currently cannot handle cases where the discrepancy in the number of response 
#   categories across different indicators is greater than one;
######################################################################################

ThresholdProbability <- function(n.Item, Loadings, Common.Factor.Mean, 
                                 Common.Factor.Variance, Unique.Factor.Variances, 
                                 n.Threshold, Thresholds) {
    
    # Vector of loadings
    LAMDA <- matrix(Loadings,n.Item,1)  
    
    # Common factor mean
    KAPPA <- Common.Factor.Mean
    
    # Common factor variance
    PHI <- Common.Factor.Variance
    
    # Covariance matrix for the unique factors of indicators that load on C1FAMO
    THETA <- diag(Unique.Factor.Variances)
    
    # Latent response variable means
    MU <- LAMDA%*%KAPPA
    
    # Latent response variable covariance matrix.
    SIGMA <- (LAMDA%*%PHI%*%t(LAMDA) + THETA)
    
    # Latent response variable variances 
    Item.Variances <- diag(SIGMA)
    Item.SDs <- sqrt(Item.Variances)
    
    # Declare the matrices storing the p values
    # p values associated with scoring under a certain threshold
    p.LE.Threshold<-matrix(NA,nrow=n.Item, ncol=n.Threshold) 
    # p values associated with scoring within certain thresholds, i.e., choosing a category
    p.within.Thresholds<-matrix(NA,nrow=n.Item, ncol=n.Threshold+1)  
    
    # Calculation of p values
    for (i in 1:(n.Item)){
        for (j in 1:(n.Threshold)){
            p.LE.Threshold[i,j]<-pnorm(Thresholds[i,j], mean=MU[i], sd=Item.SDs[i])
        }
    }
    
    for (i in 1:(n.Item)){
        p.within.Thresholds[i,1]<-p.LE.Threshold[i,1]
        if (is.na(p.LE.Threshold[i,n.Threshold])){
            p.within.Thresholds[i,n.Threshold+1]<- NA
            p.within.Thresholds[i,n.Threshold]<- (1-p.LE.Threshold[i,n.Threshold-1])
            
            for (j in 2:(n.Threshold-1)){
                p.within.Thresholds[i,j]<-(p.LE.Threshold[i,j]-p.LE.Threshold[i,j-1])
            }
        }
        else {
            p.within.Thresholds[i,n.Threshold+1]<- (1-p.LE.Threshold[i,n.Threshold])
            for (j in 2:(n.Threshold)){
                p.within.Thresholds[i,j]<-(p.LE.Threshold[i,j]-p.LE.Threshold[i,j-1])
            }
        }
    }
    colnames(p.within.Thresholds) <- c(paste0('C',0:(n.Threshold)))
    rownames(p.within.Thresholds) <- c(paste0('V',1:(n.Item)))
    return(p.within.Thresholds)
}
