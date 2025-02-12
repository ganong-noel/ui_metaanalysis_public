# Load meta studies package
# note to install, you have to run
# install.packages("remotes") if you don't have remotes
# and remotes::install_github("skranz/MetaStudies")
# library(MetaStudies)
# instead, we just manually source the script with the key functions
# we added comments to the script to help ourselves understand it better


#' Perform Andrews and Kasy (2019) estimation
#'
#' @param X vector of reported coefficients
#' @param sigma vector of reported stanard errors
#' @param cutoffs significance thresholds that define intervals of different publication probabilities, e.g. \code{c(1.645, 1.96, 2.576)}.
#' @param symmetric if TRUE assume that positive and negative z-statistics are symmetrically distributed.
#' @param model either "normal" or "t". The assumed functional form of the distribution absent publication bias.
#' @param eval.max,iter.max,abs.tol Control parameters for [stats::nlminb].

metastudies_estimation = function(X,
                                  sigma,
                                  cutoffs = c(1.96),
                                  symmetric = FALSE,
                                  model = "normal",
                                  eval.max = 10 ^ 5,
                                  iter.max = 10 ^ 5,
                                  abs.tol = 10 ^ (-8),
                                  stepsize = 10 ^ (-6),
                                  cluster_ID = 1:nn) {
  #restore.point("metastudies_estimation")
  nn = length(X)


  #%nn x 1 matrix of t-statistics
  TT = X / sigma

  #design matrix with dummies for publication probability region. used as regressors for step function p
  Tpowers = Tpowers_fun(TT, cutoffs, symmetric)

  #computes the likelihood function given the parameters
  if (model == "normal") {
    LLH <-
      function (Psi)
        VariationVarianceLogLikelihood(
          Psi[1], #mean
          Psi[2], #variance
          c(Psi[-c(1, 2)],  1), #publication probabilities
          cutoffs,
          symmetric,
          X,
          sigma,
          Tpowers
        )
    psi_hat0 = c(0, 1, rep(1, length(cutoffs))) #starting values
  } else if (model == "t") {
    LLH <-
      function (Psi)
        VariationVarianceLogLikelihood(Psi[1],
                                       Psi[2],
                                       c(Psi[-c(1, 2, 3)],  1),
                                       cutoffs,
                                       symmetric,
                                       X,
                                       sigma,
                                       Tpowers,
                                       df = Psi[3])
    psi_hat0 = c(0, 1, 10, rep(1, length(cutoffs))) #starting values for minimization
  }

  #the function LLH returns the sum of log L, individual entries, and normalizing constants
  #but to optimize the function we only need to know the first of the three
  LLH_only <- function (Psi) {
    A <- LLH(Psi)
    return(A$LLH)
  }

  lower.b = c(-Inf, rep(0.01, length(psi_hat0) - 1))
  upper.b = rep(Inf, length(psi_hat0))

  #minimize the likelihood function
  find_min <-
    nlminb(
      objective = LLH_only, #object to be minimized
      start = psi_hat0, #initial values
      lower = lower.b, #lowest possible value (-Inf, 0, 0)
      upper = upper.b, #highest possible values (Inf, Inf, Inf)
      control = list(
        eval.max = eval.max,
        iter.max = iter.max,
        abs.tol = abs.tol
      )
    )

  psi_hat <- find_min$par #minimized parameters
  LLHmax <- find_min$objective #minimized value of objective

  #to_print <- LLH(psi_hat)
  #df_to_print <-
  #    tibble(
  #      normalizing_constant = to_print$normalizingconst,
  #      LogL = to_print$logL
  #    )
  #df_to_print |> data.frame() |> print()



  #robust variance and SEs for the parameters
  Var_robust <- RobustVariance(stepsize, nn, psi_hat, LLH, cluster_ID)

  se_robust <- sqrt(diag(Var_robust))


  k = length(cutoffs)
  prob.df = data.frame(
    z.min = c(ifelse(symmetric, 0, -Inf), cutoffs),
    z.max = c(cutoffs, Inf),
    pub.prob = c(psi_hat[(length(psi_hat) - k + 1):(length(psi_hat))], 1)
  )

  dat = data.frame(X = X,
                   sigma = sigma,
                   z = X / sigma)

  if (symmetric) {
    dat$interval.ind = findInterval(abs(dat$z), c(0, cutoffs, Inf))
    dat$pub.prob = prob.df$pub.prob[dat$interval.ind]
  } else {
    dat$interval.ind = findInterval(dat$z, c(-Inf, cutoffs, Inf))
    dat$pub.prob = prob.df$pub.prob[dat$interval.ind]
  }

  res = list(
    psi_hat = psi_hat,
    SE = se_robust,
    X = X,
    sigma = sigma,
    cutoffs = cutoffs,
    model = model,
    symmetric = symmetric,
    psi_vcov = Var_robust,
    prob.df = prob.df,
    est_tab = NA,
    dat = dat
  )
  res$est_tab = estimatestable(res)
  class(res) = c("MetaStudy", "list")
  res
}

print.MetaStudy = function(ms) {
  cat("\nEstimated Metastudy\n")
  str(ms)
}



Tpowers_fun = function(TT, cutoffs, symmetric) {
  n = length(TT)
  Tpowers = matrix (0, n, length(cutoffs) + 1)
  if (symmetric)
    TT = abs(TT)
  Tpowers[, 1] = TT < cutoffs[1]
  if (length(cutoffs) > 1) {
    for (m in 2:length(cutoffs)) {
      Tpowers[, m] = (TT < cutoffs[m]) * (TT >= cutoffs[m - 1])

    }
  }
  Tpowers[, length(cutoffs) + 1] = (TT) >= cutoffs[length(cutoffs)]
  Tpowers
}


#this is the log likehood to be minimized
VariationVarianceLogLikelihood <- function(lambdabar, #estimated param: mean
                                           tauhat,    #estimated param: variance
                                           betap,     #estimated param: publication probabilities
                                           cutoffs,   #researcher choice
                                           symmetric, #researcher choice
                                           X,         #data
                                           sigma,     #data
                                           Tpowers,   #design matrix for pub probabilities
                                           df = Inf) {
  #if df argument is provided, switch to t-dist
  n = length(X)

  #publication probabilities
  betap = as.matrix(betap, length(betap), 1)

  #  %vector of estimated publication probabilities
  phat = Tpowers %*% betap

  #  %%%%%%%%%%%%%%%%%%%%%%%%
  #@  vector of un-truncated likelihoods
  #.    standard deviation incorporating sampling error sigma and param uncertainty tauhat
  #.    this is the key statistic for the "noised up" distribution described
  #.    on page 2775 of the article
  sigmatilde = sqrt(sigma ^ 2 + tauhat ^ 2)

  #fX is the pdf of the normal distribution
  # (X - lambdabar) / sigmatilde is X minus estimated mean / estimated SD
  # dt(., df) is a degrees of freedom adjustment
  # fX is the t-statistic after the df adjustment
  # Note: not totally clear to me why we are dividing by sigmatilde twice
  fX = dt((X - lambdabar) / sigmatilde, df) / sigmatilde


  #  normalizingconstant
  #sigma / sigmatilde is the ratio of the observed standard error in the study
  #to the expected standard deviation also incorporating tauhat.
  #it ranges from 0.2 for the noisiest studies to 0.7 for the least noisy ones
  
  #(AG) scaled lambdabar / sigmatilde to make matrix subtraction conform
  normalizedcutoffs = ((sigma / sigmatilde)  %*% t(cutoffs) -
                         (lambdabar / sigmatilde) %*% rep(1, length(cutoffs)))
  if (symmetric) {
    normalizednegativecutoffs = ((sigma / sigmatilde) %*% t(-cutoffs) - 
                                   (lambdabar / sigmatilde) %*% rep(1, length(cutoffs)))
    cdfs = pt(normalizedcutoffs, df) - pt(normalizednegativecutoffs, df)
  } else {
    cdfs = pt(normalizedcutoffs, df)
  }
  cdfs = cbind(rep(0, n), cdfs, rep(1, n))
  cellprobas = cdfs[, -1] - cdfs[, -(length(cutoffs) + 2)]
  normalizingconst = cellprobas %*% betap


  #likelihood for each observation
  #probability of publication * uncensored normal / normalizing constant
  L <- phat * fX / normalizingconst

  logL <- log(L)

  #total likelihood = L_1*L_2*L_3... up to N
  #log(total likelihood) = sum(LogL_i)
  # objective function; note the sign flip, since we are doing minimization
  LLH <- -sum(logL)


  if (is.nan(LLH)) {
    show(lambdabar)
    show(tauhat)
    show(betap)
  }

  return(list(LLH = LLH, logL = logL, normalizingconst = normalizingconst))

}


estimatestable = function(x = NULL,
                          psi_hat = x$psi_hat,
                          SE = x$SE,
                          cutoffs = x$cutoffs,
                          symmetric = x$symmetric,
                          model = x$model) {
  l = length(psi_hat)
  estimates = matrix(0, 2, l)
  estimates[1, ] = psi_hat
  estimates[2, ] = SE
  rownames(estimates) = c("estimate", "standard error")
  colnames(estimates) = rep(" ", l)
  colnames(estimates)[1] = intToUtf8(956) #mu
  colnames(estimates)[2] = intToUtf8(964) #tau
  if (model == "t") {
    colnames(estimates)[3] = "df"
    shift = 1
  } else {
    shift = 0
  }

  if (symmetric) {
    colnames(estimates)[3 + shift] = paste("[0,", cutoffs[1], "]")
    for (i in seq(2, length(cutoffs), length = max(0, length(cutoffs) - 1))) {
      colnames(estimates)[2 + i + shift] = paste("(", cutoffs[i - 1], ",", cutoffs[i], "]")
    }
  } else {
    colnames(estimates)[3 + shift] = paste("(-", intToUtf8(8734), ",", cutoffs[1], "]")
    for (i in seq(2, length(cutoffs), length = max(0, length(cutoffs) - 1))) {
      colnames(estimates)[2 + i + shift] = paste("(", cutoffs[i - 1], ",", cutoffs[i], "]")
    }
  }
  estimates
}

RobustVariance <- function(stepsize, n, thetahat, LLH, cluster_ID) {
  Info <- matrix(0, length(thetahat), length(thetahat))

  for (n1 in 1:length(thetahat)) {
    for (n2 in 1:length(thetahat)) {
      thetaplusplus <- thetahat

      thetaplusminus <- thetahat

      thetaminusplus <- thetahat

      thetaminusminus <- thetahat




      thetaplusplus[n1] <- thetaplusplus[n1] + stepsize

      thetaplusplus[n2] <- thetaplusplus[n2] + stepsize

      LLH_plusplus <- LLH(thetaplusplus)

      LLH_plusplus <- LLH_plusplus$LLH

      thetaplusminus[n1] <- thetaplusminus[n1] + stepsize

      thetaplusminus[n2] <- thetaplusminus[n2] - stepsize

      LLH_plusminus <- LLH(thetaplusminus)

      LLH_plusminus <- LLH_plusminus$LLH

      thetaminusplus[n1] <- thetaminusplus[n1] - stepsize

      thetaminusplus[n2] <- thetaminusplus[n2] + stepsize

      LLH_minusplus <- LLH(thetaminusplus)

      LLH_minusplus <- LLH_minusplus$LLH

      thetaminusminus[n1] <- thetaminusminus[n1] - stepsize

      thetaminusminus[n2] <- thetaminusminus[n2] - stepsize

      LLH_minusminus <- LLH(thetaminusminus)

      LLH_minusminus <- LLH_minusminus$LLH

      Info[n1, n2] = ((LLH_plusplus - LLH_plusminus) / (2 * stepsize) -
                        (LLH_minusplus - LLH_minusminus) / (2 * stepsize)
      ) / (2 * stepsize)

    }
  }

  Var = solve(Info)

  #show(Var)
  #%Calculate misspecification-robust standard errors
  score_mat <- matrix(0, n, length(thetahat))

  for (n1 in 1:length(thetahat)) {
    theta_plus <- thetahat

    theta_plus[n1] <- theta_plus[n1] + stepsize

    funvalue <- LLH(theta_plus)


    #funvalue<-funvalue$LLH;
    LLH_plus <- funvalue$LLH

    logL_plus <- funvalue$logL


    theta_plus <- thetahat

    theta_plus[n1] <- theta_plus[n1] - stepsize

    funvalue <- LLH(theta_plus)

    # funvalue<-funvalue$LLH;
    LLH_minus <- funvalue$LLH

    logL_minus <- funvalue$logL


    score_mat[, n1] = (logL_plus - logL_minus) / (2 * stepsize)

  }
  Cov = Clustered_covariance_estimate(score_mat, cluster_ID)

  #show(score_mat)
  #show(Cov)
  Var_robust = n * solve(Info) %*% Cov %*% solve(Info)


  return(Var_robust)
}


Clustered_covariance_estimate <- function(g, cluster_index) {
  # %given a matrix of moment condition values g, compute a clustering-robust
  #%estimate of the covariance matrix Sigma
  I <- order(cluster_index)

  cluster_index <- sort(cluster_index)

  g = g[I,]
  g = g - matrix(rep(apply(g, 2, mean), length(I)), nrow = length(I), byrow =
                   TRUE)

  gsum = apply(g, 2, cumsum)

  index_diff <-
    cluster_index[-1] != cluster_index[-length(cluster_index)]

  index_diff <- c(index_diff, 1)


  gsum = gsum[index_diff == 1,]
  gsum = rbind(gsum[1,], diff(gsum))

  Sigma = 1 / (dim(g)[1] - 1) * (t(gsum) %*% gsum)



  return (Sigma)
}
