#' Estimate the parameters of a Gaussian mixture model
#'
#' This function estimates the parameters of a Gaussian mixture model using the
#' expectation-maximization (EM) algorithm. Given an initial model, this
#' algorithm iteratively updates the parameters, monotonically increasing the
#' log-likelihood until convergence to a local maximum (Bilmes, 1998). A
#' Bayesian regularization is applied by default to prevent that a mixture
#' component comes down to a single point and leads to a zero covariance matrix
#' (Ormoneit and Tresp, 1996). Although the EM algorithm only applies to the
#' joint model, good parameters can be found for a derived conditional model.
#' However, care should be taken as the monotonic increase of the conditional
#' log-likelihood is not guaranteed.
#'
#' @param gmm An initial object of class \code{gmm}.
#' @param data A data frame or numeric matrix containing the data used in the
#' EM algorithm. Its columns must explicitly be named after the variables of
#' \code{gmm} and must not contain missing values.
#' @param regul A positive numeric value corresponding to the regularization
#' constant if a Bayesian regularization is applied. If \code{NULL}, no
#' regularization is applied.
#' @param epsilon A positive numeric value corresponding to the convergence
#' threshold for the increase in log-likelihood.
#' @param max_iter_em A non-negative integer corresponding to the maximum number
#' of iterations.
#' @param verbose A logical value indicating whether iterations in progress
#' are displayed.
#'
#' @return A list with elements:
#' \item{gmm}{The final \code{gmm} object.}
#' \item{posterior}{A numeric matrix containing the posterior probabilities for
#' each observation.}
#' \item{seq_loglik}{A numeric vector containing the sequence of log-likelihoods
#' measured initially and after each iteration.}
#'
#' @references
#' Bilmes, J. A. (1998). A Gentle Tutorial of the EM Algorithm and its
#' Application to Parameter Estimation for Gaussian Mixture and Hidden Markov
#' Models. Technical report, International Computer Science Institute.
#'
#' Ormoneit, D. and Tresp, V. (1996). Improved Gaussian Mixture Density
#' Estimates Using Bayesian Penalty Terms and Network Averaging. In
#' \emph{Advances in Neural Information Processing Systems 8}, pages 542--548.
#'
#' @seealso \code{\link{smem}}, \code{\link{stepwise}}
#'
#' @examples
#' data(data_body)
#' gmm_1 <- split_comp(add_var(NULL,
#'                             data_body[, c("WAIST", "AGE", "FAT", "HEIGHT",
#'                                           "WEIGHT")]),
#'                     n_sub = 3)
#' res_em <- em(gmm_1, data_body, verbose = TRUE)
#'
#' @export

em <- function(gmm, data, regul = 0.01, epsilon = 1e-06, max_iter_em = 100,
               verbose = FALSE) {
  if (!inherits(gmm, "gmm")) {
    "gmm is not of class \"gmm\"" %>%
      stop()
  }

  is_data_frame <- data %>%
    is.data.frame()

  if (!is_data_frame & !is.matrix(data)) {
    "data is not a data frame or numeric matrix" %>%
      stop()
  }

  n_obs <- data %>%
    nrow()

  if (n_obs == 0) {
    "data has no row" %>%
      stop()
  }

  col_data <- data %>%
    colnames()

  if (any(duplicated(col_data))) {
    "data has duplicated column names" %>%
      stop()
  }

  mu <- gmm$mu
  var_gmm <- mu %>%
    rownames()
  n_var_gmm <- var_gmm %>%
    length()

  if (any(!(var_gmm %in% col_data))) {
    "variables of gmm are not column names of data" %>%
      stop()
  }

  data <- data[, var_gmm, drop = FALSE]

  if (is_data_frame) {
    data <- data %>%
      as.matrix()
  }

  if (!is.numeric(data)) {
    "data is not numeric" %>%
      stop()
  }

  if (is.null(regul)) {
    regul <- 0
    is_regul <- 0
  } else {
    if (!is.vector(regul, "numeric")) {
      "regul is not a numeric value" %>%
        stop()
    }

    if (length(regul) != 1) {
      "regul is not of length 1" %>%
        stop()
    }

    if (!is.finite(regul)) {
      "regul is not finite" %>%
        stop()
    }

    if (regul <= 0) {
      "regul is not positive" %>%
        stop()
    }

    is_regul <- 1
  }

  if (!is.vector(epsilon, "numeric")) {
    "epsilon is not a numeric value" %>%
      stop()
  }

  if (length(epsilon) != 1) {
    "epsilon is not of length 1" %>%
      stop()
  }

  if (!is.finite(epsilon)) {
    "epsilon is not finite" %>%
      stop()
  }

  if (epsilon <= 0) {
    "epsilon is not positive" %>%
      stop()
  }

  if (!is.vector(max_iter_em, "numeric")) {
    "max_iter_em is not a numeric value" %>%
      stop()
  }

  if (length(max_iter_em) != 1) {
    "max_iter_em is not of length 1" %>%
      stop()
  }

  if (is.na(max_iter_em)) {
    "max_iter_em is NA" %>%
      stop()
  }

  if (max_iter_em < 0) {
    "max_iter_em is negative" %>%
      stop()
  }

  if (round(max_iter_em) != max_iter_em) {
    "max_iter_em is not an integer" %>%
      stop()
  }

  if (!is.vector(verbose, "logical")) {
    "verbose is not a logical value" %>%
      stop()
  }

  if (length(verbose) != 1) {
    "verbose is not of length 1" %>%
      stop()
  }

  if (is.na(verbose)) {
    "verbose is NA" %>%
      stop()
  }

  t_data <- data %>%
    t()
  regul_sigma <- regul * diag(n_var_gmm)
  alpha <- gmm$alpha
  sigma <- gmm$sigma
  n_comp <- alpha %>%
    length()
  seq_comp <- n_comp %>%
    seq_len()

  if (verbose) {
    "iter 0" %>%
      cat()
  }

  res_iter <- seq_comp %>%
    map(function(comp) {
      tc_data <- t_data - mu[, comp]
      chol_sigma <- sigma[[comp]] %>%
        chol()
      ls_det_sigma <- chol_sigma %>%
        diag() %>%
        log() %>%
        sum()
      log_dens_comp <- log(alpha[comp]) - ls_det_sigma -
        0.5 *
        (n_var_gmm * log(2 * pi) +
           colSums(backsolve(chol_sigma, tc_data, transpose = TRUE) ^ 2))
      penalty <- - is_regul * ls_det_sigma -
        0.5 * regul * sum(diag(chol2inv(chol_sigma)))
      list(log_dens_comp = log_dens_comp, penalty = penalty) %>%
        return()
    }) %>%
    transpose()
  log_dens_comp <- res_iter$log_dens_comp %>%
    do.call("cbind", .)
  max_log_dens_comp <- log_dens_comp %>%
    apply(1, max)
  s_log_dens_comp <- log_dens_comp - max_log_dens_comp
  s_log_dens_comp[max_log_dens_comp == - Inf, ] <- 0
  s_log_dens <- s_log_dens_comp %>%
    exp() %>%
    rowSums() %>%
    log()
  posterior <- exp(s_log_dens_comp - s_log_dens)
  loglik <- s_log_dens %>%
    sum() + sum(max_log_dens_comp) + sum(unlist(res_iter$penalty))
  seq_loglik <- loglik
  diff_loglik <- Inf
  iter <- 0

  while (diff_loglik > epsilon & iter < max_iter_em) {
    iter <- iter + 1

    if (verbose) {
      "    loglik = " %>%
        str_c(loglik, "\niter ", iter) %>%
        cat()
    }

    alpha_old <- alpha
    mu_old <- mu
    sigma_old <- sigma
    posterior_old <- posterior
    loglik_old <- loglik
    sum_post <- posterior %>%
      colSums()
    alpha <- sum_post / n_obs

    if (0 %in% alpha) {
      keep_comp <- alpha > 0
      posterior <- posterior[, keep_comp, drop = FALSE]
      sum_post <- sum_post[keep_comp]
      alpha <- alpha[keep_comp]
      n_comp <- alpha %>%
        length()
      seq_comp <- n_comp %>%
        seq_len()
    }

    mu <- (t_data %*% posterior) / matrix(sum_post, n_var_gmm, n_comp, TRUE)
    res_iter <- seq_comp %>%
      map(function(comp) {
        tc_data <- t_data - mu[, comp]
        twc_data <- tc_data *
          matrix(sqrt(posterior[, comp]), n_var_gmm, n_obs, TRUE)
        sigma <- (tcrossprod(twc_data) + regul_sigma) /
          (sum_post[comp] + is_regul)
        chol_sigma <- tryCatch(chol(sigma), error = function(e) {return(e)})

        if (inherits(chol_sigma, "error")) {
          error <- "sigma[[" %>%
            str_c(comp, "]] is not positive definite")

          if (is_regul == 0) {
            error <- error %>%
              str_c(" (use argument regul to avoid singularities)")
          }

          error %>%
            stop()
        }

        ls_det_sigma <- chol_sigma %>%
          diag() %>%
          log() %>%
          sum()
        log_dens_comp <- log(alpha[comp]) - ls_det_sigma -
          0.5 *
          (n_var_gmm * log(2 * pi) +
             colSums(backsolve(chol_sigma, tc_data, transpose = TRUE) ^ 2))
        penalty <- - is_regul * ls_det_sigma -
          0.5 * regul * sum(diag(chol2inv(chol_sigma)))
        list(sigma = sigma, log_dens_comp = log_dens_comp,
             penalty = penalty) %>%
          return()
      }) %>%
      transpose()
    sigma <- res_iter$sigma
    log_dens_comp <- res_iter$log_dens_comp %>%
      do.call("cbind", .)
    max_log_dens_comp <- log_dens_comp %>%
      apply(1, max)
    s_log_dens_comp <- log_dens_comp - max_log_dens_comp
    s_log_dens_comp[max_log_dens_comp == - Inf, ] <- 0
    s_log_dens <- s_log_dens_comp %>%
      exp() %>%
      rowSums() %>%
      log()
    posterior <- exp(s_log_dens_comp - s_log_dens)
    loglik <- s_log_dens %>%
      sum() + sum(max_log_dens_comp) + sum(unlist(res_iter$penalty))
    seq_loglik <- seq_loglik %>%
      c(loglik)
    diff_loglik <- loglik - loglik_old
  }

  if (diff_loglik < 0) {
    alpha <- alpha_old
    mu <- mu_old
    sigma <- sigma_old
    posterior <- posterior_old
    seq_loglik[iter + 1] <- loglik_old
  }

  rownames(posterior) <- data %>%
    rownames()

  if (verbose) {
    "    loglik = " %>%
      str_c(seq_loglik[iter + 1], "\n") %>%
      cat()
  }

  list(gmm = gmm(alpha, mu, sigma), posterior = posterior,
       seq_loglik = seq_loglik) %>%
    return()
}
