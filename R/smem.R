#' Select the number of mixture components and estimate the parameters of a
#' Gaussian mixture model
#'
#' This function selects the number of mixture components and estimates the
#' parameters of a Gaussian mixture model using a split-and-merge EM (SMEM)
#' algorithm. At the first iteration, the classic EM algorithm is performed to
#' update the parameters of the initial model. Then each following iteration
#' consists in splitting a component into two or merging two components, before
#' re-estimating the parameters with the EM algorithm. The selected split or
#' merge operation is the one that maximizes a scoring function (after the
#' re-estimation process). To avoid testing all possible operations, the split
#' and merge candidates are initially ranked according to relevant criteria
#' (Zhang \emph{et al.}, 2003). At first, the top-ranked split and top-ranked
#' merge operations are tested. If neither of them increases the score, the
#' second-ranked ones are considered, and so on. The SMEM algorithm stops if a
#' given maximum rank is reached without improving the score.
#'
#' @param gmm An initial object of class \code{gmm}.
#' @param data A data frame or numeric matrix containing the data used in the
#' SMEM algorithm. Its columns must explicitly be named after the variables of
#' \code{gmm} and must not contain missing values.
#' @param y A character vector containing the dependent variables if a
#' conditional model is estimated (which involves maximizing a conditional
#' score). If \code{NULL} (the default), the joint model is estimated.
#' @param score A character string (\code{"aic"}, \code{"bic"} or
#' \code{"loglik"}) corresponding to the scoring function.
#' @param split A logical value indicating whether split operations are allowed
#' (if \code{FALSE}, no mixture component can be split).
#' @param merge A logical value indicating whether merge operations are allowed
#' (if \code{FALSE}, no mixture component can be merged).
#' @param min_comp A positive integer corresponding to the minimum number of
#' mixture components.
#' @param max_comp A positive integer corresponding to the maximum number of
#' mixture components.
#' @param space A numeric value in [0, 1[ corresponding to the space between two
#' subcomponents resulting from a split.
#' @param max_rank A positive integer corresponding to the maximum rank for
#' testing the split and merge candidates.
#' @param max_iter_smem A non-negative integer corresponding to the maximum
#' number of iterations.
#' @param verbose A logical value indicating whether iterations in progress
#' are displayed.
#' @param \dots Additional arguments passed to function \code{\link{em}}.
#'
#' @return A list with elements:
#' \item{gmm}{The final \code{gmm} object.}
#' \item{posterior}{A numeric matrix containing the posterior probabilities for
#' each observation.}
#' \item{seq_score}{A numeric vector containing the sequence of scores measured
#' initially and after each iteration.}
#' \item{seq_oper}{A character vector containing the sequence of split and merge
#' operations performed at each iteration.}
#'
#' @references
#' Zhang, Z., Chen, C., Sun, J. and Chan, K. L. (2003). EM algorithms for
#' Gaussian mixtures with split-and-merge operation. \emph{Pattern Recognition},
#' 36(9):1973--1983.
#'
#' @seealso \code{\link{em}}, \code{\link{stepwise}}
#'
#' @examples
#' data(data_body)
#' gmm_1 <- add_var(NULL, c("WAIST", "AGE", "FAT", "HEIGHT", "WEIGHT"))
#' res_smem <- smem(gmm_1, data_body, max_comp = 3, verbose = TRUE)
#'
#' @export

smem <- function(gmm, data, y = NULL, score = "bic", split = TRUE, merge = TRUE,
                 min_comp = 1, max_comp = Inf, space = 0.5, max_rank = 1,
                 max_iter_smem = 10, verbose = FALSE, ...) {
  if (!is.vector(score, "character")) {
    "score is not a character string" %>%
      stop()
  }

  if (length(score) != 1) {
    "score is not of length 1" %>%
      stop()
  }

  score <- score %>%
    str_to_lower()

  if (score == "aic") {
    f_score <- AIC.gmm
  } else if (score == "bic") {
    f_score <- BIC.gmm
  } else if (score == "loglik") {
    f_score <- logLik.gmm
  } else {
    "score is not \"aic\", \"bic\" or \"loglik\"" %>%
      stop()
  }

  v_score <- gmm %>%
    f_score(object = ., data = data, y = y, ...)

  if (!is.vector(min_comp, "numeric")) {
    "min_comp is not a numeric value" %>%
      stop()
  }

  if (length(min_comp) != 1) {
    "min_comp is not of length 1" %>%
      stop()
  }

  if (!is.finite(min_comp)) {
    "min_comp is not finite" %>%
      stop()
  }

  if (round(min_comp) != min_comp) {
    "min_comp is not an integer" %>%
      stop()
  }

  if (!is.vector(max_comp, "numeric")) {
    "max_comp is not a numeric value" %>%
      stop()
  }

  if (length(max_comp) != 1) {
    "max_comp is not of length 1" %>%
      stop()
  }

  if (is.na(max_comp)) {
    "max_comp is NA" %>%
      stop()
  }

  if (round(max_comp) != max_comp) {
    "max_comp is not an integer" %>%
      stop()
  }

  if (!between(min_comp, 1, max_comp)) {
    "min_comp is not in [1, max_comp]" %>%
      stop()
  }

  if (!between(length(gmm$alpha), min_comp, max_comp)) {
    "the number of components of gmm is not in [min_comp, max_comp]" %>%
      stop()
  }

  if (!is.vector(max_iter_smem, "numeric")) {
    "max_iter_smem is not a numeric value" %>%
      stop()
  }

  if (length(max_iter_smem) != 1) {
    "max_iter_smem is not of length 1" %>%
      stop()
  }

  if (is.na(max_iter_smem)) {
    "max_iter_smem is NA" %>%
      stop()
  }

  if (max_iter_smem < 0) {
    "max_iter_smem is negative" %>%
      stop()
  }

  if (round(max_iter_smem) != max_iter_smem) {
    "max_iter_smem is not an integer" %>%
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

  if (verbose) {
    "iter 0    " %>%
      str_c(score, " = ", v_score, "\n") %>%
      cat()
  }

  posterior <- NULL
  seq_score <- v_score
  seq_oper <- character()

  if (max_iter_smem > 0) {
    if (!is.vector(split, "logical")) {
      "split is not a logical value" %>%
        stop()
    }

    if (length(split) != 1) {
      "split is not of length 1" %>%
        stop()
    }

    if (is.na(split)) {
      "split is NA" %>%
        stop()
    }

    if (!is.vector(merge, "logical")) {
      "merge is not a logical value" %>%
        stop()
    }

    if (length(merge) != 1) {
      "merge is not of length 1" %>%
        stop()
    }

    if (is.na(merge)) {
      "merge is NA" %>%
        stop()
    }

    if (!is.vector(max_rank, "numeric")) {
      "max_rank is not a numeric value" %>%
        stop()
    }

    if (length(max_rank) != 1) {
      "max_rank is not of length 1" %>%
        stop()
    }

    if (is.na(max_rank)) {
      "max_rank is NA" %>%
        stop()
    }

    if (max_rank <= 0) {
      "max_rank is not positive" %>%
        stop()
    }

    if (round(max_rank) != max_rank) {
      "max_rank is not an integer" %>%
        stop()
    }

    var_gmm <- gmm$mu %>%
      rownames()
    data <- data[, var_gmm, drop = FALSE]

    if (verbose) {
      "iter 1" %>%
        cat()
    }

    res_em <- gmm %>%
      em(gmm = ., data = data, verbose = FALSE, ...)
    gmm_em <- res_em$gmm
    v_score_em <- gmm_em %>%
      f_score(object = ., data = data, y = y, ...)

    if (v_score_em >= v_score & length(gmm_em$alpha) >= min_comp) {
      gmm <- gmm_em
      posterior <- res_em$posterior
      v_score <- v_score_em
    }

    seq_score <- seq_score %>%
      c(v_score)
    oper <- "EM"
    seq_oper <- oper
    t_data <- data %>%
      t()
    n_obs <- data %>%
      nrow()
    n_var_gmm <- var_gmm %>%
      length()
    iter <- 1

    while (oper != "none" & iter < max_iter_smem) {
      iter <- iter + 1

      if (verbose) {
        "    " %>%
          str_c(score, " = ", v_score, "    oper = ", oper, "\niter ", iter) %>%
          cat()
      }

      gmm_old <- gmm
      alpha <- gmm$alpha
      mu <- gmm$mu
      sigma <- gmm$sigma
      n_comp <- alpha %>%
        length()
      log_gauss_comp <- n_comp %>%
        seq_len() %>%
        map(function(comp) {
          tc_data <- t_data - mu[, comp]
          chol_sigma <- sigma[[comp]] %>%
            chol()
          return(- sum(log(diag(chol_sigma))) -
                   0.5 * (n_var_gmm * log(2 * pi) +
                            colSums(backsolve(chol_sigma, tc_data,
                                              transpose = TRUE) ^ 2)))
        }) %>%
        do.call("cbind", .)
      log_dens_comp <- log_gauss_comp + matrix(log(alpha), n_obs, n_comp, TRUE)
      max_log_dens_comp <- log_dens_comp %>%
        apply(1, max)
      s_log_dens_comp <- log_dens_comp - max_log_dens_comp
      s_log_dens <- s_log_dens_comp %>%
        exp() %>%
        rowSums() %>%
        log()
      log_post <- s_log_dens_comp - s_log_dens
      max_log_post <- log_post %>%
        apply(2, max)
      s_log_post <- log_post - matrix(max_log_post, n_obs, n_comp, TRUE)

      if (split & n_comp < max_comp) {
        s_log_sum_post <- s_log_post %>%
          exp() %>%
          colSums() %>%
          log()
        log_local_dens <- s_log_post -
          matrix(s_log_sum_post, n_obs, n_comp, TRUE)
        rank_split <- colSums(exp(log_local_dens) *
                                (log_local_dens - log_gauss_comp)) %>%
          desc() %>%
          order()
        n_split <- n_comp
      } else {
        n_split <- 0
      }

      if (merge & n_comp > min_comp) {
        s_log_n_post <- exp(2 * s_log_post) %>%
          colSums() %>%
          log() * 0.5
        rank_merge <- exp(s_log_post -
                            matrix(s_log_n_post, n_obs, n_comp, TRUE)) %>%
          crossprod() %>%
          desc() %>%
          order()
        rank_merge <- c((rank_merge - 1) %% n_comp + 1,
                        (rank_merge - 1) %/% n_comp + 1) %>%
          matrix(ncol = 2)
        rank_merge <- rank_merge[rank_merge[, 1] < rank_merge[, 2], ,
                                 drop = FALSE]
        n_merge <- 0.5 * n_comp * (n_comp - 1)
      } else {
        n_merge <- 0
      }

      max_rank_iter <- n_split %>%
        max(n_merge) %>%
        min(max_rank)
      oper <- "none"
      rank <- 0

      while (oper == "none" & rank < max_rank_iter) {
        rank <- rank + 1

        if (rank <= n_split) {
          comp <- rank_split[rank]
          res_split <- gmm_old %>%
            split_comp(comp, space = space) %>%
            em(gmm = ., data = data, verbose = FALSE, ...)
          gmm_split <- res_split$gmm
          v_score_split <- gmm_split %>%
            f_score(object = ., data = data, y = y, ...)

          if (v_score_split > v_score & length(gmm_split$alpha) >= min_comp) {
            gmm <- gmm_split
            posterior <- res_split$posterior
            v_score <- v_score_split
            oper <- "split " %>%
              str_c(comp, " + EM")
          }
        }

        if (rank <= n_merge) {
          comp <- rank_merge[rank, ]
          res_merge <- gmm_old %>%
            merge_comp(comp) %>%
            em(gmm = ., data = data, verbose = FALSE, ...)
          gmm_merge <- res_merge$gmm
          v_score_merge <- gmm_merge %>%
            f_score(object = ., data = data, y = y, ...)

          if (v_score_merge >= v_score & length(gmm_merge$alpha) >= min_comp) {
            gmm <- gmm_merge
            posterior <- res_merge$posterior
            v_score <- v_score_merge
            oper <- "merge " %>%
              str_c(comp[1], " and ", comp[2], " + EM")
          }
        }
      }

      seq_score <- seq_score %>%
        c(v_score)
      seq_oper <- seq_oper %>%
        c(oper)
    }

    if (verbose) {
      "    " %>%
        str_c(score, " = ", v_score, "    oper = ", oper, "\n") %>%
        cat()
    }
  }

  if (is.null(posterior)) {
    posterior <- gmm %>%
      em(data, max_iter_em = 0) %>%
      .$posterior
  }

  list(gmm = gmm, posterior = posterior, seq_score = seq_score,
       seq_oper = seq_oper) %>%
    return()
}
