#' Learn the structure and the parameters of a Gaussian mixture graphical model
#' with incomplete data
#'
#' This function learns the structure and the parameters of a Gaussian mixture
#' graphical model with incomplete data using the structural EM algorithm. At
#' each iteration, the parametric EM algorithm is performed to complete the data
#' and update the parameters (E step). The completed data are then used to
#' update the structure (M step), and so on. Each iteration is guaranteed to
#' increase the scoring function until convergence to a local maximum (Koller
#' and Friedman, 2009). In practice, due to the sampling process inherent in
#' particle-based inference, it may happen that the monotonic increase no longer
#' occurs when approaching the local maximum, resulting in an earlier
#' termination of the algorithm.
#'
#' @param gmgm An object of class \code{gmbn} (non-temporal) or \code{gmdbn}.
#' @param data A data frame containing the data used for learning. Its columns
#' must explicitly be named after nodes of \code{gmgm} and can contain missing
#' values (columns with no value can be removed).
#' @param nodes A character vector containing the nodes whose local conditional
#' models are learned (by default all the nodes of \code{gmgm}). If \code{gmgm}
#' is a \code{gmdbn} object, the same nodes are learned for each of its
#' \code{gmbn} elements. This constraint can be overcome by passing a list of
#' character vectors named after some of these elements (\code{b_1}, \dots) and
#' containing learned nodes specific to them.
#' @param arcs_cand A data frame containing the candidate arcs for addition or
#' removal (by default all possible non-temporal arcs). The column \code{from}
#' describes the start node, the column \code{to} the end node and the column
#' \code{lag} the time lag between them. Missing values in \code{from} or
#' \code{to} are interpreted as "all possible nodes", which allows to quickly
#' define large set of arcs that share common attributes. Missing values in
#' \code{lag} are replaced by 0. If \code{gmgm} is a \code{gmdbn} object, the
#' same candidate arcs are used for each of its \code{gmbn} elements. This
#' constraint can be overcome by passing a list of data frames named after some
#' of these elements (\code{b_1}, \dots) and containing candidate arcs specific
#' to them. If arcs already in \code{gmgm} are not candidates, they cannot be
#' removed. Therefore, setting \code{arcs_cand} to \code{NULL} is equivalent to
#' learning only the mixture structure (and the parameters) of the model.
#' @param col_seq A character vector containing the column names of \code{data}
#' that describe the observation sequence. If \code{NULL} (the default), all the
#' observations belong to a single sequence. If \code{gmgm} is a \code{gmdbn}
#' object, the observations of a same sequence must be ordered such that the
#' \eqn{t}th one is related to time slice \eqn{t} (note that the sequences can
#' have different lengths). If \code{gmgm} is a \code{gmbn} object, this
#' argument is ignored.
#' @param score A character string (\code{"aic"}, \code{"bic"} or
#' \code{"loglik"}) corresponding to the scoring function.
#' @param n_part A positive integer corresponding to the number of particles
#' generated for each observation (if \code{gmgm} is a \code{gmbn} object) or
#' observation sequence (if \code{gmgm} is a \code{gmdbn} object) during
#' inference.
#' @param max_part_sim An integer greater than or equal to \code{n_part}
#' corresponding to the maximum number of particles that can be processed
#' simultaneously during inference. This argument is used to prevent memory
#' overflow, dividing \code{data} into smaller subsets that are handle
#' sequentially.
#' @param min_ess A numeric value in [0, 1] corresponding to the minimum ESS
#' (expressed as a proportion of \code{n_part}) under which the renewal step of
#' sequential importance resampling is performed. If \code{1} (the default),
#' this step is performed at each time slice. If \code{gmgm} is a \code{gmbn}
#' object, this argument is ignored.
#' @param max_iter_sem A non-negative integer corresponding to the maximum
#' number of iterations.
#' @param max_iter_pem A non-negative integer corresponding to the maximum
#' number of iterations of the parametric EM algorithm.
#' @param verbose A logical value indicating whether iterations in progress
#' are displayed.
#' @param \dots Additional arguments passed to function \code{\link{stepwise}}.
#'
#' @return A list with elements:
#' \item{gmgm}{The final \code{gmbn} or \code{gmdbn} object (with the highest
#' score).}
#' \item{data}{A data frame (tibble) containing the complete data used to learn
#' the final \code{gmbn} or \code{gmdbn} object.}
#' \item{seq_score}{A numeric matrix containing the sequence of scores measured
#' after the E and M steps of each iteration.}
#'
#' @references
#' Koller, D. and Friedman, N. (2009). \emph{Probabilistic Graphical Models:
#' Principles and Techniques}. The MIT Press.
#'
#' @seealso \code{\link{param_em}}, \code{\link{param_learn}},
#' \code{\link{struct_learn}}
#'
#' @examples
#' \donttest{
#' set.seed(0)
#' data(data_body)
#' data_1 <- data_body
#' data_1$GENDER[sample.int(2148, 430)] <- NA
#' data_1$AGE[sample.int(2148, 430)] <- NA
#' data_1$HEIGHT[sample.int(2148, 430)] <- NA
#' data_1$WEIGHT[sample.int(2148, 430)] <- NA
#' data_1$FAT[sample.int(2148, 430)] <- NA
#' data_1$WAIST[sample.int(2148, 430)] <- NA
#' data_1$GLYCO[sample.int(2148, 430)] <- NA
#' gmbn_1 <- add_nodes(NULL,
#'                     c("AGE", "FAT", "GENDER", "GLYCO", "HEIGHT", "WAIST",
#'                       "WEIGHT"))
#' arcs_cand_1 <- data.frame(from = c("AGE", "GENDER", "HEIGHT", "WEIGHT", NA,
#'                                    "AGE", "GENDER", "AGE", "FAT", "GENDER",
#'                                    "HEIGHT", "WEIGHT", "AGE", "GENDER",
#'                                    "HEIGHT"),
#'                           to = c("FAT", "FAT", "FAT", "FAT", "GLYCO", "HEIGHT",
#'                                  "HEIGHT", "WAIST", "WAIST", "WAIST", "WAIST",
#'                                  "WAIST", "WEIGHT", "WEIGHT", "WEIGHT"))
#' res_learn_1 <- struct_em(gmbn_1, data_1, arcs_cand = arcs_cand_1,
#'                          verbose = TRUE, max_comp = 3, max_rank = 1,
#'                          regul = 0.01, max_iter_em = 100)
#'
#' set.seed(0)
#' data(data_air)
#' data_2 <- data_air
#' data_2$NO2[sample.int(7680, 1536)] <- NA
#' data_2$O3[sample.int(7680, 1536)] <- NA
#' data_2$TEMP[sample.int(7680, 1536)] <- NA
#' data_2$WIND[sample.int(7680, 1536)] <- NA
#' gmdbn_1 <- gmdbn(b_2 = add_nodes(NULL, c("NO2", "O3", "TEMP", "WIND")),
#'                  b_13 = add_nodes(NULL, c("NO2", "O3", "TEMP", "WIND")))
#' arcs_cand_2 <- data.frame(from = c("NO2", "NO2", "NO2", "O3", "TEMP", "TEMP",
#'                                    "WIND", "WIND"),
#'                           to = c("NO2", "O3", "O3", "O3", NA, NA, NA, NA),
#'                           lag = c(1, 0, 1, 1, 0, 1, 0, 1))
#' res_learn_2 <- struct_em(gmdbn_1, data_2, arcs_cand = arcs_cand_2,
#'                          col_seq = "DATE", verbose = TRUE, max_comp = 3,
#'                          max_rank = 1, regul = 0.01, max_iter_em = 100)}
#'
#' @export

struct_em <- function(gmgm, data, nodes = structure(gmgm)$nodes,
                      arcs_cand = tibble(lag = 0), col_seq = NULL,
                      score = "bic", n_part = 1000, max_part_sim = 1e06,
                      min_ess = 1, max_iter_sem = Inf, max_iter_pem = Inf,
                      verbose = FALSE, ...) {
  is_gmbn <- gmgm %>%
    inherits("gmbn")

  if (!is_gmbn & !inherits(gmgm, "gmdbn")) {
    "gmgm is not of class \"gmbn\" or \"gmdbn\"" %>%
      stop()
  }

  if (!is.vector(max_iter_sem, "numeric")) {
    "max_iter_sem is not a numeric value" %>%
      stop()
  }

  if (length(max_iter_sem) != 1) {
    "max_iter_sem is not of length 1" %>%
      stop()
  }

  if (is.na(max_iter_sem)) {
    "max_iter_sem is not finite" %>%
      stop()
  }

  if (max_iter_sem < 0) {
    "max_iter_sem is negative" %>%
      stop()
  }

  if (round(max_iter_sem) != max_iter_sem) {
    "max_iter_sem is not an integer" %>%
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

  dots <- list(...)
  names_dots <- dots %>%
    names()

  if (is.null(names_dots)) {
    names_dots <- "" %>%
      rep(length(dots))
  }

  dots_un <- dots[names_dots == ""]
  names_dots_em <- c("regul", "epsilon", "max_iter_em")
  names_dots_un <- c("add", "remove", "min_x", "max_x", "max_iter_step",
                     "split", "merge", "min_comp", "max_comp", "space",
                     "max_rank", "max_iter_smem", names_dots_em) %>%
    setdiff(names_dots) %>%
    .[seq_along(dots_un)]
  dots_em <- dots %>%
    c(dots_un) %>%
    .[c(names_dots, names_dots_un) %in% names_dots_em]
  data_compl <- NULL
  seq_score <- numeric() %>%
    matrix(2, 0) %>%
    list()
  v_score <- - Inf
  diff_score <- Inf
  iter <- 0

  while (diff_score > 0 & iter < max_iter_sem) {
    iter <- iter + 1

    if (verbose) {
      "iter " %>%
        str_c(iter) %>%
        cat()
    }

    gmgm_old <- gmgm
    data_compl_old <- data_compl
    v_score_old <- v_score
    res_pem <- list(gmgm = gmgm, data = data, nodes = nodes, col_seq = col_seq,
                    n_part = n_part, max_part_sim = max_part_sim,
                    min_ess = min_ess, max_iter_pem = max_iter_pem,
                    verbose = FALSE) %>%
      c(dots_em) %>%
      do.call("param_em", .)
    gmgm <- res_pem$gmgm
    data_compl <- res_pem$data

    if (max_iter_pem == 0) {
      if (is_gmbn) {
        data_compl <- gmgm %>%
          inference(data, n_part = n_part, max_part_sim = max_part_sim)
      } else {
        data_compl <- gmgm %>%
          smoothing(data, col_seq = col_seq, n_part = n_part,
                    max_part_sim = max_part_sim, min_ess = min_ess)
      }
    }

    res_learn <- gmgm %>%
      struct_learn(gmgm = ., data = data_compl, nodes = nodes,
                   arcs_cand = arcs_cand, col_seq = col_seq, score = score,
                   verbose = FALSE, ...)
    gmgm <- res_learn$gmgm
    evol_score <- res_learn$evol_score$global
    seq_score <- seq_score %>%
      c(list(evol_score))
    v_score <- evol_score["new"]
    diff_score <- v_score - v_score_old

    if (verbose) {
      if (iter == 1) {
        score <- str_to_lower(score)
      }

      "    " %>%
        str_c(score, "_e = ", evol_score["old"], "    ", score, "_m = ",
              v_score, "\n") %>%
        cat()
    }
  }

  if (diff_score < 0) {
    gmgm <- gmgm_old
    data_compl <- data_compl_old
  }

  seq_score <- seq_score %>%
    do.call("cbind", .)
  rownames(seq_score) <- c("E", "M")
  list(gmgm = gmgm, data = data_compl, seq_score = seq_score) %>%
    return()
}
