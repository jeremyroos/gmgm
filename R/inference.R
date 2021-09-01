#' Perform inference in a Gaussian mixture Bayesian network
#'
#' This function performs inference in a (non-temporal) Gaussian mixture
#' Bayesian network. This task consists in estimating the state of the system
#' given partial observations of it (the evidence). Inference is performed by
#' likelihood weighting, which is a particle-based approximate method (Koller
#' and Friedman, 2009).
#'
#' @param gmbn A (non-temporal) object of class \code{gmbn}.
#' @param evid A data frame containing the evidence. Its columns must explicitly
#' be named after nodes of \code{gmbn} and can contain missing values (columns
#' with no value can be removed).
#' @param nodes A character vector containing the inferred nodes (by default all
#' the nodes of \code{gmbn}).
#' @param n_part A positive integer corresponding to the number of particles
#' generated for each observation.
#' @param max_part_sim An integer greater than or equal to \code{n_part}
#' corresponding to the maximum number of particles that can be processed
#' simultaneously. This argument is used to prevent memory overflow, dividing
#' \code{evid} into smaller subsets that are handled sequentially.
#' @param verbose A logical value indicating whether subsets of \code{evid} in
#' progress are displayed.
#'
#' @return A data frame (tibble) with a structure similar to \code{evid}
#' containing the estimated values of the inferred nodes.
#'
#' @references
#' Koller, D. and Friedman, N. (2009). \emph{Probabilistic Graphical Models:
#' Principles and Techniques}. The MIT Press.
#'
#' @seealso \code{\link{filtering}}, \code{\link{prediction}},
#' \code{\link{smoothing}}
#'
#' @examples
#' \donttest{
#' set.seed(0)
#' data(gmbn_body, data_body)
#' evid <- data_body
#' evid$GENDER[sample.int(2148, 430)] <- NA
#' evid$AGE[sample.int(2148, 430)] <- NA
#' evid$HEIGHT[sample.int(2148, 430)] <- NA
#' evid$WEIGHT[sample.int(2148, 430)] <- NA
#' evid$FAT[sample.int(2148, 430)] <- NA
#' evid$WAIST[sample.int(2148, 430)] <- NA
#' evid$GLYCO[sample.int(2148, 430)] <- NA
#' infer <- inference(gmbn_body, evid, verbose = TRUE)}
#'
#' @export

inference <- function(gmbn, evid, nodes = names(gmbn), n_part = 1000,
                      max_part_sim = 1e06, verbose = FALSE) {
  if (!inherits(gmbn, "gmbn")) {
    "gmbn is not of class \"gmbn\"" %>%
      stop()
  }

  if (!is.data.frame(evid)) {
    "evid is not a data frame" %>%
      stop()
  }

  evid <- evid %>%
    ungroup()

  if (any(duplicated(colnames(evid)))) {
    "evid has duplicated column names" %>%
      stop()
  }

  if (!is.vector(nodes, "character")) {
    "nodes is not a character vector" %>%
      stop()
  }

  nodes <- nodes %>%
    unique()
  n_nodes <- nodes %>%
    length()

  if (n_nodes == 0) {
    "nodes is empty" %>%
      stop()
  }

  struct <- gmbn %>%
    structure()
  nodes_gmbn <- struct$nodes

  if (any(!(nodes %in% nodes_gmbn))) {
    "elements of nodes are not nodes of gmbn" %>%
      stop()
  }

  nodes <- nodes %>%
    sort()
  n_obs <- evid %>%
    nrow()

  if (n_obs == 0) {
    infer <- numeric() %>%
      matrix(0, n_nodes, dimnames = list(NULL, nodes)) %>%
      as_tibble()
  } else {
    if (!is.vector(n_part, "numeric")) {
      "n_part is not a numeric value" %>%
        stop()
    }

    if (length(n_part) != 1) {
      "n_part is not of length 1" %>%
        stop()
    }

    if (!is.finite(n_part)) {
      "n_part is not finite" %>%
        stop()
    }

    if (n_part <= 0) {
      "n_part is not positive" %>%
        stop()
    }

    if (round(n_part) != n_part) {
      "n_part is not an integer" %>%
        stop()
    }

    if (!is.vector(max_part_sim, "numeric")) {
      "max_part_sim is not a numeric value" %>%
        stop()
    }

    if (length(max_part_sim) != 1) {
      "max_part_sim is not of length 1" %>%
        stop()
    }

    if (is.na(max_part_sim)) {
      "max_part_sim is NA" %>%
        stop()
    }

    if (max_part_sim < n_part) {
      "max_part_sim is lower than n_part" %>%
        stop()
    }

    if (round(max_part_sim) != max_part_sim) {
      "max_part_sim is not an integer" %>%
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

    if (n_nodes < length(nodes_gmbn)) {
      evid_nodes <- evid %>%
        select(any_of(nodes_gmbn))
      nodes_obs <- evid_nodes %>%
        select(where(~ !any(is.na(.)))) %>%
        colnames()
      nodes_miss <- evid_nodes %>%
        select(where(~ all(is.na(.)))) %>%
        colnames() %>%
        c(setdiff(nodes_gmbn, colnames(evid_nodes)))
      gmbn <- gmbn %>%
        relevant(nodes, nodes_obs, nodes_miss)
      nodes_gmbn <- gmbn %>%
        names()
    }

    prefix <- nodes_gmbn %>%
      last() %>%
      str_c("_")
    col_seq <- prefix %>%
      str_c("seq")
    col_sub <- prefix %>%
      str_c("sub")
    col_weight <- prefix %>%
      str_c("weight")
    n_sub <- (n_obs * n_part - 1) %/% max_part_sim + 1
    infer <- evid %>%
      select(any_of(nodes_gmbn)) %>%
      mutate(!!col_seq := seq_len(n()),
             !!col_sub := ntile(!!sym(col_seq), n_sub)) %>%
      group_by(across(col_sub)) %>%
      group_map(function(evid, sub) {
        if (verbose) {
          "subset " %>%
            str_c(sub[[col_sub]], " / ", n_sub, "\n") %>%
            cat()
        }

        evid %>%
          select(all_of(col_seq)) %>%
          particles(col_weight = col_weight, n_part = n_part) %>%
          propagation(gmbn, evid, col_seq = col_seq,
                      col_weight = col_weight) %>%
          aggregation(nodes, col_seq = col_seq, col_weight = col_weight) %>%
          select(all_of(nodes)) %>%
          return()
      }) %>%
      bind_rows()
  }

  infer %>%
    return()
}
