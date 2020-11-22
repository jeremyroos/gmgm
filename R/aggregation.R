#' Aggregate particles to obtain inferred values
#'
#' This function aggregates particles to obtain inferred values. Assuming that
#' the particles have been propagated to a given time slice \eqn{t}, the
#' weighted average of the samples is computed to estimate the state of the
#' system at \eqn{t} or at previous time slices (Koller and Friedman, 2009).
#'
#' @param part A data frame containing the particles propagated to time slice
#' \eqn{t}, as obtained from function \code{\link{particles}} or
#' \code{\link{propagation}}.
#' @param nodes A character vector containing the inferred nodes.
#' @param col_seq A character vector containing the column names of \code{part}
#' that describe the observation sequence. If \code{NULL} (the default), all the
#' particles belong to a single sequence.
#' @param col_weight A character string corresponding to the column name of
#' \code{part} that describes the particle weight.
#' @param lag A non-negative integer vector containing the time lags \eqn{l_1,
#' l_2, \dots}{l1, l2, \dots} such that the samples of time slices \eqn{t - l_1,
#' t - l_2, \dots}{t - l1, t - l2, \dots} are aggregated.
#'
#' @return If \code{lag} has one element, a data frame (tibble) containing the
#' aggregated values of the inferred nodes and their observation sequences (if
#' \code{col_seq} is not \code{NULL}). If \code{lag} has two or more elements, a
#' list of data frames (tibbles) containing these values for each time lag.
#'
#' @references
#' Koller, D. and Friedman, N. (2009). \emph{Probabilistic Graphical Models:
#' Principles and Techniques}. The MIT Press.
#'
#' @seealso \code{\link{aggregation}}, \code{\link{particles}}
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' set.seed(0)
#' data(gmdbn_air, data_air)
#' evid <- data_air %>%
#'   group_by(DATE) %>%
#'   slice(1:3) %>%
#'   ungroup()
#' evid$NO2[sample.int(150, 30)] <- NA
#' evid$O3[sample.int(150, 30)] <- NA
#' evid$TEMP[sample.int(150, 30)] <- NA
#' evid$WIND[sample.int(150, 30)] <- NA
#' aggreg <- particles(data.frame(DATE = unique(evid$DATE))) %>%
#'   propagation(gmdbn_air, evid, col_seq = "DATE", n_times = 3) %>%
#'   aggregation(c("NO2", "O3", "TEMP", "WIND"), col_seq = "DATE", lag = c(0, 1))}
#'
#' @export

aggregation <- function(part, nodes, col_seq = NULL, col_weight = "weight",
                        lag = 0) {
  if (!is.data.frame(part)) {
    "part is not a data frame" %>%
      stop()
  }

  part <- part %>%
    as_tibble()

  if (nrow(part) == 0) {
    "part has no row" %>%
      stop()
  }

  col_part <- part %>%
    colnames()

  if (any(duplicated(col_part))) {
    "part has duplicated column names" %>%
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

  if (any(is.na(nodes))) {
    "nodes has NA elements" %>%
      stop()
  }

  if (any(!str_detect(nodes,
                      "^(\\.([A-Za-z_\\.]|$)|[A-Za-z])[A-Za-z0-9_\\.]*$") |
          str_detect(nodes, "\\.[1-9][0-9]*$"))) {
    "nodes contains invalid node names" %>%
      stop()
  }

  if (!is.null(col_seq)) {
    if (!is.vector(col_seq, "character")) {
      "col_seq is not a character vector" %>%
        stop()
    }

    col_seq <- col_seq %>%
      unique()

    if (any(!str_detect(col_seq,
                        "^(\\.([A-Za-z_\\.]|$)|[A-Za-z])[A-Za-z0-9_\\.]*$"))) {
      "col_seq contains invalid column names" %>%
        stop()
    }

    if (any(str_remove(col_seq, "\\.[1-9][0-9]*$") %in% nodes)) {
      "col_seq contains nodes (or instantiations of nodes)" %>%
        stop()
    }

    if (any(!(col_seq %in% col_part))) {
      "elements of col_seq are not column names of part" %>%
        stop()
    }
  }

  seq <- part %>%
    select_at(col_seq)

  if (any(!(map_chr(seq, mode) %in% c("numeric", "character", "logical")))) {
    "columns of part[col_seq] have invalid types" %>%
      stop()
  }

  if (!is.vector(col_weight, "character")) {
    "col_weight is not a character string" %>%
      stop()
  }

  if (length(col_weight) != 1) {
    "col_weight is not of length 1" %>%
      stop()
  }

  if (!str_detect(col_weight,
                  "^(\\.([A-Za-z_\\.]|$)|[A-Za-z])[A-Za-z0-9_\\.]*$")) {
    "col_weight is an invalid column name" %>%
      stop()
  }

  if (!(col_weight %in% col_part)) {
    "col_weight is not a column name of part" %>%
      stop()
  }

  if (!is.numeric(part[[col_weight]])) {
    "part[[col_weight]] is not numeric" %>%
      stop()
  }

  if (str_remove(col_weight, "\\.[1-9][0-9]*$") %in% nodes) {
    "col_weight is a node (or an instantiation of a node)" %>%
      stop()
  }

  if (col_weight %in% col_seq) {
    "col_weight is in col_seq" %>%
      stop()
  }

  if (!is.vector(lag, "numeric")) {
    "lag is not a numeric vector" %>%
      stop()
  }

  lag <- lag %>%
    unique()
  n_lags <- lag %>%
    length()

  if (n_lags == 0) {
    "lag is empty" %>%
      stop()
  }

  if (any(!is.finite(lag))) {
    "lag has non-finite elements" %>%
      stop()
  }

  if (any(lag < 0)) {
    "lag has negative elements" %>%
      stop()
  }

  if (any(round(lag) != lag)) {
    "lag has non-integer elements" %>%
      stop()
  }

  nodes <- nodes %>%
    sort()
  nodes_0 <- numeric() %>%
    matrix(0, n_nodes, dimnames = list(NULL, nodes)) %>%
    as_tibble()
  aggreg_0 <- seq %>%
    slice(0) %>%
    bind_cols(nodes_0)
  lag <- lag %>%
    sort()
  aggreg <- "." %>%
    rep(n_lags) %>%
    str_c(lag) %>%
    str_replace("\\.0", "") %>%
    map(function(lag) {
      nodes_aggreg <- col_part[col_part %in% str_c(nodes, lag)]

      if (any(!map_lgl(part[nodes_aggreg], is.numeric))) {
        "columns of part related to nodes are not numeric" %>%
          stop()
      }

      part %>%
        mutate_at(nodes_aggreg, ~ . * !!sym(col_weight)) %>%
        group_by_at(col_seq) %>%
        summarise_at(nodes_aggreg, ~ sum(.) / sum(!!sym(col_weight))) %>%
        ungroup() %>%
        set_names(c(col_seq, str_remove(nodes_aggreg, "\\.[1-9][0-9]*$"))) %>%
        bind_rows(aggreg_0, .) %>%
        return()
    })

  if (n_lags == 1) {
    aggreg[[1]] %>%
      return()
  } else {
    aggreg %>%
      set_names(str_c("lag_", lag)) %>%
      return()
  }
}
