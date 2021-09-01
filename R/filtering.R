#' Perform filtering inference in a Gaussian mixture dynamic Bayesian network
#'
#' This function performs filtering inference in a Gaussian mixture dynamic
#' Bayesian network. For a sequence of \eqn{T} time slices, this task consists
#' in estimating the state of the system at each time slice \eqn{t}
#' (for \ifelse{html}{\out{1 &le; <i>t</i> &le; <i>T</i>}}{\eqn{1 \le t \le T}})
#' given all the data (the evidence) collected up to \eqn{t}. This
#' function is also designed to perform fixed-lag smoothing inference, which
#' consists in defining a time lag \eqn{l} such that at each time slice \eqn{t}
#' (for \ifelse{html}{\out{<i>l</i> + 1 &le; <i>t</i> &le; <i>T</i>}}{\eqn{l + 1
#' \le t \le T}}), the state at \ifelse{html}{\out{<i>t</i> &minus;
#' <i>l</i>}}{\eqn{t - l}} is estimated given the evidence collected up to
#' \eqn{t} (Murphy, 2002). Filtering and fixed-lag smoothing inference are
#' performed by sequential importance resampling, which is a particle-based
#' approximate method (Koller and Friedman, 2009).
#'
#' @param gmdbn An object of class \code{gmdbn}.
#' @param evid A data frame containing the evidence. Its columns must explicitly
#' be named after nodes of \code{gmdbn} and can contain missing values (columns
#' with no value can be removed).
#' @param nodes A character vector containing the inferred nodes (by default all
#' the nodes of \code{gmdbn}).
#' @param col_seq A character vector containing the column names of \code{evid}
#' that describe the observation sequence. If \code{NULL} (the default), all the
#' observations belong to a single sequence. The observations of a same sequence
#' must be ordered such that the \eqn{t}th one is related to time slice \eqn{t}
#' (note that the sequences can have different lengths).
#' @param lag A non-negative integer vector containing the time lags for which
#' fixed-lag smoothing inference is performed. If \code{0} (the default),
#' filtering inference is performed.
#' @param n_part A positive integer corresponding to the number of particles
#' generated for each observation sequence.
#' @param max_part_sim An integer greater than or equal to \code{n_part}
#' corresponding to the maximum number of particles that can be processed
#' simultaneously. This argument is used to prevent memory overflow, dividing
#' \code{evid} into smaller subsets that are handled sequentially.
#' @param min_ess A numeric value in [0, 1] corresponding to the minimum ESS
#' (expressed as a proportion of \code{n_part}) under which the renewal step of
#' sequential importance resampling is performed. If \code{1} (the default),
#' this step is performed at each time slice.
#' @param verbose A logical value indicating whether subsets of \code{evid} and
#' time slices in progress are displayed.
#'
#' @return If \code{lag} has one element, a data frame (tibble) with a structure
#' similar to \code{evid} containing the estimated values of the inferred
#' nodes and their observation sequences (if \code{col_seq} is not \code{NULL}).
#' If \code{lag} has two or more elements, a list of data frames (tibbles)
#' containing these values for each time lag.
#'
#' @references
#' Koller, D. and Friedman, N. (2009). \emph{Probabilistic Graphical Models:
#' Principles and Techniques}. The MIT Press.
#'
#' Murphy, K. (2002). \emph{Dynamic Bayesian Networks: Representation, Inference
#' and Learning}. PhD thesis, University of California.
#'
#' @seealso \code{\link{inference}}, \code{\link{prediction}},
#' \code{\link{smoothing}}
#'
#' @examples
#' \donttest{
#' set.seed(0)
#' data(gmdbn_air, data_air)
#' evid <- data_air
#' evid$NO2[sample.int(7680, 1536)] <- NA
#' evid$O3[sample.int(7680, 1536)] <- NA
#' evid$TEMP[sample.int(7680, 1536)] <- NA
#' evid$WIND[sample.int(7680, 1536)] <- NA
#' filt <- filtering(gmdbn_air, evid, col_seq = "DATE", lag = c(0, 1),
#'                   verbose = TRUE)}
#'
#' @export

filtering <- function(gmdbn, evid, nodes = names(gmdbn$b_1), col_seq = NULL,
                      lag = 0, n_part = 1000, max_part_sim = 1e06, min_ess = 1,
                      verbose = FALSE) {

  if (!inherits(gmdbn, "gmdbn")) {
    "gmdbn is not of class \"gmdbn\"" %>%
      stop()
  }

  if (!is.data.frame(evid)) {
    "evid is not a data frame" %>%
      stop()
  }

  evid <- evid %>%
    ungroup()
  col_evid <- evid %>%
    colnames()

  if (any(duplicated(col_evid))) {
    "evid has duplicated column names" %>%
      stop()
  }

  struct <- gmdbn %>%
    structure()
  nodes_gmdbn <- struct$nodes
  is_col_seq <- !is.null(col_seq)

  if (is_col_seq) {
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

    if (any(str_remove(col_seq, "\\.[1-9][0-9]*$") %in% nodes_gmdbn)) {
      "col_seq contains nodes (or instantiations of nodes) of gmdbn" %>%
        stop()
    }

    if (any(!(col_seq %in% col_evid))) {
      "elements of col_seq are not column names of evid" %>%
        stop()
    }
  }

  seq <- evid %>%
    select(all_of(col_seq)) %>%
    as_tibble()

  if (any(!(map_chr(seq, mode) %in% c("numeric", "character", "logical")))) {
    "columns of evid[col_seq] have invalid types" %>%
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

  lag <- lag %>%
    sort()

  if (length(gmdbn) == 1) {
    infer <- seq %>%
      bind_cols(inference(gmdbn$b_1, evid, nodes = nodes, n_part = n_part,
                          max_part_sim = max_part_sim, verbose = verbose))
    filt <- lag %>%
      map(function(lag) {
        infer %>%
          group_by(across(col_seq)) %>%
          mutate(across(nodes, ~ lead(lag(., lag), lag))) %>%
          ungroup() %>%
          return()
      })

    if (n_lags == 1) {
      filt <- filt[[1]]
    } else {
      filt <- filt %>%
        set_names(str_c("lag_", lag))
    }
  } else {
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

    if (any(!(nodes %in% nodes_gmdbn))) {
      "elements of nodes are not nodes of gmdbn" %>%
        stop()
    }

    nodes <- nodes %>%
      sort()
    col_evid_prop <- col_seq %>%
      c(nodes_gmdbn)
    prefix <- col_evid_prop %>%
      sort() %>%
      last() %>%
      str_c("_")
    col_n <- prefix %>%
      str_c("n")
    n_times_seq <- seq %>%
      group_by(across(col_seq)) %>%
      summarise(!!col_n := n(), .groups = "drop")
    min_lag <- lag[1]

    if (max(n_times_seq[[col_n]], 0) <= min_lag) {
      nodes_0 <- numeric() %>%
        matrix(0, n_nodes, dimnames = list(NULL, nodes)) %>%
        as_tibble()
      filt <- seq %>%
        bind_rows(nodes_0)

      if (n_lags > 1) {
        filt <- filt %>%
          list() %>%
          rep(n_lags) %>%
          set_names(str_c("lag_", lag))
      }
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

      n_nodes_gmdbn <- nodes_gmdbn %>%
        length()

      if (n_nodes < n_nodes_gmdbn) {
        evid_nodes <- evid %>%
          select(any_of(nodes_gmdbn))
        nodes_obs <- evid_nodes %>%
          select(where(~ !any(is.na(.)))) %>%
          colnames()
        nodes_miss <- evid_nodes %>%
          select(where(~ all(is.na(.)))) %>%
          colnames() %>%
          c(setdiff(nodes_gmdbn, colnames(evid_nodes)))
        gmdbn <- gmdbn %>%
          relevant(nodes, nodes_obs, nodes_miss)
        nodes_gmdbn <- gmdbn$b_1 %>%
          names()
        n_nodes_gmdbn <- nodes_gmdbn %>%
          length()
        col_evid_prop <- col_seq %>%
          c(nodes_gmdbn)
      }

      col_sub <- prefix %>%
        str_c("sub")
      col_weight <- prefix %>%
        str_c("weight")
      col_prop <- col_seq %>%
        c(col_weight)
      max_lag <- lag[n_lags]
      n_prop <- struct$arcs %>%
        bind_rows() %>%
        filter(from %in% nodes_gmdbn, to %in% nodes_gmdbn) %>%
        .$lag %>%
        max(max_lag) - 1

      if (n_prop >= 0) {
        col_prop <- col_prop %>%
          c(str_c(rep(str_c(nodes_gmdbn, "."), n_prop),
                  rep(rev(seq_len(n_prop)), each = n_nodes_gmdbn)),
            nodes_gmdbn)
      }

      evid <- evid %>%
        select(any_of(col_evid_prop))
      times_gmbn <- gmdbn %>%
        names() %>%
        str_remove("b_") %>%
        as.numeric() %>%
        c(Inf)
      min_time <- min_lag + 1
      time_gmbn <- min_time
      i_gmbn <- time_gmbn %>%
        findInterval(times_gmbn)
      list_filt <- list()
      n_sub <- (nrow(n_times_seq) * n_part - 1) %/% max_part_sim + 1
      filt <- n_times_seq %>%
        mutate(!!col_sub := ntile(!!sym(col_n), n_sub)) %>%
        group_by(across(col_sub)) %>%
        group_map(function(n_times_seq, sub) {
          if (verbose) {
            verb <- "subset " %>%
              str_c(sub[[col_sub]], " / ", n_sub)
            "\n" %>%
              c(verb, "\n", rep("-", str_length(verb)), "\n") %>%
              str_c(collapse = "") %>%
              cat()
          }

          seq <- n_times_seq %>%
            select(all_of(col_seq))

          if (n_sub > 1) {
            evid <- seq %>%
              inner_join(evid, by = col_seq)
          }

          part <- seq %>%
            particles(col_weight = col_weight, n_part = n_part) %>%
            propagation(gmdbn, evid, col_seq = col_seq, col_weight = col_weight,
                        n_times = min_lag, min_ess = min_ess)
          max_time <- n_times_seq[[col_n]] %>%
            max()

          for (time in min_time:max_time) {
            if (verbose) {
              "time " %>%
                str_c(time, " / ", max_time, "\n") %>%
                cat()
            }

            evid_time <- evid %>%
              group_by(across(col_seq)) %>%
              slice(time) %>%
              ungroup()

            if (is_col_seq) {
              part <- part %>%
                semi_join(evid_time, by = col_seq)
            }

            part <- part %>%
              select(any_of(col_prop))

            if (time == time_gmbn) {
              gmbn <- gmdbn[[i_gmbn]]
              i_gmbn <- i_gmbn + 1
              time_gmbn <- times_gmbn[i_gmbn]
            }

            part <- part %>%
              propagation(gmbn, evid_time, col_seq = col_seq,
                          col_weight = col_weight, min_ess = min_ess)
            filt_time <- part %>%
              aggregation(nodes, col_seq = col_seq, col_weight = col_weight,
                          lag = lag)
            list_filt <- list_filt %>%
              c(list(filt_time))
          }

          list_filt %>%
            return()
        })
      col_time <- prefix %>%
        str_c("time")
      seq_time <- seq %>%
        group_by(across(col_seq)) %>%
        mutate(!!col_time := seq_len(n())) %>%
        ungroup()
      col_seq_time <- col_seq %>%
        c(col_time)
      col_filt <- col_seq %>%
        c(nodes)

      if (n_lags == 1) {
        filt <- filt %>%
          bind_rows() %>%
          group_by(across(col_seq)) %>%
          mutate(!!col_time := seq_len(n())) %>%
          ungroup() %>%
          left_join(seq_time, ., by = col_seq_time) %>%
          select(all_of(col_filt))
      } else {
        filt <- filt %>%
          flatten() %>%
          transpose() %>%
          map2(lag, function(filt, lag) {
            filt %>%
              bind_rows() %>%
              group_by(across(col_seq)) %>%
              mutate(!!col_time := seq_len(n())) %>%
              ungroup() %>%
              left_join(seq_time, ., by = col_seq_time) %>%
              group_by(across(col_seq)) %>%
              mutate(across(nodes, ~ lead(., lag - min_lag))) %>%
              ungroup() %>%
              select(all_of(col_filt)) %>%
              return()
          })
      }
    }
  }

  filt %>%
    return()
}
