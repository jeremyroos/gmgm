#' Perform smoothing inference in a Gaussian mixture dynamic Bayesian network
#'
#' This function performs smoothing inference in a Gaussian mixture dynamic
#' Bayesian network. For a sequence of \eqn{T} time slices, this task consists
#' in estimating the state of the system at each time slice \eqn{t}
#' (for \ifelse{html}{\out{1 &le; <i>t</i> &le; <i>T</i>}}{\eqn{1 \le t \le T}})
#' given all the data (the evidence) collected up to \eqn{T}. Smoothing
#' inference is performed by sequential importance resampling, which is a
#' particle-based approximate method (Koller and Friedman, 2009).
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
#' @return A data frame (tibble) with a structure similar to \code{evid}
#' containing the estimated values of the inferred nodes and their observation
#' sequences (if \code{col_seq} is not \code{NULL}).
#'
#' @references
#' Koller, D. and Friedman, N. (2009). \emph{Probabilistic Graphical Models:
#' Principles and Techniques}. The MIT Press.
#'
#' @seealso \code{\link{filtering}}, \code{\link{inference}},
#' \code{\link{prediction}}
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
#' smooth <- smoothing(gmdbn_air, evid, col_seq = "DATE", verbose = TRUE)}
#'
#' @export

smoothing <- function(gmdbn, evid, nodes = names(gmdbn$b_1), col_seq = NULL,
                      n_part = 1000, max_part_sim = 1e06, min_ess = 1,
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
    select_at(col_seq) %>%
    as_tibble()

  if (any(!(map_chr(seq, mode) %in% c("numeric", "character", "logical")))) {
    "columns of evid[col_seq] have invalid types" %>%
      stop()
  }

  if (length(gmdbn) == 1) {
    smooth <- seq %>%
      bind_cols(inference(gmdbn$b_1, evid, nodes = nodes, n_part = n_part,
                          max_part_sim = max_part_sim, verbose = verbose))
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

    if (nrow(evid) == 0) {
      nodes_0 <- numeric() %>%
        matrix(0, n_nodes, dimnames = list(NULL, nodes))
      smooth <- seq %>%
        slice(0) %>%
        bind_cols(as_tibble(nodes_0))
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
      arcs <- struct$arcs %>%
        bind_rows()

      if (n_nodes < n_nodes_gmdbn) {
        nodes_obs <- evid %>%
          select_if(col_evid %in% nodes_gmdbn) %>%
          select_if(~ !any(is.na(.))) %>%
          colnames()
        blanket <- nodes
        blanket_miss <- blanket[!(blanket %in% nodes_obs)]

        while (length(blanket_miss) > 0) {
          child <- arcs %>%
            filter(from %in% blanket_miss) %>%
            .$to
          copar <- arcs %>%
            filter(to %in% c(blanket_miss, child)) %>%
            .$from
          blanket_add <- child %>%
            c(copar) %>%
            setdiff(blanket)
          blanket <- blanket %>%
            c(blanket_add)
          blanket_miss <- blanket_add[!(blanket_add %in% nodes_obs)]
        }

        gmdbn <- gmdbn %>%
          remove_nodes(setdiff(nodes_gmdbn, blanket))
        nodes_gmdbn <- gmdbn$b_1 %>%
          names()
        n_nodes_gmdbn <- nodes_gmdbn %>%
          length()
        arcs <- arcs %>%
          filter(from %in% blanket, to %in% blanket)
      }

      col_evid_prop <- col_seq %>%
        c(nodes_gmdbn)
      prefix <- col_evid_prop %>%
        sort() %>%
        last() %>%
        str_c("_")
      col_n <- prefix %>%
        str_c("n")
      col_sub <- prefix %>%
        str_c("sub")
      col_weight <- prefix %>%
        str_c("weight")
      col_draw <- prefix %>%
        str_c("draw")
      col_prop <- col_seq %>%
        c(col_weight, col_draw)
      n_prop <- arcs$lag %>%
        max(0) - 1

      if (n_prop >= 0) {
        col_prop <- col_prop %>%
          c(nodes_gmdbn,
            str_c(rep(str_c(nodes_gmdbn, "."), n_prop),
                  rep(seq_len(n_prop), each = n_nodes_gmdbn)))
      }

      col_smooth <- col_seq %>%
        c(nodes)
      col_part <- col_smooth %>%
        c(col_weight, col_draw)
      col_time <- prefix %>%
        str_c("time")
      evid <- evid %>%
        select_if(col_evid %in% col_evid_prop)
      list_part <- list()
      times_gmbn <- gmdbn %>%
        names() %>%
        str_remove("b_") %>%
        as.numeric() %>%
        c(Inf)
      time_gmbn <- 1
      i_gmbn <- 1
      n_times_seq <- seq %>%
        group_by_at(col_seq) %>%
        summarise(!!col_n := n(), .groups = "drop")
      n_sub <- (nrow(n_times_seq) * n_part - 1) %/% max_part_sim + 1
      smooth <- n_times_seq %>%
        mutate(!!col_sub := ntile(!!sym(col_n), n_sub)) %>%
        group_by_at(col_sub) %>%
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
            select_at(col_seq)

          if (n_sub > 1) {
            evid <- seq %>%
              inner_join(evid, by = col_seq)
          }

          part <- seq %>%
            particles(col_weight = col_weight, n_part = n_part)
          max_time <- n_times_seq[[col_n]] %>%
            max()

          for (time in seq_len(max_time)) {
            if (verbose) {
              "time " %>%
                str_c(time, " / ", max_time, "\n") %>%
                cat()
            }

            evid_time <- evid %>%
              group_by_at(col_seq) %>%
              slice(time) %>%
              ungroup()
            part <- part %>%
              mutate(!!col_draw := seq_len(n()))

            if (is_col_seq) {
              part <- part %>%
                semi_join(evid_time, by = col_seq)
            }

            part <- part %>%
              select_if(colnames(.) %in% col_prop)

            if (time == time_gmbn) {
              gmbn <- gmdbn[[i_gmbn]]
              i_gmbn <- i_gmbn + 1
              time_gmbn <- times_gmbn[i_gmbn]
            }

            part <- part %>%
              propagation(gmbn, evid_time, col_seq = col_seq,
                          col_weight = col_weight, min_ess = min_ess)
            list_part <- list_part %>%
              c(list(select_at(part, col_part)))
          }

          n_times_seq %>%
            group_by_at(col_n) %>%
            group_map(function(seq, n) {
              max_time <- n[[col_n]]
              part <- list_part[[max_time]]

              if (is_col_seq) {
                part <- part %>%
                  semi_join(seq, by = col_seq)
              }

              weights <- part[[col_weight]]
              list_aggreg <- list()
              list_aggreg[[max_time]] <- part %>%
                select_at(col_smooth)

              for (time in rev(seq_len(max_time - 1))) {
                part <- list_part[[time]] %>%
                  slice(part[[col_draw]])
                list_aggreg[[time]] <- part %>%
                  select_at(col_smooth)
              }

              list_aggreg %>%
                map(function(part) {
                  part %>%
                    mutate_at(nodes, ~ . * weights) %>%
                    group_by_at(col_seq) %>%
                    summarise_at(nodes, sum) %>%
                    ungroup() %>%
                    return()
                }) %>%
                bind_rows() %>%
                return()
            }) %>%
            bind_rows() %>%
            return()
        }) %>%
        bind_rows() %>%
        group_by_at(col_seq) %>%
        mutate(!!col_time := seq_len(n())) %>%
        ungroup()
      smooth <- seq %>%
        group_by_at(col_seq) %>%
        mutate(!!col_time := seq_len(n())) %>%
        ungroup() %>%
        inner_join(smooth, by = c(col_seq, col_time)) %>%
        select_at(col_smooth)
    }
  }

  smooth %>%
    return()
}
