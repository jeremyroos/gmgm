#' Add arcs to a Gaussian mixture graphical model
#'
#' This function adds arcs to a Gaussian mixture graphical model. For each added
#' arc, a variable related to the start node is added to the Gaussian mixture
#' model describing the local distribution over the end node and its parents,
#' with mean 0 and variance 1 for each mixture component.
#'
#' @param gmgm An object of class \code{gmbn} or \code{gmdbn}.
#' @param arcs A data frame containing the added arcs. The column \code{from}
#' describes the start node, the column \code{to} the end node and the column
#' \code{lag} the time lag between them. Missing values in \code{from} or
#' \code{to} are interpreted as "all possible nodes", which allows to quickly
#' define large set of arcs that share common attributes. Missing values in
#' \code{lag} are replaced by 0. If \code{gmgm} is a \code{gmdbn} object, the
#' same arcs are added to each of its \code{gmbn} elements. This constraint can
#' be overcome by passing a list of data frames named after some of these
#' elements (\code{b_1}, \dots) and containing arcs specifically added to them.
#' The arcs whose time lags exceed the maximum temporal depth of their
#' \code{gmbn} element are not taken into account.
#'
#' @return The \code{gmbn} or \code{gmdbn} object after adding the arcs.
#'
#' @seealso \code{\link{add_nodes}}, \code{\link{remove_arcs}},
#' \code{\link{remove_nodes}}, \code{\link{rename_nodes}}
#'
#' @examples
#' data(gmbn_body)
#' gmbn_1 <- add_arcs(gmbn_body,
#'                    data.frame(from = c("GENDER", "AGE"),
#'                               to = c("GLYCO", "WEIGHT")))
#'
#' data(gmdbn_air)
#' gmdbn_1 <- add_arcs(gmdbn_air,
#'                     list(b_2 = data.frame(from = "WIND", to = "NO2", lag = 1),
#'                          b_13 = data.frame(from = c("NO2", "NO2"),
#'                                            to = c("O3", "O3"), lag = c(0, 1))))
#'
#' @export

add_arcs <- function(gmgm, arcs) {
  if (inherits(gmgm, "gmbn")) {
    if (!is.null(arcs)) {
      if (!is.data.frame(arcs)) {
        "arcs is not a data frame" %>%
          stop()
      }

      arcs <- arcs %>%
        ungroup()
      col_arcs <- arcs %>%
        colnames()

      if (any(duplicated(col_arcs))) {
        "arcs has duplicated column names" %>%
          stop()
      }

      if (!any(c("from", "to", "lag") %in% col_arcs)) {
        "arcs has no column from, to or lag" %>%
          stop()
      }

      nodes <- gmgm %>%
        names()

      if ("from" %in% col_arcs) {
        from <- arcs$from
        from <- from[!is.na(from)]

        if (!is.character(from)) {
          if (length(from) == 0 | is.factor(from)) {
            arcs <- arcs %>%
              mutate(from = as.character(from))
            from <- from %>%
              as.character()
          } else {
            "arcs[[\"from\"]] is not character" %>%
              stop()
          }
        }

        if (any(!(from %in% nodes))) {
          "elements of arcs[[\"from\"]] are not nodes of gmgm" %>%
            stop()
        }
      }

      if ("to" %in% col_arcs) {
        to <- arcs$to
        to <- to[!is.na(to)]

        if (!is.character(to)) {
          if (length(to) == 0 | is.factor(to)) {
            arcs <- arcs %>%
              mutate(to = as.character(to))
            to <- to %>%
              as.character()
          } else {
            "arcs[[\"to\"]] is not character" %>%
              stop()
          }
        }

        if (any(!(to %in% nodes))) {
          "elements of arcs[[\"to\"]] are not nodes of gmgm" %>%
            stop()
        }
      }

      if ("lag" %in% col_arcs) {
        lag <- arcs$lag
        lag <- lag[!is.na(lag)]

        if (!is.numeric(lag)) {
          if (length(lag) == 0) {
            arcs <- arcs %>%
              mutate(lag = as.numeric(lag))
            lag <- lag %>%
              as.numeric()
          } else {
            "arcs[[\"lag\"]] is not numeric" %>%
              stop()
          }
        }

        if (any(is.infinite(lag))) {
          "arcs[[\"lag\"]] has infinite elements" %>%
            stop()
        }

        if (any(lag < 0)) {
          "arcs[[\"lag\"]] has negative elements" %>%
            stop()
        }

        if (any(round(lag) != lag)) {
          "arcs[[\"lag\"]] has non-integer elements" %>%
            stop()
        }
      }

      arcs <- tibble(from = character(), to = character(), lag = numeric()) %>%
        bind_rows(arcs) %>%
        select(from, to, lag) %>%
        mutate(lag = replace_na(lag, 0))
      n_loops <- arcs %>%
        filter(from == to, lag == 0) %>%
        nrow()

      if (n_loops > 0) {
        "arcs contains loops" %>%
          stop()
      }

      arcs_from_na <- arcs %>%
        filter(is.na(from)) %>%
        select(to, lag) %>%
        crossing(from = nodes)
      arcs <- arcs %>%
        filter(!is.na(from)) %>%
        bind_rows(arcs_from_na)
      arcs_to_na <- arcs %>%
        filter(is.na(to)) %>%
        select(from, lag) %>%
        crossing(to = nodes)
      arcs <- arcs %>%
        filter(!is.na(to)) %>%
        bind_rows(arcs_to_na) %>%
        filter(from != to | lag > 0) %>%
        distinct(from, to, lag)

      if (nrow(arcs) > 0) {
        to <- arcs$to %>%
          unique() %>%
          sort()
        gmgm <- arcs %>%
          mutate(from_lag = if_else(lag == 0, from, str_c(from, ".", lag))) %>%
          group_by(to) %>%
          group_map(function(arcs, to) {
            gmgm[[to$to]] %>%
              add_var(arcs$from_lag) %>%
              return()
          }) %>%
          set_names(to) %>%
          c(gmgm[setdiff(nodes, to)]) %>%
          do.call("gmbn", .)
      }
    }
  } else {
    if (!inherits(gmgm, "gmdbn")) {
      "gmgm is not of class \"gmbn\" or \"gmdbn\"" %>%
        stop()
    }

    n_gmbn <- gmgm %>%
      length()
    names_gmbn <- gmgm %>%
      names()

    if (inherits(arcs, "list")) {
      names_arcs <- arcs %>%
        names()
      n_arcs <- names_arcs %>%
        length()

      if (n_arcs < length(arcs)) {
        "the elements of arcs have no names" %>%
          stop()
      }

      if (any(duplicated(names_arcs))) {
        "arcs has duplicated element names" %>%
          stop()
      }

      if (any(!(names_arcs %in% names_gmbn))) {
        "element names of arcs are not element names of gmgm" %>%
          stop()
      }

      arcs <- NULL %>%
        list() %>%
        rep(n_gmbn - n_arcs) %>%
        set_names(setdiff(names_gmbn, names_arcs)) %>%
        c(arcs) %>%
        .[names_gmbn]
    } else {
      arcs <- arcs %>%
        list() %>%
        rep(n_gmbn)
    }

    times_gmbn <- names_gmbn %>%
      str_remove("b_") %>%
      as.numeric()
    gmgm <- gmgm %>%
      list(arcs, names_gmbn, times_gmbn) %>%
      pmap(function(gmbn, arcs, name_gmbn, time_gmbn) {
        if (!is.null(arcs)) {
          if (!is.data.frame(arcs)) {
            "arcs[[\"" %>%
              str_c(name_gmbn, "\"]] is not a data frame") %>%
              stop()
          }

          arcs <- arcs %>%
            ungroup()
          col_arcs <- arcs %>%
            colnames()

          if (any(duplicated(col_arcs))) {
            "arcs[[\"" %>%
              str_c(name_gmbn, "\"]] has duplicated column names") %>%
              stop()
          }

          if ("lag" %in% col_arcs) {
            lag <- arcs$lag
            lag <- lag[!is.na(lag)]

            if (length(lag) > 0) {
              if (!is.numeric(lag)) {
                "arcs[[\"" %>%
                  str_c(name_gmbn, "\"]][[\"lag\"]] is not numeric") %>%
                  stop()
              }

              if (any(is.infinite(lag))) {
                "arcs[[\"" %>%
                  str_c(name_gmbn, "\"]][[\"lag\"]] has infinite elements") %>%
                  stop()
              }

              if (any(lag < 0)) {
                "arcs[[\"" %>%
                  str_c(name_gmbn, "\"]][[\"lag\"]] has negative elements") %>%
                  stop()
              }

              if (any(round(lag) != lag)) {
                "arcs[[\"" %>%
                  str_c(name_gmbn,
                        "\"]][[\"lag\"]] has non-integer elements") %>%
                  stop()
              }

              arcs <- arcs %>%
                filter(is.na(lag) | lag < time_gmbn)
            }
          }
        }

        gmbn %>%
          add_arcs(arcs) %>%
          return()
      }) %>%
      do.call("gmdbn", .)
  }

  gmgm %>%
    return()
}
