#' Rename nodes of a Gaussian mixture graphical model
#'
#' This function renames nodes of a Gaussian mixture graphical model. If this
#' model is a dynamic Bayesian network, the nodes are renamed for each of its
#' transition models.
#'
#' @param gmgm An object of class \code{gmbn} or \code{gmdbn}.
#' @param nodes A character vector containing the renamed nodes.
#' @param names A character vector containing the respective new names of the
#' nodes.
#'
#' @return The \code{gmbn} or \code{gmdbn} object after renaming the nodes.
#'
#' @seealso \code{\link{add_arcs}}, \code{\link{add_nodes}},
#' \code{\link{remove_arcs}}, \code{\link{remove_nodes}}
#'
#' @examples
#' data(gmbn_body)
#' gmbn_1 <- rename_nodes(gmbn_body, c("FAT", "GLYCO"),
#'                        c("BODY_FAT", "GLYCOHEMOGLOBIN"))
#'
#' data(gmdbn_air)
#' gmdbn_1 <- rename_nodes(gmdbn_air, "TEMP", "TEMPERATURE")
#'
#' @export

rename_nodes <- function(gmgm, nodes, names) {
  if (inherits(gmgm, "gmbn")) {
    struct <- gmgm %>%
      structure()
    nodes_gmgm <- struct$nodes

    if (!is.null(nodes)) {
      if (!is.vector(nodes, "character")) {
        "nodes is not a character vector" %>%
          stop()
      }

      if (any(duplicated(nodes))) {
        "nodes has duplicated elements" %>%
          stop()
      }

      if (any(!(nodes %in% nodes_gmgm))) {
        "elements of nodes are not nodes of gmgm" %>%
          stop()
      }
    }

    if (!is.null(names)) {
      if (!is.vector(names, "character")) {
        "names is not a character vector" %>%
          stop()
      }

      if (any(duplicated(names))) {
        "names has duplicated elements" %>%
          stop()
      }
    }

    n_nodes <- nodes %>%
      length()

    if (n_nodes != length(names)) {
      "nodes and names have different lengths" %>%
        stop()
    }

    if (n_nodes > 0) {
      arcs <- struct$arcs %>%
        filter(from %in% nodes) %>%
        bind_rows(tibble(from = nodes, to = nodes, lag = 0))
      to <- arcs$to %>%
        unique() %>%
        sort()
      to_new <- to
      to_new[match(nodes, to_new)] <- names
      gmgm <- arcs %>%
        mutate(from_lag = if_else(lag == 0, from, str_c(from, ".", lag)),
               from_new = names[match(from, nodes)],
               from_lag_new = if_else(lag == 0, from_new,
                                      str_c(from_new, ".", lag))) %>%
        group_by(to) %>%
        group_map(function(arcs, to) {
          gmgm[[to$to]] %>%
            rename_var(arcs$from_lag, arcs$from_lag_new) %>%
            return()
        }) %>%
        set_names(to_new) %>%
        c(gmgm[setdiff(nodes_gmgm, to)]) %>%
        do.call("gmbn", .)
    }
  } else {
    if (!inherits(gmgm, "gmdbn")) {
      "gmgm is not of class \"gmbn\" or \"gmdbn\"" %>%
        stop()
    }

    gmgm <- gmgm %>%
      map(rename_nodes, nodes, names) %>%
      do.call("gmdbn", .)
  }

  gmgm %>%
    return()
}
