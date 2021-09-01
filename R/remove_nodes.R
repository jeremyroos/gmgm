#' Remove nodes from a Gaussian mixture graphical model
#'
#' This function removes nodes from a Gaussian mixture graphical model. If this
#' model is a dynamic Bayesian network, the nodes are removed from each of its
#' transition models.
#'
#' @param gmgm An object of class \code{gmbn} or \code{gmdbn}.
#' @param nodes A character vector containing the removed nodes.
#'
#' @return The \code{gmbn} or \code{gmdbn} object after removing the nodes.
#'
#' @seealso \code{\link{add_arcs}}, \code{\link{add_nodes}},
#' \code{\link{relevant}}, \code{\link{remove_arcs}}, \code{\link{rename_nodes}}
#'
#' @examples
#' data(gmbn_body)
#' gmbn_1 <- remove_nodes(gmbn_body, c("FAT", "GLYCO"))
#'
#' data(gmdbn_air)
#' gmdbn_1 <- remove_nodes(gmdbn_air, "TEMP")
#'
#' @export

remove_nodes <- function(gmgm, nodes) {
  if (inherits(gmgm, "gmbn")) {
    if (!is.null(nodes) & !is.vector(nodes, "character")) {
      "nodes is not a character vector" %>%
        stop()
    }

    struct <- gmgm %>%
      structure()
    nodes_gmgm <- struct$nodes %>%
      setdiff(nodes)
    gmgm <- gmgm[nodes_gmgm]
    arcs <- struct$arcs %>%
      filter(from %in% nodes, to %in% nodes_gmgm)

    if (nrow(arcs) > 0) {
      to <- arcs$to %>%
        unique()
      gmgm <- arcs %>%
        mutate(from_lag = if_else(lag == 0, from, str_c(from, ".", lag))) %>%
        group_by(to) %>%
        group_map(function(arcs, to) {
          gmgm[[to$to]] %>%
            remove_var(arcs$from_lag) %>%
            return()
        }) %>%
        set_names(to) %>%
        c(gmgm[setdiff(nodes_gmgm, to)])
    }

    gmgm <- gmgm %>%
      do.call("gmbn", .)
  } else {
    if (!inherits(gmgm, "gmdbn")) {
      "gmgm is not of class \"gmbn\" or \"gmdbn\"" %>%
        stop()
    }

    gmgm <- gmgm %>%
      map(remove_nodes, nodes) %>%
      do.call("gmdbn", .)
  }

  gmgm %>%
    return()
}
