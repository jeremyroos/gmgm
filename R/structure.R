#' Provide the graphical structure of a Gaussian mixture graphical model
#'
#' This function provides the graphical structure of a Gaussian mixture
#' graphical model.
#'
#' @param gmgm An object of class \code{gmbn} or \code{gmdbn}.
#'
#' @return A list with elements:
#' \item{nodes}{A character vector containing the nodes.}
#' \item{arcs}{For a \code{gmbn} object, a data frame (tibble) containing the
#' arcs. For a \code{gmdbn} object, a list of data frames (tibbles) containing
#' the arcs of each \code{gmbn} element.}
#'
#' @examples
#' data(gmbn_body)
#' struct_1 <- structure(gmbn_body)
#'
#' data(gmdbn_air)
#' struct_2 <- structure(gmdbn_air)
#'
#' @export

structure <- function(gmgm) {
  if (inherits(gmgm, "gmbn")) {
    nodes <- gmgm %>%
      names()
    arcs <- gmgm %>%
      imap(function(gmm, node) {
        arcs <- gmm$mu %>%
          rownames() %>%
          .[- 1] %>%
          str_split_fixed("\\.(?=[1-9][0-9]*$)", 2)
        colnames(arcs) <- c("from", "lag")
        arcs %>%
          as_tibble() %>%
          mutate(to = node,
                 lag = replace_na(as.integer(lag), as.integer(0))) %>%
          select(from, to, lag) %>%
          return()
      }) %>%
      bind_rows()
    struct <- list(nodes = nodes, arcs = arcs)
  } else {
    if (!inherits(gmgm, "gmdbn")) {
      "gmgm is not of class \"gmbn\" or \"gmdbn\"" %>%
        stop()
    }

    struct <- gmgm %>%
      map(structure) %>%
      transpose()
    struct$nodes <- struct$nodes$b_1
  }

  struct %>%
    return()
}
