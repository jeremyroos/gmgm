#' Initialize particles to perform inference in a Gaussian mixture graphical
#' model
#'
#' This function initializes particles to perform (approximate) inference in a
#' Gaussian mixture graphical model. Particles consist in weighted sample
#' sequences propagated forward in time by sampling the model and aggregated to
#' obtain the inferred values (Koller and Friedman, 2009).
#'
#' @param seq A data frame containing the observation sequences for which
#' particles are initialized. If \code{NULL} (the default), the initialization
#' is performed for a single sequence.
#' @param col_weight A character string corresponding to the column name of the
#' resulting data frame that describes the particle weight.
#' @param n_part A positive integer corresponding to the number of particles
#' initialized for each observation sequence.
#'
#' @return A data frame (tibble) containing the initial particles.
#'
#' @references
#' Koller, D. and Friedman, N. (2009). \emph{Probabilistic Graphical Models:
#' Principles and Techniques}. The MIT Press.
#'
#' @seealso \code{\link{aggregation}}, \code{\link{propagation}}
#'
#' @examples
#' data(data_air)
#' part <- particles(data.frame(DATE = unique(data_air$DATE)))
#'
#' @export

particles <- function(seq = NULL, col_weight = "weight", n_part = 1000) {
  if (is.null(seq)) {
    seq <- tibble(.rows = 1)
    col_seq <- NULL
  } else {
    if (!is.data.frame(seq)) {
      "seq is not a data frame" %>%
        stop()
    }

    seq <- seq %>%
      as_tibble()

    if (nrow(seq) == 0) {
      "seq has no row" %>%
        stop()
    }

    col_seq <- seq %>%
      colnames()

    if (any(duplicated(col_seq))) {
      "seq has duplicated column names" %>%
        stop()
    }

    if (any(!str_detect(col_seq,
                        "^(\\.([A-Za-z_\\.]|$)|[A-Za-z])[A-Za-z0-9_\\.]*$"))) {
      "seq has invalid column names" %>%
        stop()
    }

    if (any(!(map_chr(seq, mode) %in% c("numeric", "character", "logical")))) {
      "columns of seq have invalid types" %>%
        stop()
    }
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

  if (col_weight %in% col_seq) {
    "col_weight is a column name of seq" %>%
      stop()
  }

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

  if (length(col_seq) > 0) {
    seq <- seq %>%
      distinct() %>%
      arrange(across(everything()))
  }

  seq %>%
    uncount(n_part) %>%
    mutate(!!col_weight := 1 / n_part) %>%
    return()
}
