#' Display the mixture components of a Gaussian mixture model
#'
#' This function displays the mixture components of a Gaussian mixture model.
#' For each pair of variables, the covariance matrices are represented by
#' confidence ellipses.
#'
#' @param gmm An object of class \code{gmm}.
#' @param data A data frame or numeric matrix containing the data displayed with
#' the mixture components. Its columns must explicitly be named after the
#' variables of \code{gmm}. If \code{NULL} (the default), no data is displayed.
#' @param x A character vector containing the variables displayed on the x-axis
#' (by default all the variables of \code{gmm}).
#' @param y A character vector containing the variables displayed on the y-axis
#' (by default all the variables of \code{gmm}).
#' @param level A numeric value in [0, 1[ corresponding to the confidence level
#' of the ellipses.
#'
#' @return A \code{ggplot} object displaying the mixture components (and the
#' data if required).
#'
#' @examples
#' set.seed(0)
#' data(gmm_body)
#' ellipses(gmm_body, sampling(gmm_body, n = 500))
#'
#' @export

ellipses <- function(gmm, data = NULL, y = rownames(gmm$mu),
                     x = rownames(gmm$mu), level = 0.95) {
  if (!inherits(gmm, "gmm")) {
    "gmm is not of class \"gmm\"" %>%
      stop()
  }

  if (!is.vector(y, "character")) {
    "y is not a character vector" %>%
      stop()
  }

  y <- y %>%
    unique()

  if (length(y) == 0) {
    "y is empty" %>%
      stop()
  }

  mu <- gmm$mu
  var_gmm <- mu %>%
    rownames()

  if (any(!(y %in% var_gmm))) {
    "elements of y are not variables of gmm" %>%
      stop()
  }

  if (!is.vector(x, "character")) {
    "x is not a character vector" %>%
      stop()
  }

  x <- x %>%
    unique()

  if (length(x) == 0) {
    "x is empty" %>%
      stop()
  }

  if (any(!(x %in% var_gmm))) {
    "elements of x are not variables of gmm" %>%
      stop()
  }

  if (!is.vector(level, "numeric")) {
    "level is not a numeric value" %>%
      stop()
  }

  if (length(level) != 1) {
    "level is not of length 1" %>%
      stop()
  }

  if (is.na(level)) {
    "level is NA" %>%
      stop()
  }

  if (level < 0 | level >= 1) {
    "level is not in [0, 1[" %>%
      stop()
  }

  sigma <- gmm$sigma
  angle <- 0 %>%
    seq(2 * pi, length.out = 121)
  circle <- angle %>%
    cos() %>%
    rbind(sin(angle), deparse.level = 0) * sqrt(- 2 * log(1 - level))
  names_xy <- c("x", "y")
  ellip <- gmm$alpha %>%
    seq_along() %>%
    map(function(comp) {
      mu <- mu[, comp]
      sigma <- sigma[[comp]]
      x %>%
        map(function(x) {
          y %>%
            map(function(y) {
              var <- x %>%
                c(y)
              eigen_sigma <- sigma[var, var] %>%
                eigen(TRUE)
              ellip <- t(eigen_sigma$vectors %*%
                           (circle * sqrt(eigen_sigma$values)) +
                           mu[var])
              colnames(ellip) <- names_xy
              ellip %>%
                as_tibble() %>%
                return()
            }) %>%
            set_names(y) %>%
            bind_rows(.id = "var_y") %>%
            return()
        }) %>%
        set_names(x) %>%
        bind_rows(.id = "var_x") %>%
        return()
    }) %>%
    bind_rows(.id = "comp") %>%
    mutate(var_x = factor(var_x, var_gmm), var_y = factor(var_y, var_gmm))
  labels <- ellip %>%
    group_by(comp, var_x, var_y) %>%
    filter(y == max(y)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(var_x = factor(var_x, var_gmm), var_y = factor(var_y, var_gmm))
  plot <- ggplot(ellip, aes(x, y, group = comp, color = comp)) +
    theme(panel.background = element_rect(fill = "white", color = "#cccccc"),
          panel.grid = element_blank(), strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(color = "black"),
          axis.title = element_blank(),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "#cccccc")) +
    facet_grid(var_y ~ var_x, scales = "free", switch = "y") +
    scale_y_continuous(position = "right")

  if (!is.null(data)) {
    is_matrix <- data %>%
      is.matrix()

    if (!is.data.frame(data) & !is_matrix) {
      "data is not a data frame or numeric matrix" %>%
        stop()
    }

    col_data <- data %>%
      colnames()

    if (any(duplicated(col_data))) {
      "data has duplicated column names" %>%
        stop()
    }

    if (any(!(y %in% col_data))) {
      "elements of y are not column names of data" %>%
        stop()
    }

    if (any(!(x %in% col_data))) {
      "elements of x are not column names of data" %>%
        stop()
    }

    if (is_matrix) {
      data <- data %>%
        as_tibble()
    }

    if (nrow(data) > 0) {
      if (any(!map_lgl(data[y], is.numeric))) {
        "columns of data[y] are not numeric" %>%
          stop()
      }

      if (any(!map_lgl(data[x], is.numeric))) {
        "columns of data[x] are not numeric" %>%
          stop()
      }
    }

    data <- x %>%
      map(function(x) {
        y %>%
          map(function(y) {
            tibble(x = data[[x]], y = data[[y]]) %>%
              return()
          }) %>%
          set_names(y) %>%
          bind_rows(.id = "var_y") %>%
          return()
      }) %>%
      set_names(x) %>%
      bind_rows(.id = "var_x") %>%
      mutate(var_x = factor(var_x, var_gmm), var_y = factor(var_y, var_gmm))
    plot <- plot +
      geom_point(aes(x, y), data, alpha = 0.2, inherit.aes = FALSE)
  }

  plot +
    geom_path(show.legend = FALSE) +
    geom_text(aes(label = comp), labels, vjust = 1.2, show.legend = FALSE) %>%
    return()
}
