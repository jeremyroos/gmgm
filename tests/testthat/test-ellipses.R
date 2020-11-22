gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_1) <- "gmm"

test_that("display mixture components", {
  expect_is(ellipses(gmm_1), "ggplot")
})

test_that("display mixture components and data", {
  expect_is(ellipses(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6))),
            "ggplot")
  expect_is(ellipses(gmm_1,
                     matrix(c(0, 3, 6, 9, 4, 7, 1, 6), 4,
                            dimnames = list(NULL, c("A", "B")))),
            "ggplot")
})

test_that("display mixture components and data with missing values", {
  expect_is(ellipses(gmm_1,
                     data.frame(A = c(0, 3, NA, NA), B = c(4, 7, 1, NA))),
            "ggplot")
})

test_that("display mixture components and data with extra columns", {
  expect_is(ellipses(gmm_1,
                     data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6),
                                C = c(0, 0, 0, 0))),
            "ggplot")
})

test_that("display mixture components and data with no row", {
  expect_is(ellipses(gmm_1, data.frame(A = numeric(), B = numeric())),
            "ggplot")
  expect_is(ellipses(gmm_1, data.frame(A = logical(), B = logical())),
            "ggplot")
})

test_that("display mixture components and data for subsets of variables", {
  expect_is(ellipses(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                     y = "B", x = "A"),
            "ggplot")
})

test_that("display mixture components and data for subsets of duplicated variables", {
  expect_is(ellipses(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                     y = c("B", "B"), x = c("A", "A")),
            "ggplot")
})
