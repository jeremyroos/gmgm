gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 1), 1, dimnames = list("A", NULL)),
              sigma = list(matrix(1, dimnames = list("A", "A")),
                           matrix(2, dimnames = list("A", "A"))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 0, 1, 0), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 1), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 0, 0, 1), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = 1, mu = matrix(0, 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 1), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 4.5, 1, 4.5), 2,
                          dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 5.25), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 0, 0, 5.25), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_4) <- "gmm"
gmm_5 <- list(alpha = 1,
              mu = matrix(c(4.5, 4.5), dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(11.25, 0, 0, 5.25), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_5) <- "gmm"
gmm_6 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 4, 1, 4), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 1), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 0, 0, 1), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_6) <- "gmm"
gmm_7 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 0, 1, 0), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 2.225074e-308), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 0, 0, 2.225074e-308), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_7) <- "gmm"

test_that("add variables from a character vector", {
  expect_equal(add_var(gmm_1, "B"), gmm_2)
})

test_that("define the variables of a new gmm from a character vector", {
  expect_equal(add_var(NULL, c("A", "B")), gmm_3)
})

test_that("add variables from data", {
  expect_equal(add_var(gmm_1, data.frame(B = c(4, 7, 1, 6))), gmm_4)
  expect_equal(add_var(gmm_1,
                       matrix(c(4, 7, 1, 6), dimnames = list(NULL, "B"))),
               gmm_4)
})

test_that("define the variables of a new gmm from data", {
  expect_equal(add_var(NULL, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6))),
               gmm_5)
})

test_that("add variables from data with one row", {
  expect_equal(add_var(gmm_1, data.frame(B = 4)), gmm_6)
})

test_that("add variables from data with no row", {
  expect_equal(add_var(gmm_1, data.frame(B = numeric())), gmm_2)
  expect_equal(add_var(gmm_1, data.frame(B = logical())), gmm_2)
})

test_that("add variables from data with missing values", {
  expect_equal(add_var(gmm_1, data.frame(B = c(4, 7, 1, 6, NA))), gmm_4)
  expect_equal(add_var(gmm_1, data.frame(B = c(4, NA))), gmm_6)
  expect_equal(add_var(gmm_1, data.frame(B = NA)), gmm_2)
})

test_that("add variables from data with a non-positive definite covariance matrix", {
  expect_equal(add_var(gmm_1, data.frame(B = c(0, 0, 0, 0))), gmm_7,
               tolerance = 0.01)
})

test_that("add duplicated variables", {
  expect_equal(add_var(gmm_1, c("B", "B")), gmm_2)
})

test_that("add existent variables", {
  expect_equal(add_var(gmm_1, "A"), gmm_1)
  expect_equal(add_var(gmm_1, data.frame(A = c(0, 3, 6, 9))), gmm_1)
})

test_that("add no variable", {
  expect_equal(add_var(gmm_1, NULL), gmm_1)
  expect_equal(add_var(gmm_1, character()), gmm_1)
  expect_equal(add_var(gmm_1, data.frame(c(NA, NA, NA, NA))[, FALSE]), gmm_1)
  expect_equal(add_var(gmm_1, data.frame()), gmm_1)
})
