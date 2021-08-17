gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(2, 0, 3, 1), 2, dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(2, 1, 1, 1), 2,
                                  dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(5, 3, 3, 2), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 1), 1, dimnames = list("A", NULL)),
              sigma = list(matrix(1, dimnames = list("A", "A")),
                           matrix(2, dimnames = list("A", "A"))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(2, 0, 3, 1), 2,
                          dimnames = list(c("B", "A.1"), NULL)),
              sigma = list(matrix(c(2, 1, 1, 1), 2,
                                  dimnames = list(c("B", "A.1"),
                                                  c("B", "A.1"))),
                           matrix(c(5, 3, 3, 2), 2,
                                  dimnames = list(c("B", "A.1"),
                                                  c("B", "A.1")))))
class(gmm_3) <- "gmm"

gmbn_1 <- list(A = gmm_2, B = gmm_1)
class(gmbn_1) <- "gmbn"
gmbn_2 <- list(A = gmm_2, B = gmm_3)
class(gmbn_2) <- "gmbn"

gmdbn_1 <- list(b_1 = gmbn_1, b_2 = gmbn_1)
class(gmdbn_1) <- "gmdbn"
gmdbn_2 <- list(b_1 = gmbn_1, b_2 = gmbn_2)
class(gmdbn_2) <- "gmdbn"

test_that("compute the log-likelihood of a gmm object", {
  expect_equal(logLik(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6))),
               - 110.1373, tolerance = 0.01)
  expect_equal(logLik(gmm_1,
                      matrix(c(0, 3, 6, 9, 4, 7, 1, 6), 4,
                             dimnames = list(NULL, c("A", "B")))),
               - 110.1373, tolerance = 0.01)
})

test_that("compute the log-likelihood of a gmm object with missing values", {
  expect_equal(logLik(gmm_1,
                      data.frame(A = c(0, 3, NA, NA), B = c(4, 7, 1, NA))),
               as.numeric(NA))
})

test_that("compute the log-likelihood of a gmm object with extra columns", {
  expect_equal(logLik(gmm_1,
                      data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6),
                                 C = c(0, 0, 0, 0))),
               - 110.1373, tolerance = 0.01)
})

test_that("compute the log-likelihood of a gmm object with no row", {
  expect_equal(logLik(gmm_1, data.frame(A = numeric(), B = numeric())), - 0.05)
  expect_equal(logLik(gmm_1, data.frame(A = logical(), B = logical())), - 0.05)
})

test_that("compute the non-regularized log-likelihood of a gmm object", {
  expect_equal(logLik(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                      regul = NULL),
               - 110.0873, tolerance = 0.01)
})

test_that("compute the conditional log-likelihood of a gmm object", {
  expect_equal(logLik(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                      y = "B"),
               - 80.30329, tolerance = 0.01)
})

test_that("compute the log-likelihood of a gmbn object", {
  expect_equal(logLik(gmbn_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6))),
               list(global = - 110.0873,
                    local = c(A = - 29.78401, B = - 80.30329)),
               tolerance = 0.01)
})

test_that("compute the log-likelihood of a gmbn object with missing values", {
  expect_equal(logLik(gmbn_1,
                      data.frame(A = c(0, 3, NA, NA), B = c(4, 7, 1, NA))),
               list(global = as.numeric(NA),
                    local = c(A = as.numeric(NA), B = as.numeric(NA))))
})

test_that("compute the log-likelihood of a gmbn object with extra columns", {
  expect_equal(logLik(gmbn_1,
                      data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6),
                                 C = c(0, 0, 0, 0))),
               list(global = - 110.0873,
                    local = c(A = - 29.78401, B = - 80.30329)),
               tolerance = 0.01)
})

test_that("compute the log-likelihood of a gmbn object with no row", {
  expect_equal(logLik(gmbn_1, data.frame(A = numeric(), B = numeric())),
               list(global = 0, local = c(A = 0, B = 0)))
  expect_equal(logLik(gmbn_1, data.frame(A = logical(), B = logical())),
               list(global = 0, local = c(A = 0, B = 0)))
})

test_that("compute the log-likelihood of a temporal gmbn object with one observation sequence", {
  expect_equal(logLik(gmbn_2, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6))),
               list(global = - 69.80351,
                    local = c(A = - 28.55093, B = - 41.25258)),
               tolerance = 0.01)
  expect_equal(logLik(gmbn_2, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                      col_seq = character()),
               list(global = - 69.80351,
                    local = c(A = - 28.55093, B = - 41.25258)),
               tolerance = 0.01)
})

test_that("compute the log-likelihood of a temporal gmbn object with several observation sequences", {
  expect_equal(logLik(gmbn_2,
                      data.frame(seq = c(1, 1, 2, 2), A = c(0, 3, 6, 9),
                                 B = c(4, 7, 1, 6)),
                      col_seq = "seq"),
               list(global = - 49.27128,
                    local = c(A = - 20.5246, B = - 28.74667)),
               tolerance = 0.01)
})

test_that("compute the log-likelihood of a temporal gmbn object with duplicated observation sequence column names", {
  expect_equal(logLik(gmbn_2,
                      data.frame(seq = c(1, 1, 2, 2), A = c(0, 3, 6, 9),
                                 B = c(4, 7, 1, 6)),
                      col_seq = c("seq", "seq")),
               list(global = - 49.27128,
                    local = c(A = - 20.5246, B = - 28.74667)),
               tolerance = 0.01)
})

test_that("compute the log-likelihood of a gmdbn object with non-temporal gmbn elements", {
  expect_equal(logLik(gmdbn_1,
                      data.frame(seq = c(1, 1, 2, 2), A = c(0, 3, 6, 9),
                                 B = c(4, 7, 1, 6)),
                      col_seq = "seq"),
               list(global = - 110.0873,
                    local = list(b_1 = c(A = - 9.259408, B = - 40.732401),
                                 b_2 = c(A = - 20.5246, B = - 39.57089))),
               tolerance = 0.01)
})

test_that("compute the log-likelihood of a gmdbn object with temporal gmbn elements", {
  expect_equal(logLik(gmdbn_2,
                      data.frame(seq = c(1, 1, 2, 2), A = c(0, 3, 6, 9),
                                 B = c(4, 7, 1, 6)),
                      col_seq = "seq"),
               list(global = - 99.26309,
                    local = list(b_1 = c(A = - 9.259408, B = - 40.732401),
                                 b_2 = c(A = - 20.5246, B = - 28.74667))),
               tolerance = 0.01)
})
