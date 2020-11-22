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

test_that("compute the AIC of a gmm object", {
  expect_equal(AIC(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6))),
               - 121.0873, tolerance = 0.01)
  expect_equal(AIC(gmm_1,
                   matrix(c(0, 3, 6, 9, 4, 7, 1, 6), 4,
                          dimnames = list(NULL, c("A", "B")))),
               - 121.0873, tolerance = 0.01)
})

test_that("compute the AIC of a gmm object with missing values", {
  expect_equal(AIC(gmm_1, data.frame(A = c(0, 3, NA, NA), B = c(4, 7, 1, NA))),
               as.numeric(NA))
})

test_that("compute the AIC of a gmm object with extra columns", {
  expect_equal(AIC(gmm_1,
                   data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1 , 6),
                              C = c(0, 0, 0, 0))),
               - 121.0873, tolerance = 0.01)
})

test_that("compute the AIC of a gmm object with no row", {
  expect_equal(AIC(gmm_1, data.frame(A = numeric(), B = numeric())), - 11)
  expect_equal(AIC(gmm_1, data.frame(A = logical(), B = logical())), - 11)
})

test_that("compute the regularized AIC of a gmm object", {
  expect_equal(AIC(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                   regul = 0.01),
               - 121.1373, tolerance = 0.01)
})

test_that("compute the conditional AIC of a gmm object", {
  expect_equal(AIC(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                   y = "B"),
               - 91.30329, tolerance = 0.01)
})

test_that("compute the AIC of a gmbn object", {
  expect_equal(AIC(gmbn_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6))),
               list(global = - 126.0873,
                    local = c(A = - 34.78401, B = - 91.30329)),
               tolerance = 0.01)
})

test_that("compute the AIC of a gmbn object with missing values", {
  expect_equal(AIC(gmbn_1, data.frame(A = c(0, 3, NA, NA), B = c(4, 7, 1, NA))),
               list(global = as.numeric(NA),
                    local = c(A = as.numeric(NA), B = as.numeric(NA))))
})

test_that("compute the AIC of a gmbn object with extra columns", {
  expect_equal(AIC(gmbn_1,
                   data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6),
                              C = c(0, 0, 0, 0))),
               list(global = - 126.0873,
                    local = c(A = - 34.78401, B = - 91.30329)),
               tolerance = 0.01)
})

test_that("compute the AIC of a gmbn object with no row", {
  expect_equal(AIC(gmbn_1, data.frame(A = numeric(), B = numeric())),
               list(global = - 16, local = c(A = - 5, B = - 11)))
  expect_equal(AIC(gmbn_1, data.frame(A = logical(), B = logical())),
               list(global = - 16, local = c(A = - 5, B = - 11)))
})

test_that("compute the AIC of a temporal gmbn object with one observation sequence", {
  expect_equal(AIC(gmbn_2, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6))),
               list(global = - 85.80351,
                    local = c(A = - 33.55093, B = - 52.25258)),
               tolerance = 0.01)
  expect_equal(AIC(gmbn_2, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                   col_seq = character()),
               list(global = - 85.80351,
                    local = c(A = - 33.55093, B = - 52.25258)),
               tolerance = 0.01)
})

test_that("compute the AIC of a temporal gmbn object with several observation sequences", {
  expect_equal(AIC(gmbn_2,
                   data.frame(seq = c(1, 1, 2, 2), A = c(0, 3, 6, 9),
                              B = c(4, 7, 1, 6)),
                   col_seq = "seq"),
               list(global = - 65.27128,
                    local = c(A = -25.5246, B = -39.74667)),
               tolerance = 0.01)
})

test_that("compute the AIC of a temporal gmbn object with duplicated observation sequence column names", {
  expect_equal(AIC(gmbn_2,
                   data.frame(seq = c(1, 1, 2, 2), A = c(0, 3, 6, 9),
                              B = c(4, 7, 1, 6)),
                   col_seq = c("seq", "seq")),
               list(global = - 65.27128,
                    local = c(A = -25.5246, B = -39.74667)),
               tolerance = 0.01)
})

test_that("compute the AIC of a gmdbn object with non-temporal gmbn elements", {
  expect_equal(AIC(gmdbn_1,
                   data.frame(seq = c(1, 1, 2, 2), A = c(0, 3, 6, 9),
                              B = c(4, 7, 1, 6)),
                   col_seq = "seq"),
               list(global = - 142.0873,
                    local = list(b_1 = c(A = - 14.25941, B = - 51.7324),
                                 b_2 = c(A = - 25.5246, B = - 50.57089))),
               tolerance = 0.01)
})

test_that("compute the AIC of a gmdbn object with temporal gmbn elements", {
  expect_equal(AIC(gmdbn_2,
                   data.frame(seq = c(1, 1, 2, 2), A = c(0, 3, 6, 9),
                              B = c(4, 7, 1, 6)),
                   col_seq = "seq"),
               list(global = - 131.2631,
                    local = list(b_1 = c(A = - 14.25941, B = - 51.7324),
                                 b_2 = c(A = - 25.5246, B = - 39.74667))),
               tolerance = 0.01)
})
