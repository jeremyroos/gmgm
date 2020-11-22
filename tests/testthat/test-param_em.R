gmm_1 <- list(alpha = 1, mu = matrix(0, dimnames = list("A", NULL)),
              sigma = list(matrix(1, dimnames = list("A", "A"))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(2, 0, 3, 1), 2, dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(2, 1, 1, 1), 2,
                                  dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(5, 3, 3, 2), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = 1, mu = matrix(2.79699, dimnames = list("A", NULL)),
              sigma = list(matrix(2.735939, dimnames = list("A", "A"))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = c(0.5000027, 0.4999973),
              mu = matrix(c(7.632571, 1.514438, 7.341044, 4.079556), 2,
                          dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(0.7050431, 0.6013697, 0.6013697, 0.9303182),
                                  2, dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(3.133886, 2.08542, 2.08542, 1.392639), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_4) <- "gmm"
gmm_5 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(2, 0, 4, 3, 1, 5), 3,
                          dimnames = list(c("B", "A", "A.1"), NULL)),
              sigma = list(matrix(c(2, 1, 2, 1, 1, 1, 2, 1, 3), 3,
                                  dimnames = list(c("B", "A", "A.1"),
                                                  c("B", "A", "A.1"))),
                           matrix(c(5, 3, 2, 3, 2, 1, 2, 1, 4), 3,
                                  dimnames = list(c("B", "A", "A.1"),
                                                  c("B", "A", "A.1")))))
class(gmm_5) <- "gmm"
gmm_6 <- list(alpha = 1, mu = matrix(0.6648996, dimnames = list("A", NULL)),
              sigma = list(matrix(0.298061, dimnames = list("A", "A"))))
class(gmm_6) <- "gmm"
gmm_7 <- list(alpha = c(0.5, 0.5),
              mu = matrix(c(6, 1.329799, 0.4110909, 0), 2,
                          dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(0.005, 0, 0, 0.005), 2,
                                  dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(0.005, 0, 0, 0.005), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_7) <- "gmm"
gmm_8 <- list(alpha = 1, mu = matrix(2.912906, dimnames = list("A", NULL)),
              sigma = list(matrix(3.382651, dimnames = list("A", "A"))))
class(gmm_8) <- "gmm"
gmm_9 <- list(alpha = c(0.25, 0.75),
              mu = matrix(c(9, 0.2469726, 3, 7.03098, 3.801551, 1.244818), 3,
                          dimnames = list(c("B", "A", "A.1"), NULL)),
              sigma = list(matrix(c(0.005, 0, 0, 0, 0.005, 0, 0, 0, 0.005), 3,
                                  dimnames = list(c("B", "A", "A.1"),
                                                  c("B", "A", "A.1"))),
                           matrix(c(3.0364242, 1.8635943, 0.6763447, 1.863594,
                                    1.859246, 1.055151, 0.6763447, 1.0551514,
                                    0.7280029),
                                  3,
                                  dimnames = list(c("B", "A", "A.1"),
                                                  c("B", "A", "A.1")))))
class(gmm_9) <- "gmm"

gmbn_1 <- list(A = gmm_1, B = gmm_2)
class(gmbn_1) <- "gmbn"
gmbn_2 <- list(A = gmm_3, B = gmm_4)
class(gmbn_2) <- "gmbn"
gmbn_3 <- list(A = gmm_1, B = gmm_5)
class(gmbn_3) <- "gmbn"
gmbn_4 <- list(A = gmm_6, B = gmm_7)
class(gmbn_4) <- "gmbn"
gmbn_5 <- list(A = gmm_8, B = gmm_9)
class(gmbn_5) <- "gmbn"

gmdbn_1 <- list(b_1 = gmbn_1, b_2 = gmbn_3)
class(gmdbn_1) <- "gmdbn"
gmdbn_2 <- list(b_1 = gmbn_4, b_2 = gmbn_5)
class(gmdbn_2) <- "gmdbn"

test_that("perform the parametric EM algorithm for a gmbn object", {
  set.seed(0)
  expect_equal(param_em(gmbn_1,
                        data.frame(A = c(0, NA, 6, NA, 3, NA),
                                   B = c(NA, 7, NA, 6, NA, 9)),
                        n_part = 2, max_iter_pem = 2, regul = 0.01,
                        max_iter_em = 5),
               list(gmgm = gmbn_2,
                    data = tibble(A = c(0, 1.9, 6, 3.24, 3, 2.64),
                                  B = c(6.9, 7, 10.2, 6, 5.79, 9)),
                    seq_loglik = matrix(c(- 80.68157, - 17.8767, - 19.89646,
                                          - 13.02486),
                                        2,
                                        dimnames = list(c("E", "M"), NULL))),
               tolerance = 0.01)
})

test_that("perform no iteration of the parametric EM algorithm for a gmbn object", {
  expect_equal(param_em(gmbn_1,
                        data.frame(A = c(0, NA, 6, NA, 3, NA),
                                   B = c(NA, 7, NA, 6, NA, 9)),
                        max_iter_pem = 0),
               list(gmgm = gmbn_1, data = NULL,
                    seq_loglik = matrix(numeric(), 2,
                                        dimnames = list(c("E", "M"), NULL))),
               tolerance = 0.01)
})

test_that("perform the parametric EM algorithm for a gmbn object with verbose", {
  set.seed(0)
  expect_equal(param_em(gmbn_1,
                        data.frame(A = c(0, NA, 6, NA, 3, NA),
                                   B = c(NA, 7, NA, 6, NA, 9)),
                        n_part = 2, max_iter_pem = 2, verbose = TRUE,
                        regul = 0.01, max_iter_em = 5),
               list(gmgm = gmbn_2,
                    data = tibble(A = c(0, 1.9, 6, 3.24, 3, 2.64),
                                  B = c(6.9, 7, 10.2, 6, 5.79, 9)),
                    seq_loglik = matrix(c(- 80.68157, - 17.8767, - 19.89646,
                                          - 13.02486),
                                        2, dimnames = list(c("E", "M"), NULL))),
               tolerance = 0.01)
})

test_that("perform the parametric EM algorithm for a gmdbn object", {
  set.seed(0)
  expect_equal(param_em(gmdbn_1,
                        data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                   A = c(0, NA, 6, NA, 3, NA),
                                   B = c(NA, 7, NA, 6, NA, 9)),
                        col_seq = "seq", n_part = 2, max_iter_pem = 2,
                        regul = 0.01, max_iter_em = 5),
               list(gmgm = gmdbn_2,
                    data = tibble(seq = c(1, 1, 1, 2, 2, 2),
                                  A = c(0, 2.4, 6, 1.33, 3, 0.247),
                                  B = c(0.411, 7, 9.51, 6, 4.58, 9)),
                    seq_loglik = matrix(c(- 105.619123, - 3.623037, - 225.92326,
                                          - 10.24564),
                                        2,
                                        dimnames = list(c("E", "M"), NULL))),
               tolerance = 0.01)
})
