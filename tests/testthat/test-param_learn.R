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
gmm_3 <- list(alpha = 1, mu = matrix(4.833333, dimnames = list("A", NULL)),
              sigma = list(matrix(8.40619, dimnames = list("A", "A"))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = c(0.5382973, 0.4617027),
              mu = matrix(c(3.848964, 5.760557, 6.70297, 3.752287), 2,
                          dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(3.495108, 1.211245, 1.211245, 5.175567), 2,
                                  dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(3.162533, 4.930844, 4.930844, 8.208434), 2,
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
gmm_6 <- list(alpha = 1, mu = matrix(4.5, dimnames = list("A", NULL)),
              sigma = list(matrix(13.50333, dimnames = list("A", "A"))))
class(gmm_6) <- "gmm"
gmm_7 <- list(alpha = c(0.5, 0.5),
              mu = matrix(c(6, 9, 4, 0), 2, dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(0.005, 0, 0, 0.005), 2,
                                  dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(0.005, 0, 0, 0.005), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_7) <- "gmm"
gmm_8 <- list(alpha = 1, mu = matrix(5, dimnames = list("A", NULL)),
              sigma = list(matrix(3.602, dimnames = list("A", "A"))))
class(gmm_8) <- "gmm"
gmm_9 <- list(alpha = c(0.5, 0.5),
              mu = matrix(c(2.5, 4.5, 6, 8, 5.5, 1.5), 3,
                          dimnames = list(c("B", "A", "A.1"), NULL)),
              sigma = list(matrix(c(1.503333, - 1.5, 3, - 1.5, 1.503333, - 3, 3,
                                    - 3, 6.003333),
                                  3,
                                  dimnames = list(c("B", "A", "A.1"),
                                                  c("B", "A", "A.1"))),
                           matrix(c(0.67, 1.666667, 1, 1.666667, 4.17, 2.5, 1,
                                    2.5, 1.503333),
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

test_that("learn the parameters of a gmbn object", {
  expect_equal(param_learn(gmbn_1,
                           data.frame(A = c(0, 3, 6, 9, 3, 8),
                                      B = c(4, 7, 1, 6, 4, 9)),
                           regul = 0.01, max_iter_em = 5),
               list(gmgm = gmbn_2,
                    evol_loglik =
                      list(global = c(old = - 209.71807, new = - 27.45113),
                           local = matrix(c(- 105.01363, - 15.39994,
                                            - 104.70444, - 12.05119),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B"))))),
               tolerance = 0.01)
})

test_that("learn the parameters of a gmdbn object", {
  expect_equal(param_learn(gmdbn_1,
                           data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                      A = c(0, 3, 6, 9, 3, 8),
                                      B = c(4, 7, 1, 6, 4, 9)),
                           col_seq = "seq", regul = 0.01, max_iter_em = 5),
               list(gmgm = gmdbn_2,
                    evol_loglik =
                      list(global = c(old = - 208.136536, new = - 3.779152),
                           local =
                             list(b_1 = matrix(c(- 42.337877, - 5.940443,
                                                 - 41.48239, 3.46044),
                                               2,
                                               dimnames = list(c("old", "new"),
                                                               c("A", "B"))),
                                  b_2 = matrix(c(- 62.675754, - 8.737345,
                                                 - 61.640511, 7.438196),
                                               2,
                                               dimnames = list(c("old", "new"),
                                                               c("A", "B")))))),
               tolerance = 0.01)
})
